{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Network.AWS.EC2.Metadata
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.EC2.Metadata
    ( Metadata(..)
    , metadata
    , metadataByKey
    ) where

import           Control.Applicative
import           Control.Exception
import           Control.Monad.IO.Class
import           Data.ByteString        (ByteString)
import qualified Data.ByteString.Char8  as BS
import           Data.Maybe
import           Data.Monoid
import           Network.AWS.Types
import           Network.Http.Client
import qualified System.IO.Streams      as Streams

data Metadata
    = AmiId
    | AmiLaunchIndex
    | AmiManifestPath
    | Hostname
    | InstanceAction
    | InstanceId
    | InstanceType
    | KernelId
    | LocalHostname
    | LocalIPV4
    | Mac
    | Profile
    | PublicHostname
    | PublicIPV4
    | ReservationId
    | SecurityCredentials ByteString
    | AvailabilityZone

instance Show Metadata where
    show = BS.unpack . toBS

instance IsByteString Metadata where
    toBS meta = case meta of
        AmiId                 -> "ami-id"
        AmiLaunchIndex        -> "ami-launch-index"
        AmiManifestPath       -> "ami-manifest-path"
        Hostname              -> "hostname"
        InstanceAction        -> "instance-action"
        InstanceId            -> "instance-id"
        InstanceType          -> "instance-type"
        KernelId              -> "kernel-id"
        LocalHostname         -> "local-hostname"
        LocalIPV4             -> "local-ipv4"
        Mac                   -> "mac"
        Profile               -> "profile"
        PublicHostname        -> "public-hostname"
        PublicIPV4            -> "public-ipv4"
        ReservationId         -> "reservation-id"
        SecurityCredentials r -> "iam/security-credentials/" <> r
        AvailabilityZone      -> "placement/availability-zone"

-- {
--   "Code" : "Success",
--   "LastUpdated" : "2013-08-05T06:32:22Z",
--   "Type" : "AWS-HMAC",
--   "AccessKeyId" : "ASIAJL2P226VZ5IWGPCA",
--   "SecretAccessKey" : "/0cu7VvUbOgYP8zSKLj3imkovMGbKvMMZOFItYXd",
--   "Token" : "AQoDYXdzEBAaoAL80jr5LjYBukdIym/QOGHvFH+ER7bJC9h0ScihxfLB06Kw3jwO7nxlPNXxLJT1vY3i2klLyK4aIEfQ1hnHUUzrSpFm1I75bJBfLaV7cWuXQCdJl4Uyu30lngYca9M33Irk9GgkTZiC6ovKX4Ma/S9yDYI+b4W8PTrmvvX0mDbYjlm90RpzRqND82wOr/AyUeci6Z9TN5dHaS3vtzgO4tv92YWYJJ6sxhT/7cwcYnavFb69X24uF1EW9cte0gJvHlDFuXw/v7YI1CGJsvuLzMmz7qWdPs23I+KvLOcfkYKYmxz1uyWuIdXDEsOS2XWSc9OGe1fzDCFPe/U5Me1wozcw2+k12YGK87btxOwHPUONfpVczMiWytswrzgi3VwwSxAgsI79jwU=",
--   "Expiration" : "2013-08-05T12:53:28Z"
-- }

metadata :: MonadIO m => Metadata -> m ByteString
metadata = metadataByKey . toBS

metadataByKey :: MonadIO m => ByteString -> m ByteString
metadataByKey key = request $ "http://169.254.169.254/latest/meta-data" <> key

--
-- Internal
--

request :: MonadIO m => ByteString -> m ByteString
request url = liftIO $
    bracket (establishConnection url) closeConnection $ \conn -> do
        rq <- buildRequest $ http GET url
        sendRequest conn rq emptyBody
        receiveResponse conn $ \_ inp -> fromMaybe "" <$> Streams.read inp
