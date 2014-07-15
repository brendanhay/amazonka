{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.PutBucketVersioning
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Sets the versioning state of an existing bucket. To set the versioning
-- state, you must be the bucket owner.
module Network.AWS.S3.V2006_03_01.PutBucketVersioning where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Maybe
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error)
import           Network.AWS.Request.RestS3
import           Network.AWS.S3.V2006_03_01.Types
import           Network.HTTP.Client  (Response)
import           Prelude              hiding (head)

-- | Default PutBucketVersioning request.
putBucketVersioning :: VersioningConfiguration -- ^ '_pbvrVersioningConfiguration'
                    -> BucketName -- ^ '_pbvrBucket'
                    -> PutBucketVersioning
putBucketVersioning p1 p2 = PutBucketVersioning
    { _pbvrVersioningConfiguration = p1
    , _pbvrBucket = p2
    , _pbvrContentMD5 = Nothing
    , _pbvrMFA = Nothing
    }

data PutBucketVersioning = PutBucketVersioning
    { _pbvrVersioningConfiguration :: VersioningConfiguration
    , _pbvrBucket :: BucketName
    , _pbvrContentMD5 :: Maybe Text
    , _pbvrMFA :: Maybe Text
      -- ^ The concatenation of the authentication device's serial number, a
      -- space, and the value that is displayed on your authentication
      -- device.
    } deriving (Show, Generic)

instance ToPath PutBucketVersioning where
    toPath PutBucketVersioning{..} = mconcat
        [ "/"
        , toBS _pbvrBucket
        ]

instance ToQuery PutBucketVersioning

instance ToHeaders PutBucketVersioning where
    toHeaders PutBucketVersioning{..} = concat
        [ "Content-MD5" =: _pbvrContentMD5
        , "x-amz-mfa" =: _pbvrMFA
        ]

instance ToBody PutBucketVersioning where
    toBody = undefined -- toBody . _pbvrVersioningConfiguration

instance AWSRequest PutBucketVersioning where
    type Sv PutBucketVersioning = S3

    request  = put
    response = headerResponse . const $ Right PutBucketVersioningResponse

data instance Rs PutBucketVersioning = PutBucketVersioningResponse
    deriving (Eq, Show, Generic)
