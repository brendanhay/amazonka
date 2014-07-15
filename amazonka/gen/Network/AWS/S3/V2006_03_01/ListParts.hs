{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.ListParts
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Lists the parts that have been uploaded for a specific multipart upload.
module Network.AWS.S3.V2006_03_01.ListParts where

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

-- | Default ListParts request.
listParts :: BucketName -- ^ '_lprBucket'
          -> Text -- ^ '_lprUploadId'
          -> ObjectKey -- ^ '_lprKey'
          -> ListParts
listParts p1 p2 p3 = ListParts
    { _lprBucket = p1
    , _lprUploadId = p2
    , _lprKey = p3
    , _lprMaxParts = Nothing
    , _lprPartNumberMarker = Nothing
    }

data ListParts = ListParts
    { _lprBucket :: BucketName
    , _lprUploadId :: Text
      -- ^ Upload ID identifying the multipart upload whose parts are being
      -- listed.
    , _lprKey :: ObjectKey
    , _lprMaxParts :: Maybe Integer
      -- ^ Sets the maximum number of parts to return.
    , _lprPartNumberMarker :: Maybe Integer
      -- ^ Specifies the part after which listing should begin. Only parts
      -- with higher part numbers will be listed.
    } deriving (Show, Generic)

instance ToPath ListParts where
    toPath ListParts{..} = mconcat
        [ "/"
        , toBS _lprBucket
        , "/"
        , toBS _lprKey
        ]

instance ToQuery ListParts

instance ToHeaders ListParts

instance ToBody ListParts

instance AWSRequest ListParts where
    type Sv ListParts = S3

    request  = get
    response = xmlResponse

instance AWSPager ListParts where
    next rq rs
        | not (_lpoIsTruncated rs) = Nothing
        | otherwise = Just $ rq
            { _lprPartNumberMarker = _lpoNextPartNumberMarker rs
            }

data instance Rs ListParts = ListPartsResponse
    { _lpoIsTruncated :: Bool
      -- ^ Indicates whether the returned list of parts is truncated.
    , _lpoBucket :: Maybe BucketName
      -- ^ Name of the bucket to which the multipart upload was initiated.
    , _lpoInitiator :: Maybe Initiator
      -- ^ Identifies who initiated the multipart upload.
    , _lpoMaxParts :: Maybe Integer
      -- ^ Maximum number of parts that were allowed in the response.
    , _lpoUploadId :: Maybe Text
      -- ^ Upload ID identifying the multipart upload whose parts are being
      -- listed.
    , _lpoNextPartNumberMarker :: Maybe Integer
      -- ^ When a list is truncated, this element specifies the last part in
      -- the list, as well as the value to use for the part-number-marker
      -- request parameter in a subsequent request.
    , _lpoKey :: Maybe ObjectKey
      -- ^ Object key for which the multipart upload was initiated.
    , _lpoOwner :: Maybe Owner
    , _lpoPartNumberMarker :: Maybe Integer
      -- ^ Part number after which listing begins.
    , _lpoParts :: [Part]
    , _lpoStorageClass :: Maybe StorageClass
      -- ^ The class of storage used to store the object.
    } deriving (Show, Generic)

instance FromXML (Rs ListParts) where
    fromXMLOptions = xmlOptions
