{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.V2006_03_01.CompleteMultipartUpload
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Completes a multipart upload by assembling previously uploaded parts.
module Network.AWS.S3.V2006_03_01.CompleteMultipartUpload where

import Network.AWS.Request.RestS3
import Network.AWS.S3.V2006_03_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'CompleteMultipartUpload' request.
completeMultipartUpload :: BucketName -- ^ '_cmusBucket'
                        -> Text -- ^ '_cmusUploadId'
                        -> ObjectKey -- ^ '_cmusKey'
                        -> CompleteMultipartUpload
completeMultipartUpload p1 p2 p3 = CompleteMultipartUpload
    { _cmusBucket = p1
    , _cmusUploadId = p2
    , _cmusKey = p3
    , _cmusMultipartUpload = Nothing
    }

data CompleteMultipartUpload = CompleteMultipartUpload
    { _cmusBucket :: BucketName
    , _cmusUploadId :: Text
    , _cmusKey :: ObjectKey
    , _cmusMultipartUpload :: Maybe CompletedMultipartUpload
    } deriving (Show, Generic)

makeLenses ''CompleteMultipartUpload

instance ToPath CompleteMultipartUpload where
    toPath CompleteMultipartUpload{..} = mconcat
        [ "/"
        , toBS _cmusBucket
        , "/"
        , toBS _cmusKey
        ]

instance ToQuery CompleteMultipartUpload where
    toQuery CompleteMultipartUpload{..} = mconcat
        [ "uploadId" =? _cmusUploadId
        ]

instance ToHeaders CompleteMultipartUpload

instance ToBody CompleteMultipartUpload where
    toBody = toBody . encodeXML . _cmusMultipartUpload

data CompleteMultipartUploadResponse = CompleteMultipartUploadResponse
    { _cmupBucket :: Maybe BucketName
    , _cmupETag :: Maybe ETag
      -- ^ Entity tag of the object.
    , _cmupLocation :: Maybe Text
    , _cmupKey :: Maybe ObjectKey
    , _cmupExpiration :: Maybe RFC822
      -- ^ If the object expiration is configured, this will contain the
      -- expiration date (expiry-date) and rule ID (rule-id). The value of
      -- rule-id is URL encoded.
    , _cmupVersionId :: Maybe ObjectVersionId
      -- ^ Version of the object.
    , _cmupServerSideEncryption :: Maybe ServerSideEncryption
      -- ^ The Server-side encryption algorithm used when storing this
      -- object in S3.
    } deriving (Show, Generic)

makeLenses ''CompleteMultipartUploadResponse

instance AWSRequest CompleteMultipartUpload where
    type Sv CompleteMultipartUpload = S3
    type Rs CompleteMultipartUpload = CompleteMultipartUploadResponse

    request = post
    response _ = cursorResponse $ \hs xml ->
        pure CompleteMultipartUploadResponse
            <*> xml %|? "BucketName"
            <*> xml %|? "ETag"
            <*> xml %|? "Location"
            <*> xml %|? "ObjectKey"
            <*> hs ~:? "x-amz-expiration"
            <*> hs ~:? "x-amz-version-id"
            <*> hs ~:? "x-amz-server-side-encryption"
