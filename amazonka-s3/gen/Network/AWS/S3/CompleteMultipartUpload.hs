{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.S3.CompleteMultipartUpload
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Completes a multipart upload by assembling previously uploaded parts.
module Network.AWS.S3.CompleteMultipartUpload
    (
    -- * Request
      CompleteMultipartUpload
    -- ** Request constructor
    , completeMultipartUpload
    -- ** Request lenses
    , cmurBucket
    , cmurKey
    , cmurMultipartUpload
    , cmurUploadId

    -- * Response
    , CompleteMultipartUploadOutput
    -- ** Response constructor
    , completeMultipartUploadOutput
    -- ** Response lenses
    , cmuoBucket
    , cmuoETag
    , cmuoExpiration
    , cmuoKey
    , cmuoLocation
    , cmuoServerSideEncryption
    , cmuoVersionId
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.S3.Types

data CompleteMultipartUpload = CompleteMultipartUpload
    { _cmurBucket          :: BucketName
    , _cmurKey             :: ObjectKey
    , _cmurMultipartUpload :: Maybe CompletedMultipartUpload
    , _cmurUploadId        :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CompleteMultipartUpload' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmurBucket' @::@ 'BucketName'
--
-- * 'cmurKey' @::@ 'ObjectKey'
--
-- * 'cmurMultipartUpload' @::@ 'Maybe' 'CompletedMultipartUpload'
--
-- * 'cmurUploadId' @::@ 'Text'
--
completeMultipartUpload :: BucketName -- ^ 'cmurBucket'
                        -> ObjectKey -- ^ 'cmurKey'
                        -> Text -- ^ 'cmurUploadId'
                        -> CompleteMultipartUpload
completeMultipartUpload p1 p2 p3 = CompleteMultipartUpload
    { _cmurBucket          = p1
    , _cmurKey             = p2
    , _cmurUploadId        = p3
    , _cmurMultipartUpload = Nothing
    }

cmurBucket :: Lens' CompleteMultipartUpload BucketName
cmurBucket = lens _cmurBucket (\s a -> s { _cmurBucket = a })

cmurKey :: Lens' CompleteMultipartUpload ObjectKey
cmurKey = lens _cmurKey (\s a -> s { _cmurKey = a })

cmurMultipartUpload :: Lens' CompleteMultipartUpload (Maybe CompletedMultipartUpload)
cmurMultipartUpload =
    lens _cmurMultipartUpload (\s a -> s { _cmurMultipartUpload = a })

cmurUploadId :: Lens' CompleteMultipartUpload Text
cmurUploadId = lens _cmurUploadId (\s a -> s { _cmurUploadId = a })

instance ToPath CompleteMultipartUpload where
    toPath CompleteMultipartUpload{..} = mconcat
        [ "/"
        , toText _cmurBucket
        , "/"
        , toText _cmurKey
        ]

instance ToQuery CompleteMultipartUpload where

instance ToHeaders CompleteMultipartUpload

instance ToBody CompleteMultipartUpload where
    toBody = toBody . encodeXML . _cmurMultipartUpload

data CompleteMultipartUploadOutput = CompleteMultipartUploadOutput
    { _cmuoBucket               :: Maybe BucketName
    , _cmuoETag                 :: Maybe ETag
    , _cmuoExpiration           :: Maybe RFC822
    , _cmuoKey                  :: Maybe ObjectKey
    , _cmuoLocation             :: Maybe Text
    , _cmuoServerSideEncryption :: Maybe Text
    , _cmuoVersionId            :: Maybe ObjectVersionId
    } deriving (Eq, Ord, Show, Generic)

instance AWSRequest CompleteMultipartUpload where
    type Sv CompleteMultipartUpload = S3
    type Rs CompleteMultipartUpload = CompleteMultipartUploadOutput

    request  = post
    response = const . xmlResponse $ \h x ->
        <$> x %| "Bucket"
        <*> x %| "ETag"
        <*> h ~: "x-amz-expiration"
        <*> x %| "Key"
        <*> x %| "Location"
        <*> h ~: "x-amz-server-side-encryption"
        <*> h ~: "x-amz-version-id"
