{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
    , cmuBucket
    , cmuKey
    , cmuMultipartUpload
    , cmuUploadId

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
    { _cmuBucket          :: Text
    , _cmuKey             :: Text
    , _cmuMultipartUpload :: Maybe CompletedMultipartUpload
    , _cmuUploadId        :: Text
    } deriving (Eq, Show, Generic)

-- | 'CompleteMultipartUpload' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmuBucket' @::@ 'Text'
--
-- * 'cmuKey' @::@ 'Text'
--
-- * 'cmuMultipartUpload' @::@ 'Maybe' 'CompletedMultipartUpload'
--
-- * 'cmuUploadId' @::@ 'Text'
--
completeMultipartUpload :: Text -- ^ 'cmuBucket'
                        -> Text -- ^ 'cmuKey'
                        -> Text -- ^ 'cmuUploadId'
                        -> CompleteMultipartUpload
completeMultipartUpload p1 p2 p3 = CompleteMultipartUpload
    { _cmuBucket          = p1
    , _cmuKey             = p2
    , _cmuUploadId        = p3
    , _cmuMultipartUpload = Nothing
    }

cmuBucket :: Lens' CompleteMultipartUpload Text
cmuBucket = lens _cmuBucket (\s a -> s { _cmuBucket = a })

cmuKey :: Lens' CompleteMultipartUpload Text
cmuKey = lens _cmuKey (\s a -> s { _cmuKey = a })

cmuMultipartUpload :: Lens' CompleteMultipartUpload (Maybe CompletedMultipartUpload)
cmuMultipartUpload =
    lens _cmuMultipartUpload (\s a -> s { _cmuMultipartUpload = a })

cmuUploadId :: Lens' CompleteMultipartUpload Text
cmuUploadId = lens _cmuUploadId (\s a -> s { _cmuUploadId = a })

instance ToPath CompleteMultipartUpload where
    toPath CompleteMultipartUpload{..} = mconcat
        [ "/"
        , toText _cmuBucket
        , "/"
        , toText _cmuKey
        ]

instance ToQuery CompleteMultipartUpload where
    toQuery x = "uploadId" =? _cmuUploadId x

instance ToHeaders CompleteMultipartUpload

instance ToBody CompleteMultipartUpload where
    toBody = toBody . encodeXML . _cmuMultipartUpload

data CompleteMultipartUploadOutput = CompleteMultipartUploadOutput
    { _cmuoBucket               :: Maybe Text
    , _cmuoETag                 :: Maybe Text
    , _cmuoExpiration           :: Maybe RFC822
    , _cmuoKey                  :: Maybe Text
    , _cmuoLocation             :: Maybe Text
    , _cmuoServerSideEncryption :: Maybe Text
    , _cmuoVersionId            :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'CompleteMultipartUploadOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmuoBucket' @::@ 'Maybe' 'Text'
--
-- * 'cmuoETag' @::@ 'Maybe' 'Text'
--
-- * 'cmuoExpiration' @::@ 'Maybe' 'UTCTime'
--
-- * 'cmuoKey' @::@ 'Maybe' 'Text'
--
-- * 'cmuoLocation' @::@ 'Maybe' 'Text'
--
-- * 'cmuoServerSideEncryption' @::@ 'Maybe' 'Text'
--
-- * 'cmuoVersionId' @::@ 'Maybe' 'Text'
--
completeMultipartUploadOutput :: CompleteMultipartUploadOutput
completeMultipartUploadOutput = CompleteMultipartUploadOutput
    { _cmuoLocation             = Nothing
    , _cmuoBucket               = Nothing
    , _cmuoKey                  = Nothing
    , _cmuoExpiration           = Nothing
    , _cmuoETag                 = Nothing
    , _cmuoServerSideEncryption = Nothing
    , _cmuoVersionId            = Nothing
    }

cmuoBucket :: Lens' CompleteMultipartUploadOutput (Maybe Text)
cmuoBucket = lens _cmuoBucket (\s a -> s { _cmuoBucket = a })

-- | Entity tag of the object.
cmuoETag :: Lens' CompleteMultipartUploadOutput (Maybe Text)
cmuoETag = lens _cmuoETag (\s a -> s { _cmuoETag = a })

-- | If the object expiration is configured, this will contain the expiration
-- date (expiry-date) and rule ID (rule-id). The value of rule-id is URL
-- encoded.
cmuoExpiration :: Lens' CompleteMultipartUploadOutput (Maybe UTCTime)
cmuoExpiration = lens _cmuoExpiration (\s a -> s { _cmuoExpiration = a })
    . mapping _Time

cmuoKey :: Lens' CompleteMultipartUploadOutput (Maybe Text)
cmuoKey = lens _cmuoKey (\s a -> s { _cmuoKey = a })

cmuoLocation :: Lens' CompleteMultipartUploadOutput (Maybe Text)
cmuoLocation = lens _cmuoLocation (\s a -> s { _cmuoLocation = a })

-- | The Server-side encryption algorithm used when storing this object in S3.
cmuoServerSideEncryption :: Lens' CompleteMultipartUploadOutput (Maybe Text)
cmuoServerSideEncryption =
    lens _cmuoServerSideEncryption
        (\s a -> s { _cmuoServerSideEncryption = a })

-- | Version of the object.
cmuoVersionId :: Lens' CompleteMultipartUploadOutput (Maybe Text)
cmuoVersionId = lens _cmuoVersionId (\s a -> s { _cmuoVersionId = a })

instance AWSRequest CompleteMultipartUpload where
    type Sv CompleteMultipartUpload = S3
    type Rs CompleteMultipartUpload = CompleteMultipartUploadOutput

    request  = post
    response = const . xmlResponse $ \h x -> CompleteMultipartUploadOutput
        <$> x %| "Bucket"
        <*> x %| "ETag"
        <*> h ~:? "x-amz-expiration"
        <*> x %| "Key"
        <*> x %| "Location"
        <*> h ~:? "x-amz-server-side-encryption"
        <*> h ~:? "x-amz-version-id"
