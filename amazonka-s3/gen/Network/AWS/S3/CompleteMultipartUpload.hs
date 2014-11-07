{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

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
    { _cmurBucket          :: Text
    , _cmurKey             :: Text
    , _cmurMultipartUpload :: Maybe CompletedMultipartUpload
    , _cmurUploadId        :: Text
    } deriving (Eq, Show, Generic)

-- | 'CompleteMultipartUpload' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cmurBucket' @::@ 'Text'
--
-- * 'cmurKey' @::@ 'Text'
--
-- * 'cmurMultipartUpload' @::@ 'Maybe' 'CompletedMultipartUpload'
--
-- * 'cmurUploadId' @::@ 'Text'
--
completeMultipartUpload :: Text -- ^ 'cmurBucket'
                        -> Text -- ^ 'cmurKey'
                        -> Text -- ^ 'cmurUploadId'
                        -> CompleteMultipartUpload
completeMultipartUpload p1 p2 p3 = CompleteMultipartUpload
    { _cmurBucket          = p1
    , _cmurKey             = p2
    , _cmurUploadId        = p3
    , _cmurMultipartUpload = Nothing
    }

cmurBucket :: Lens' CompleteMultipartUpload Text
cmurBucket = lens _cmurBucket (\s a -> s { _cmurBucket = a })

cmurKey :: Lens' CompleteMultipartUpload Text
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

    request  = post'
    response = const . xmlResponse $ \h x -> CompleteMultipartUploadOutput
        <$> x %| "Bucket"
        <*> x %| "ETag"
        <*> h ~:? "x-amz-expiration"
        <*> x %| "Key"
        <*> x %| "Location"
        <*> h ~:? "x-amz-server-side-encryption"
        <*> h ~:? "x-amz-version-id"
