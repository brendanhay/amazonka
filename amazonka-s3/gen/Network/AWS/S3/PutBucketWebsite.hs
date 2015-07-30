{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketWebsite
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Set the website configuration for a bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/PutBucketWebsite.html>
module Network.AWS.S3.PutBucketWebsite
    (
    -- * Request
      PutBucketWebsite
    -- ** Request constructor
    , putBucketWebsite
    -- ** Request lenses
    , pbwContentMD5
    , pbwBucket
    , pbwWebsiteConfiguration

    -- * Response
    , PutBucketWebsiteResponse
    -- ** Response constructor
    , putBucketWebsiteResponse
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'putBucketWebsite' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pbwContentMD5'
--
-- * 'pbwBucket'
--
-- * 'pbwWebsiteConfiguration'
data PutBucketWebsite = PutBucketWebsite'
    { _pbwContentMD5           :: !(Maybe Text)
    , _pbwBucket               :: !BucketName
    , _pbwWebsiteConfiguration :: !WebsiteConfiguration
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketWebsite' smart constructor.
putBucketWebsite :: BucketName -> WebsiteConfiguration -> PutBucketWebsite
putBucketWebsite pBucket_ pWebsiteConfiguration_ =
    PutBucketWebsite'
    { _pbwContentMD5 = Nothing
    , _pbwBucket = pBucket_
    , _pbwWebsiteConfiguration = pWebsiteConfiguration_
    }

-- | FIXME: Undocumented member.
pbwContentMD5 :: Lens' PutBucketWebsite (Maybe Text)
pbwContentMD5 = lens _pbwContentMD5 (\ s a -> s{_pbwContentMD5 = a});

-- | FIXME: Undocumented member.
pbwBucket :: Lens' PutBucketWebsite BucketName
pbwBucket = lens _pbwBucket (\ s a -> s{_pbwBucket = a});

-- | FIXME: Undocumented member.
pbwWebsiteConfiguration :: Lens' PutBucketWebsite WebsiteConfiguration
pbwWebsiteConfiguration = lens _pbwWebsiteConfiguration (\ s a -> s{_pbwWebsiteConfiguration = a});

instance AWSRequest PutBucketWebsite where
        type Sv PutBucketWebsite = S3
        type Rs PutBucketWebsite = PutBucketWebsiteResponse
        request = putXML
        response = receiveNull PutBucketWebsiteResponse'

instance ToElement PutBucketWebsite where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}WebsiteConfiguration"
              .
              _pbwWebsiteConfiguration

instance ToHeaders PutBucketWebsite where
        toHeaders PutBucketWebsite'{..}
          = mconcat ["Content-MD5" =# _pbwContentMD5]

instance ToPath PutBucketWebsite where
        toPath PutBucketWebsite'{..} = [toBS _pbwBucket]

instance ToQuery PutBucketWebsite where
        toQuery = const (mconcat ["website"])

-- | /See:/ 'putBucketWebsiteResponse' smart constructor.
data PutBucketWebsiteResponse =
    PutBucketWebsiteResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'PutBucketWebsiteResponse' smart constructor.
putBucketWebsiteResponse :: PutBucketWebsiteResponse
putBucketWebsiteResponse = PutBucketWebsiteResponse'
