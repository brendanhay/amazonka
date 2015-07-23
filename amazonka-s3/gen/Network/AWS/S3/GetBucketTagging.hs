{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketTagging
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the tag set associated with the bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketTagging.html>
module Network.AWS.S3.GetBucketTagging
    (
    -- * Request
      GetBucketTagging
    -- ** Request constructor
    , getBucketTagging
    -- ** Request lenses
    , gbtrqBucket

    -- * Response
    , GetBucketTaggingResponse
    -- ** Response constructor
    , getBucketTaggingResponse
    -- ** Response lenses
    , gbtrsStatus
    , gbtrsTagSet
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getBucketTagging' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbtrqBucket'
newtype GetBucketTagging = GetBucketTagging'
    { _gbtrqBucket :: BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetBucketTagging' smart constructor.
getBucketTagging :: BucketName -> GetBucketTagging
getBucketTagging pBucket_ =
    GetBucketTagging'
    { _gbtrqBucket = pBucket_
    }

-- | FIXME: Undocumented member.
gbtrqBucket :: Lens' GetBucketTagging BucketName
gbtrqBucket = lens _gbtrqBucket (\ s a -> s{_gbtrqBucket = a});

instance AWSRequest GetBucketTagging where
        type Sv GetBucketTagging = S3
        type Rs GetBucketTagging = GetBucketTaggingResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetBucketTaggingResponse' <$>
                   (pure (fromEnum s)) <*>
                     (x .@? "TagSet" .!@ mempty >>= parseXMLList "Tag"))

instance ToHeaders GetBucketTagging where
        toHeaders = const mempty

instance ToPath GetBucketTagging where
        toPath GetBucketTagging'{..}
          = mconcat ["/", toText _gbtrqBucket]

instance ToQuery GetBucketTagging where
        toQuery = const (mconcat ["tagging"])

-- | /See:/ 'getBucketTaggingResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbtrsStatus'
--
-- * 'gbtrsTagSet'
data GetBucketTaggingResponse = GetBucketTaggingResponse'
    { _gbtrsStatus :: !Int
    , _gbtrsTagSet :: ![Tag]
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetBucketTaggingResponse' smart constructor.
getBucketTaggingResponse :: Int -> GetBucketTaggingResponse
getBucketTaggingResponse pStatus_ =
    GetBucketTaggingResponse'
    { _gbtrsStatus = pStatus_
    , _gbtrsTagSet = mempty
    }

-- | FIXME: Undocumented member.
gbtrsStatus :: Lens' GetBucketTaggingResponse Int
gbtrsStatus = lens _gbtrsStatus (\ s a -> s{_gbtrsStatus = a});

-- | FIXME: Undocumented member.
gbtrsTagSet :: Lens' GetBucketTaggingResponse [Tag]
gbtrsTagSet = lens _gbtrsTagSet (\ s a -> s{_gbtrsTagSet = a});
