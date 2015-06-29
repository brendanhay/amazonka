{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.GetBucketTagging
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns the tag set associated with the bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketTagging.html>
module Network.AWS.S3.GetBucketTagging
    (
    -- * Request
      GetBucketTagging
    -- ** Request constructor
    , getBucketTagging
    -- ** Request lenses
    , gbtBucket

    -- * Response
    , GetBucketTaggingResponse
    -- ** Response constructor
    , getBucketTaggingResponse
    -- ** Response lenses
    , gbtrStatus
    , gbtrTagSet
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getBucketTagging' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbtBucket'
newtype GetBucketTagging = GetBucketTagging'
    { _gbtBucket :: BucketName
    } deriving (Eq,Show)

-- | 'GetBucketTagging' smart constructor.
getBucketTagging :: BucketName -> GetBucketTagging
getBucketTagging pBucket =
    GetBucketTagging'
    { _gbtBucket = pBucket
    }

-- | FIXME: Undocumented member.
gbtBucket :: Lens' GetBucketTagging BucketName
gbtBucket = lens _gbtBucket (\ s a -> s{_gbtBucket = a});

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
          = mconcat ["/", toText _gbtBucket]

instance ToQuery GetBucketTagging where
        toQuery = const (mconcat ["tagging"])

-- | /See:/ 'getBucketTaggingResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbtrStatus'
--
-- * 'gbtrTagSet'
data GetBucketTaggingResponse = GetBucketTaggingResponse'
    { _gbtrStatus :: !Int
    , _gbtrTagSet :: ![Tag]
    } deriving (Eq,Show)

-- | 'GetBucketTaggingResponse' smart constructor.
getBucketTaggingResponse :: Int -> GetBucketTaggingResponse
getBucketTaggingResponse pStatus =
    GetBucketTaggingResponse'
    { _gbtrStatus = pStatus
    , _gbtrTagSet = mempty
    }

-- | FIXME: Undocumented member.
gbtrStatus :: Lens' GetBucketTaggingResponse Int
gbtrStatus = lens _gbtrStatus (\ s a -> s{_gbtrStatus = a});

-- | FIXME: Undocumented member.
gbtrTagSet :: Lens' GetBucketTaggingResponse [Tag]
gbtrTagSet = lens _gbtrTagSet (\ s a -> s{_gbtrTagSet = a});
