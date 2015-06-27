{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.S3.GetBucketLifecycle
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

-- | Returns the lifecycle configuration information set on the bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketLifecycle.html>
module Network.AWS.S3.GetBucketLifecycle
    (
    -- * Request
      GetBucketLifecycle
    -- ** Request constructor
    , getBucketLifecycle
    -- ** Request lenses
    , getBucket

    -- * Response
    , GetBucketLifecycleResponse
    -- ** Response constructor
    , getBucketLifecycleResponse
    -- ** Response lenses
    , gblrRules
    , gblrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getBucketLifecycle' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'getBucket'
newtype GetBucketLifecycle = GetBucketLifecycle'
    { _getBucket :: BucketName
    } deriving (Eq,Read,Show)

-- | 'GetBucketLifecycle' smart constructor.
getBucketLifecycle :: BucketName -> GetBucketLifecycle
getBucketLifecycle pBucket =
    GetBucketLifecycle'
    { _getBucket = pBucket
    }

-- | FIXME: Undocumented member.
getBucket :: Lens' GetBucketLifecycle BucketName
getBucket = lens _getBucket (\ s a -> s{_getBucket = a});

instance AWSRequest GetBucketLifecycle where
        type Sv GetBucketLifecycle = S3
        type Rs GetBucketLifecycle =
             GetBucketLifecycleResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetBucketLifecycleResponse' <$>
                   (may (parseXMLList "Rule") x) <*>
                     (pure (fromEnum s)))

instance ToHeaders GetBucketLifecycle where
        toHeaders = const mempty

instance ToPath GetBucketLifecycle where
        toPath GetBucketLifecycle'{..}
          = mconcat ["/", toText _getBucket]

instance ToQuery GetBucketLifecycle where
        toQuery = const (mconcat ["lifecycle"])

-- | /See:/ 'getBucketLifecycleResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gblrRules'
--
-- * 'gblrStatus'
data GetBucketLifecycleResponse = GetBucketLifecycleResponse'
    { _gblrRules  :: Maybe [Rule]
    , _gblrStatus :: !Int
    } deriving (Eq,Read,Show)

-- | 'GetBucketLifecycleResponse' smart constructor.
getBucketLifecycleResponse :: Int -> GetBucketLifecycleResponse
getBucketLifecycleResponse pStatus =
    GetBucketLifecycleResponse'
    { _gblrRules = Nothing
    , _gblrStatus = pStatus
    }

-- | FIXME: Undocumented member.
gblrRules :: Lens' GetBucketLifecycleResponse [Rule]
gblrRules = lens _gblrRules (\ s a -> s{_gblrRules = a}) . _Default;

-- | FIXME: Undocumented member.
gblrStatus :: Lens' GetBucketLifecycleResponse Int
gblrStatus = lens _gblrStatus (\ s a -> s{_gblrStatus = a});
