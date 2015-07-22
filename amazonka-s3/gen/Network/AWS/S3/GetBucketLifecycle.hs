{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketLifecycle
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the lifecycle configuration information set on the bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketLifecycle.html>
module Network.AWS.S3.GetBucketLifecycle
    (
    -- * Request
      GetBucketLifecycle
    -- ** Request constructor
    , getBucketLifecycle
    -- ** Request lenses
    , grqBucket

    -- * Response
    , GetBucketLifecycleResponse
    -- ** Response constructor
    , getBucketLifecycleResponse
    -- ** Response lenses
    , grsRules
    , grsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getBucketLifecycle' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grqBucket'
newtype GetBucketLifecycle = GetBucketLifecycle'
    { _grqBucket :: BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetBucketLifecycle' smart constructor.
getBucketLifecycle :: BucketName -> GetBucketLifecycle
getBucketLifecycle pBucket =
    GetBucketLifecycle'
    { _grqBucket = pBucket
    }

-- | FIXME: Undocumented member.
grqBucket :: Lens' GetBucketLifecycle BucketName
grqBucket = lens _grqBucket (\ s a -> s{_grqBucket = a});

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
          = mconcat ["/", toText _grqBucket]

instance ToQuery GetBucketLifecycle where
        toQuery = const (mconcat ["lifecycle"])

-- | /See:/ 'getBucketLifecycleResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'grsRules'
--
-- * 'grsStatus'
data GetBucketLifecycleResponse = GetBucketLifecycleResponse'
    { _grsRules  :: !(Maybe [Rule])
    , _grsStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBucketLifecycleResponse' smart constructor.
getBucketLifecycleResponse :: Int -> GetBucketLifecycleResponse
getBucketLifecycleResponse pStatus =
    GetBucketLifecycleResponse'
    { _grsRules = Nothing
    , _grsStatus = pStatus
    }

-- | FIXME: Undocumented member.
grsRules :: Lens' GetBucketLifecycleResponse [Rule]
grsRules = lens _grsRules (\ s a -> s{_grsRules = a}) . _Default;

-- | FIXME: Undocumented member.
grsStatus :: Lens' GetBucketLifecycleResponse Int
grsStatus = lens _grsStatus (\ s a -> s{_grsStatus = a});
