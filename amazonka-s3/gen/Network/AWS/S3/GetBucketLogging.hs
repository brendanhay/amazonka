{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketLogging
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- | Returns the logging status of a bucket and the permissions users have to
-- view and modify that status. To use GET, you must be the bucket owner.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketLogging.html>
module Network.AWS.S3.GetBucketLogging
    (
    -- * Request
      GetBucketLogging
    -- ** Request constructor
    , getBucketLogging
    -- ** Request lenses
    , gBucket

    -- * Response
    , GetBucketLoggingResponse
    -- ** Response constructor
    , getBucketLoggingResponse
    -- ** Response lenses
    , getLoggingEnabled
    , getStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getBucketLogging' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gBucket'
newtype GetBucketLogging = GetBucketLogging'
    { _gBucket :: BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetBucketLogging' smart constructor.
getBucketLogging :: BucketName -> GetBucketLogging
getBucketLogging pBucket =
    GetBucketLogging'
    { _gBucket = pBucket
    }

-- | FIXME: Undocumented member.
gBucket :: Lens' GetBucketLogging BucketName
gBucket = lens _gBucket (\ s a -> s{_gBucket = a});

instance AWSRequest GetBucketLogging where
        type Sv GetBucketLogging = S3
        type Rs GetBucketLogging = GetBucketLoggingResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetBucketLoggingResponse' <$>
                   (x .@? "LoggingEnabled") <*> (pure (fromEnum s)))

instance ToHeaders GetBucketLogging where
        toHeaders = const mempty

instance ToPath GetBucketLogging where
        toPath GetBucketLogging'{..}
          = mconcat ["/", toText _gBucket]

instance ToQuery GetBucketLogging where
        toQuery = const (mconcat ["logging"])

-- | /See:/ 'getBucketLoggingResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'getLoggingEnabled'
--
-- * 'getStatus'
data GetBucketLoggingResponse = GetBucketLoggingResponse'
    { _getLoggingEnabled :: !(Maybe LoggingEnabled)
    , _getStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBucketLoggingResponse' smart constructor.
getBucketLoggingResponse :: Int -> GetBucketLoggingResponse
getBucketLoggingResponse pStatus =
    GetBucketLoggingResponse'
    { _getLoggingEnabled = Nothing
    , _getStatus = pStatus
    }

-- | FIXME: Undocumented member.
getLoggingEnabled :: Lens' GetBucketLoggingResponse (Maybe LoggingEnabled)
getLoggingEnabled = lens _getLoggingEnabled (\ s a -> s{_getLoggingEnabled = a});

-- | FIXME: Undocumented member.
getStatus :: Lens' GetBucketLoggingResponse Int
getStatus = lens _getStatus (\ s a -> s{_getStatus = a});
