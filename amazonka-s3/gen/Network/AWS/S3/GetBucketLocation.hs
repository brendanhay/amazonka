{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketLocation
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the region the bucket resides in.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketLocation.html>
module Network.AWS.S3.GetBucketLocation
    (
    -- * Request
      GetBucketLocation
    -- ** Request constructor
    , getBucketLocation
    -- ** Request lenses
    , gblrqBucket

    -- * Response
    , GetBucketLocationResponse
    -- ** Response constructor
    , getBucketLocationResponse
    -- ** Response lenses
    , gblrsLocationConstraint
    , gblrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getBucketLocation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gblrqBucket'
newtype GetBucketLocation = GetBucketLocation'
    { _gblrqBucket :: BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetBucketLocation' smart constructor.
getBucketLocation :: BucketName -> GetBucketLocation
getBucketLocation pBucket =
    GetBucketLocation'
    { _gblrqBucket = pBucket
    }

-- | FIXME: Undocumented member.
gblrqBucket :: Lens' GetBucketLocation BucketName
gblrqBucket = lens _gblrqBucket (\ s a -> s{_gblrqBucket = a});

instance AWSRequest GetBucketLocation where
        type Sv GetBucketLocation = S3
        type Rs GetBucketLocation = GetBucketLocationResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetBucketLocationResponse' <$>
                   (x .@? "LocationConstraint") <*> (pure (fromEnum s)))

instance ToHeaders GetBucketLocation where
        toHeaders = const mempty

instance ToPath GetBucketLocation where
        toPath GetBucketLocation'{..}
          = mconcat ["/", toText _gblrqBucket]

instance ToQuery GetBucketLocation where
        toQuery = const (mconcat ["location"])

-- | /See:/ 'getBucketLocationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gblrsLocationConstraint'
--
-- * 'gblrsStatus'
data GetBucketLocationResponse = GetBucketLocationResponse'
    { _gblrsLocationConstraint :: !(Maybe Region)
    , _gblrsStatus             :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetBucketLocationResponse' smart constructor.
getBucketLocationResponse :: Int -> GetBucketLocationResponse
getBucketLocationResponse pStatus =
    GetBucketLocationResponse'
    { _gblrsLocationConstraint = Nothing
    , _gblrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
gblrsLocationConstraint :: Lens' GetBucketLocationResponse (Maybe Region)
gblrsLocationConstraint = lens _gblrsLocationConstraint (\ s a -> s{_gblrsLocationConstraint = a});

-- | FIXME: Undocumented member.
gblrsStatus :: Lens' GetBucketLocationResponse Int
gblrsStatus = lens _gblrsStatus (\ s a -> s{_gblrsStatus = a});
