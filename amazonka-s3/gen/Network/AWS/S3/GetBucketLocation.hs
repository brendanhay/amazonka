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
    , gblBucket

    -- * Response
    , GetBucketLocationResponse
    -- ** Response constructor
    , getBucketLocationResponse
    -- ** Response lenses
    , getrsLocationConstraint
    , getrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getBucketLocation' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gblBucket'
newtype GetBucketLocation = GetBucketLocation'
    { _gblBucket :: BucketName
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBucketLocation' smart constructor.
getBucketLocation :: BucketName -> GetBucketLocation
getBucketLocation pBucket_ =
    GetBucketLocation'
    { _gblBucket = pBucket_
    }

-- | FIXME: Undocumented member.
gblBucket :: Lens' GetBucketLocation BucketName
gblBucket = lens _gblBucket (\ s a -> s{_gblBucket = a});

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
          = mconcat ["/", toBS _gblBucket]

instance ToQuery GetBucketLocation where
        toQuery = const (mconcat ["location"])

-- | /See:/ 'getBucketLocationResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'getrsLocationConstraint'
--
-- * 'getrsStatus'
data GetBucketLocationResponse = GetBucketLocationResponse'
    { _getrsLocationConstraint :: !(Maybe Region)
    , _getrsStatus             :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBucketLocationResponse' smart constructor.
getBucketLocationResponse :: Int -> GetBucketLocationResponse
getBucketLocationResponse pStatus_ =
    GetBucketLocationResponse'
    { _getrsLocationConstraint = Nothing
    , _getrsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
getrsLocationConstraint :: Lens' GetBucketLocationResponse (Maybe Region)
getrsLocationConstraint = lens _getrsLocationConstraint (\ s a -> s{_getrsLocationConstraint = a});

-- | FIXME: Undocumented member.
getrsStatus :: Lens' GetBucketLocationResponse Int
getrsStatus = lens _getrsStatus (\ s a -> s{_getrsStatus = a});
