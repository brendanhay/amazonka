{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetBucketACL
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Gets the access control policy for the bucket.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetBucketACL.html>
module Network.AWS.S3.GetBucketACL
    (
    -- * Request
      GetBucketACL
    -- ** Request constructor
    , getBucketACL
    -- ** Request lenses
    , gbaBucket

    -- * Response
    , GetBucketACLResponse
    -- ** Response constructor
    , getBucketACLResponse
    -- ** Response lenses
    , gbarGrants
    , gbarOwner
    , gbarStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getBucketACL' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbaBucket'
newtype GetBucketACL = GetBucketACL'
    { _gbaBucket :: BucketName
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetBucketACL' smart constructor.
getBucketACL :: BucketName -> GetBucketACL
getBucketACL pBucket =
    GetBucketACL'
    { _gbaBucket = pBucket
    }

-- | FIXME: Undocumented member.
gbaBucket :: Lens' GetBucketACL BucketName
gbaBucket = lens _gbaBucket (\ s a -> s{_gbaBucket = a});

instance AWSRequest GetBucketACL where
        type Sv GetBucketACL = S3
        type Rs GetBucketACL = GetBucketACLResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetBucketACLResponse' <$>
                   (x .@? "AccessControlList" .!@ mempty >>=
                      may (parseXMLList "Grant"))
                     <*> (x .@? "Owner")
                     <*> (pure (fromEnum s)))

instance ToHeaders GetBucketACL where
        toHeaders = const mempty

instance ToPath GetBucketACL where
        toPath GetBucketACL'{..}
          = mconcat ["/", toText _gbaBucket]

instance ToQuery GetBucketACL where
        toQuery = const (mconcat ["acl"])

-- | /See:/ 'getBucketACLResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'gbarGrants'
--
-- * 'gbarOwner'
--
-- * 'gbarStatus'
data GetBucketACLResponse = GetBucketACLResponse'
    { _gbarGrants :: !(Maybe [Grant])
    , _gbarOwner  :: !(Maybe Owner)
    , _gbarStatus :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetBucketACLResponse' smart constructor.
getBucketACLResponse :: Int -> GetBucketACLResponse
getBucketACLResponse pStatus =
    GetBucketACLResponse'
    { _gbarGrants = Nothing
    , _gbarOwner = Nothing
    , _gbarStatus = pStatus
    }

-- | A list of grants.
gbarGrants :: Lens' GetBucketACLResponse [Grant]
gbarGrants = lens _gbarGrants (\ s a -> s{_gbarGrants = a}) . _Default;

-- | FIXME: Undocumented member.
gbarOwner :: Lens' GetBucketACLResponse (Maybe Owner)
gbarOwner = lens _gbarOwner (\ s a -> s{_gbarOwner = a});

-- | FIXME: Undocumented member.
gbarStatus :: Lens' GetBucketACLResponse Int
gbarStatus = lens _gbarStatus (\ s a -> s{_gbarStatus = a});
