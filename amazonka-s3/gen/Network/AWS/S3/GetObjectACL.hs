{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetObjectACL
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns the access control list (ACL) of an object.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/GetObjectACL.html>
module Network.AWS.S3.GetObjectACL
    (
    -- * Request
      GetObjectACL
    -- ** Request constructor
    , getObjectACL
    -- ** Request lenses
    , goarqVersionId
    , goarqRequestPayer
    , goarqBucket
    , goarqKey

    -- * Response
    , GetObjectACLResponse
    -- ** Response constructor
    , getObjectACLResponse
    -- ** Response lenses
    , goarsRequestCharged
    , goarsGrants
    , goarsOwner
    , goarsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'getObjectACL' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goarqVersionId'
--
-- * 'goarqRequestPayer'
--
-- * 'goarqBucket'
--
-- * 'goarqKey'
data GetObjectACL = GetObjectACL'
    { _goarqVersionId    :: !(Maybe ObjectVersionId)
    , _goarqRequestPayer :: !(Maybe RequestPayer)
    , _goarqBucket       :: !BucketName
    , _goarqKey          :: !ObjectKey
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'GetObjectACL' smart constructor.
getObjectACL :: BucketName -> ObjectKey -> GetObjectACL
getObjectACL pBucket pKey =
    GetObjectACL'
    { _goarqVersionId = Nothing
    , _goarqRequestPayer = Nothing
    , _goarqBucket = pBucket
    , _goarqKey = pKey
    }

-- | VersionId used to reference a specific version of the object.
goarqVersionId :: Lens' GetObjectACL (Maybe ObjectVersionId)
goarqVersionId = lens _goarqVersionId (\ s a -> s{_goarqVersionId = a});

-- | FIXME: Undocumented member.
goarqRequestPayer :: Lens' GetObjectACL (Maybe RequestPayer)
goarqRequestPayer = lens _goarqRequestPayer (\ s a -> s{_goarqRequestPayer = a});

-- | FIXME: Undocumented member.
goarqBucket :: Lens' GetObjectACL BucketName
goarqBucket = lens _goarqBucket (\ s a -> s{_goarqBucket = a});

-- | FIXME: Undocumented member.
goarqKey :: Lens' GetObjectACL ObjectKey
goarqKey = lens _goarqKey (\ s a -> s{_goarqKey = a});

instance AWSRequest GetObjectACL where
        type Sv GetObjectACL = S3
        type Rs GetObjectACL = GetObjectACLResponse
        request = get
        response
          = receiveXML
              (\ s h x ->
                 GetObjectACLResponse' <$>
                   (h .#? "x-amz-request-charged") <*>
                     (x .@? "AccessControlList" .!@ mempty >>=
                        may (parseXMLList "Grant"))
                     <*> (x .@? "Owner")
                     <*> (pure (fromEnum s)))

instance ToHeaders GetObjectACL where
        toHeaders GetObjectACL'{..}
          = mconcat
              ["x-amz-request-payer" =# _goarqRequestPayer]

instance ToPath GetObjectACL where
        toPath GetObjectACL'{..}
          = mconcat
              ["/", toText _goarqBucket, "/", toText _goarqKey]

instance ToQuery GetObjectACL where
        toQuery GetObjectACL'{..}
          = mconcat ["versionId" =: _goarqVersionId, "acl"]

-- | /See:/ 'getObjectACLResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'goarsRequestCharged'
--
-- * 'goarsGrants'
--
-- * 'goarsOwner'
--
-- * 'goarsStatus'
data GetObjectACLResponse = GetObjectACLResponse'
    { _goarsRequestCharged :: !(Maybe RequestCharged)
    , _goarsGrants         :: !(Maybe [Grant])
    , _goarsOwner          :: !(Maybe Owner)
    , _goarsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetObjectACLResponse' smart constructor.
getObjectACLResponse :: Int -> GetObjectACLResponse
getObjectACLResponse pStatus =
    GetObjectACLResponse'
    { _goarsRequestCharged = Nothing
    , _goarsGrants = Nothing
    , _goarsOwner = Nothing
    , _goarsStatus = pStatus
    }

-- | FIXME: Undocumented member.
goarsRequestCharged :: Lens' GetObjectACLResponse (Maybe RequestCharged)
goarsRequestCharged = lens _goarsRequestCharged (\ s a -> s{_goarsRequestCharged = a});

-- | A list of grants.
goarsGrants :: Lens' GetObjectACLResponse [Grant]
goarsGrants = lens _goarsGrants (\ s a -> s{_goarsGrants = a}) . _Default;

-- | FIXME: Undocumented member.
goarsOwner :: Lens' GetObjectACLResponse (Maybe Owner)
goarsOwner = lens _goarsOwner (\ s a -> s{_goarsOwner = a});

-- | FIXME: Undocumented member.
goarsStatus :: Lens' GetObjectACLResponse Int
goarsStatus = lens _goarsStatus (\ s a -> s{_goarsStatus = a});
