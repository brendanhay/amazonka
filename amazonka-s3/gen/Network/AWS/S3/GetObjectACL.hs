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
    , goaVersionId
    , goaRequestPayer
    , goaBucket
    , goaKey

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
-- * 'goaVersionId'
--
-- * 'goaRequestPayer'
--
-- * 'goaBucket'
--
-- * 'goaKey'
data GetObjectACL = GetObjectACL'
    { _goaVersionId    :: !(Maybe ObjectVersionId)
    , _goaRequestPayer :: !(Maybe RequestPayer)
    , _goaBucket       :: !BucketName
    , _goaKey          :: !ObjectKey
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'GetObjectACL' smart constructor.
getObjectACL :: BucketName -> ObjectKey -> GetObjectACL
getObjectACL pBucket_ pKey_ =
    GetObjectACL'
    { _goaVersionId = Nothing
    , _goaRequestPayer = Nothing
    , _goaBucket = pBucket_
    , _goaKey = pKey_
    }

-- | VersionId used to reference a specific version of the object.
goaVersionId :: Lens' GetObjectACL (Maybe ObjectVersionId)
goaVersionId = lens _goaVersionId (\ s a -> s{_goaVersionId = a});

-- | FIXME: Undocumented member.
goaRequestPayer :: Lens' GetObjectACL (Maybe RequestPayer)
goaRequestPayer = lens _goaRequestPayer (\ s a -> s{_goaRequestPayer = a});

-- | FIXME: Undocumented member.
goaBucket :: Lens' GetObjectACL BucketName
goaBucket = lens _goaBucket (\ s a -> s{_goaBucket = a});

-- | FIXME: Undocumented member.
goaKey :: Lens' GetObjectACL ObjectKey
goaKey = lens _goaKey (\ s a -> s{_goaKey = a});

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
          = mconcat ["x-amz-request-payer" =# _goaRequestPayer]

instance ToPath GetObjectACL where
        toPath GetObjectACL'{..}
          = mconcat
              ["/", toText _goaBucket, "/", toText _goaKey]

instance ToQuery GetObjectACL where
        toQuery GetObjectACL'{..}
          = mconcat ["versionId" =: _goaVersionId, "acl"]

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
getObjectACLResponse pStatus_ =
    GetObjectACLResponse'
    { _goarsRequestCharged = Nothing
    , _goarsGrants = Nothing
    , _goarsOwner = Nothing
    , _goarsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
goarsRequestCharged :: Lens' GetObjectACLResponse (Maybe RequestCharged)
goarsRequestCharged = lens _goarsRequestCharged (\ s a -> s{_goarsRequestCharged = a});

-- | A list of grants.
goarsGrants :: Lens' GetObjectACLResponse [Grant]
goarsGrants = lens _goarsGrants (\ s a -> s{_goarsGrants = a}) . _Default . _Coerce;

-- | FIXME: Undocumented member.
goarsOwner :: Lens' GetObjectACLResponse (Maybe Owner)
goarsOwner = lens _goarsOwner (\ s a -> s{_goarsOwner = a});

-- | FIXME: Undocumented member.
goarsStatus :: Lens' GetObjectACLResponse Int
goarsStatus = lens _goarsStatus (\ s a -> s{_goarsStatus = a});
