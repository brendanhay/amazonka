{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.GetObjectACL
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the access control list (ACL) of an object.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/GetObjectACL.html AWS API Reference> for GetObjectACL.
module Network.AWS.S3.GetObjectACL
    (
    -- * Creating a Request
      getObjectACL
    , GetObjectACL
    -- * Request Lenses
    , goaVersionId
    , goaRequestPayer
    , goaBucket
    , goaKey

    -- * Destructuring the Response
    , getObjectACLResponse
    , GetObjectACLResponse
    -- * Response Lenses
    , goarsRequestCharged
    , goarsGrants
    , goarsOwner
    , goarsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types
import           Network.AWS.S3.Types.Product

-- | /See:/ 'getObjectACL' smart constructor.
data GetObjectACL = GetObjectACL'
    { _goaVersionId    :: !(Maybe ObjectVersionId)
    , _goaRequestPayer :: !(Maybe RequestPayer)
    , _goaBucket       :: !BucketName
    , _goaKey          :: !ObjectKey
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetObjectACL' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goaVersionId'
--
-- * 'goaRequestPayer'
--
-- * 'goaBucket'
--
-- * 'goaKey'
getObjectACL
    :: BucketName -- ^ 'goaBucket'
    -> ObjectKey -- ^ 'goaKey'
    -> GetObjectACL
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

-- | Undocumented member.
goaRequestPayer :: Lens' GetObjectACL (Maybe RequestPayer)
goaRequestPayer = lens _goaRequestPayer (\ s a -> s{_goaRequestPayer = a});

-- | Undocumented member.
goaBucket :: Lens' GetObjectACL BucketName
goaBucket = lens _goaBucket (\ s a -> s{_goaBucket = a});

-- | Undocumented member.
goaKey :: Lens' GetObjectACL ObjectKey
goaKey = lens _goaKey (\ s a -> s{_goaKey = a});

instance AWSRequest GetObjectACL where
        type Rs GetObjectACL = GetObjectACLResponse
        request = get s3
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
          = mconcat ["/", toBS _goaBucket, "/", toBS _goaKey]

instance ToQuery GetObjectACL where
        toQuery GetObjectACL'{..}
          = mconcat ["versionId" =: _goaVersionId, "acl"]

-- | /See:/ 'getObjectACLResponse' smart constructor.
data GetObjectACLResponse = GetObjectACLResponse'
    { _goarsRequestCharged :: !(Maybe RequestCharged)
    , _goarsGrants         :: !(Maybe [Grant])
    , _goarsOwner          :: !(Maybe Owner)
    , _goarsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'GetObjectACLResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'goarsRequestCharged'
--
-- * 'goarsGrants'
--
-- * 'goarsOwner'
--
-- * 'goarsStatus'
getObjectACLResponse
    :: Int -- ^ 'goarsStatus'
    -> GetObjectACLResponse
getObjectACLResponse pStatus_ =
    GetObjectACLResponse'
    { _goarsRequestCharged = Nothing
    , _goarsGrants = Nothing
    , _goarsOwner = Nothing
    , _goarsStatus = pStatus_
    }

-- | Undocumented member.
goarsRequestCharged :: Lens' GetObjectACLResponse (Maybe RequestCharged)
goarsRequestCharged = lens _goarsRequestCharged (\ s a -> s{_goarsRequestCharged = a});

-- | A list of grants.
goarsGrants :: Lens' GetObjectACLResponse [Grant]
goarsGrants = lens _goarsGrants (\ s a -> s{_goarsGrants = a}) . _Default . _Coerce;

-- | Undocumented member.
goarsOwner :: Lens' GetObjectACLResponse (Maybe Owner)
goarsOwner = lens _goarsOwner (\ s a -> s{_goarsOwner = a});

-- | The response status code.
goarsStatus :: Lens' GetObjectACLResponse Int
goarsStatus = lens _goarsStatus (\ s a -> s{_goarsStatus = a});
