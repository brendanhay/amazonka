{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.RestoreObject
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Restores an archived copy of an object back into Amazon S3
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/RestoreObject.html>
module Network.AWS.S3.RestoreObject
    (
    -- * Request
      RestoreObject
    -- ** Request constructor
    , restoreObject
    -- ** Request lenses
    , roVersionId
    , roRequestPayer
    , roRestoreRequest
    , roBucket
    , roKey

    -- * Response
    , RestoreObjectResponse
    -- ** Response constructor
    , restoreObjectResponse
    -- ** Response lenses
    , rorsRequestCharged
    , rorsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'restoreObject' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'roVersionId'
--
-- * 'roRequestPayer'
--
-- * 'roRestoreRequest'
--
-- * 'roBucket'
--
-- * 'roKey'
data RestoreObject = RestoreObject'
    { _roVersionId      :: !(Maybe ObjectVersionId)
    , _roRequestPayer   :: !(Maybe RequestPayer)
    , _roRestoreRequest :: !(Maybe RestoreRequest)
    , _roBucket         :: !BucketName
    , _roKey            :: !ObjectKey
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RestoreObject' smart constructor.
restoreObject :: BucketName -> ObjectKey -> RestoreObject
restoreObject pBucket_ pKey_ =
    RestoreObject'
    { _roVersionId = Nothing
    , _roRequestPayer = Nothing
    , _roRestoreRequest = Nothing
    , _roBucket = pBucket_
    , _roKey = pKey_
    }

-- | FIXME: Undocumented member.
roVersionId :: Lens' RestoreObject (Maybe ObjectVersionId)
roVersionId = lens _roVersionId (\ s a -> s{_roVersionId = a});

-- | FIXME: Undocumented member.
roRequestPayer :: Lens' RestoreObject (Maybe RequestPayer)
roRequestPayer = lens _roRequestPayer (\ s a -> s{_roRequestPayer = a});

-- | FIXME: Undocumented member.
roRestoreRequest :: Lens' RestoreObject (Maybe RestoreRequest)
roRestoreRequest = lens _roRestoreRequest (\ s a -> s{_roRestoreRequest = a});

-- | FIXME: Undocumented member.
roBucket :: Lens' RestoreObject BucketName
roBucket = lens _roBucket (\ s a -> s{_roBucket = a});

-- | FIXME: Undocumented member.
roKey :: Lens' RestoreObject ObjectKey
roKey = lens _roKey (\ s a -> s{_roKey = a});

instance AWSRequest RestoreObject where
        type Sv RestoreObject = S3
        type Rs RestoreObject = RestoreObjectResponse
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 RestoreObjectResponse' <$>
                   (h .#? "x-amz-request-charged") <*>
                     (pure (fromEnum s)))

instance ToElement RestoreObject where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}RestoreRequest"
              .
              _roRestoreRequest

instance ToHeaders RestoreObject where
        toHeaders RestoreObject'{..}
          = mconcat ["x-amz-request-payer" =# _roRequestPayer]

instance ToPath RestoreObject where
        toPath RestoreObject'{..}
          = mconcat ["/", toBS _roBucket, "/", toBS _roKey]

instance ToQuery RestoreObject where
        toQuery RestoreObject'{..}
          = mconcat ["versionId" =: _roVersionId, "restore"]

-- | /See:/ 'restoreObjectResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'rorsRequestCharged'
--
-- * 'rorsStatus'
data RestoreObjectResponse = RestoreObjectResponse'
    { _rorsRequestCharged :: !(Maybe RequestCharged)
    , _rorsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'RestoreObjectResponse' smart constructor.
restoreObjectResponse :: Int -> RestoreObjectResponse
restoreObjectResponse pStatus_ =
    RestoreObjectResponse'
    { _rorsRequestCharged = Nothing
    , _rorsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
rorsRequestCharged :: Lens' RestoreObjectResponse (Maybe RequestCharged)
rorsRequestCharged = lens _rorsRequestCharged (\ s a -> s{_rorsRequestCharged = a});

-- | FIXME: Undocumented member.
rorsStatus :: Lens' RestoreObjectResponse Int
rorsStatus = lens _rorsStatus (\ s a -> s{_rorsStatus = a});
