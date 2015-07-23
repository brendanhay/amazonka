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
    , rorqVersionId
    , rorqRequestPayer
    , rorqRestoreRequest
    , rorqBucket
    , rorqKey

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
-- * 'rorqVersionId'
--
-- * 'rorqRequestPayer'
--
-- * 'rorqRestoreRequest'
--
-- * 'rorqBucket'
--
-- * 'rorqKey'
data RestoreObject = RestoreObject'
    { _rorqVersionId      :: !(Maybe ObjectVersionId)
    , _rorqRequestPayer   :: !(Maybe RequestPayer)
    , _rorqRestoreRequest :: !(Maybe RestoreRequest)
    , _rorqBucket         :: !BucketName
    , _rorqKey            :: !ObjectKey
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'RestoreObject' smart constructor.
restoreObject :: BucketName -> ObjectKey -> RestoreObject
restoreObject pBucket_ pKey_ =
    RestoreObject'
    { _rorqVersionId = Nothing
    , _rorqRequestPayer = Nothing
    , _rorqRestoreRequest = Nothing
    , _rorqBucket = pBucket_
    , _rorqKey = pKey_
    }

-- | FIXME: Undocumented member.
rorqVersionId :: Lens' RestoreObject (Maybe ObjectVersionId)
rorqVersionId = lens _rorqVersionId (\ s a -> s{_rorqVersionId = a});

-- | FIXME: Undocumented member.
rorqRequestPayer :: Lens' RestoreObject (Maybe RequestPayer)
rorqRequestPayer = lens _rorqRequestPayer (\ s a -> s{_rorqRequestPayer = a});

-- | FIXME: Undocumented member.
rorqRestoreRequest :: Lens' RestoreObject (Maybe RestoreRequest)
rorqRestoreRequest = lens _rorqRestoreRequest (\ s a -> s{_rorqRestoreRequest = a});

-- | FIXME: Undocumented member.
rorqBucket :: Lens' RestoreObject BucketName
rorqBucket = lens _rorqBucket (\ s a -> s{_rorqBucket = a});

-- | FIXME: Undocumented member.
rorqKey :: Lens' RestoreObject ObjectKey
rorqKey = lens _rorqKey (\ s a -> s{_rorqKey = a});

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
              _rorqRestoreRequest

instance ToHeaders RestoreObject where
        toHeaders RestoreObject'{..}
          = mconcat
              ["x-amz-request-payer" =# _rorqRequestPayer]

instance ToPath RestoreObject where
        toPath RestoreObject'{..}
          = mconcat
              ["/", toText _rorqBucket, "/", toText _rorqKey]

instance ToQuery RestoreObject where
        toQuery RestoreObject'{..}
          = mconcat ["versionId" =: _rorqVersionId, "restore"]

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
