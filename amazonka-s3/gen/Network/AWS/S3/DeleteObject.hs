{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteObject
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Removes the null version (if there is one) of an object and inserts a
-- delete marker, which becomes the latest version of the object. If there
-- isn\'t a null version, Amazon S3 does not remove any objects.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/DeleteObject.html>
module Network.AWS.S3.DeleteObject
    (
    -- * Request
      DeleteObject
    -- ** Request constructor
    , deleteObject
    -- ** Request lenses
    , dorqVersionId
    , dorqMFA
    , dorqRequestPayer
    , dorqBucket
    , dorqKey

    -- * Response
    , DeleteObjectResponse
    -- ** Response constructor
    , deleteObjectResponse
    -- ** Response lenses
    , dorsVersionId
    , dorsRequestCharged
    , dorsDeleteMarker
    , dorsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'deleteObject' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dorqVersionId'
--
-- * 'dorqMFA'
--
-- * 'dorqRequestPayer'
--
-- * 'dorqBucket'
--
-- * 'dorqKey'
data DeleteObject = DeleteObject'
    { _dorqVersionId    :: !(Maybe ObjectVersionId)
    , _dorqMFA          :: !(Maybe Text)
    , _dorqRequestPayer :: !(Maybe RequestPayer)
    , _dorqBucket       :: !BucketName
    , _dorqKey          :: !ObjectKey
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'DeleteObject' smart constructor.
deleteObject :: BucketName -> ObjectKey -> DeleteObject
deleteObject pBucket pKey =
    DeleteObject'
    { _dorqVersionId = Nothing
    , _dorqMFA = Nothing
    , _dorqRequestPayer = Nothing
    , _dorqBucket = pBucket
    , _dorqKey = pKey
    }

-- | VersionId used to reference a specific version of the object.
dorqVersionId :: Lens' DeleteObject (Maybe ObjectVersionId)
dorqVersionId = lens _dorqVersionId (\ s a -> s{_dorqVersionId = a});

-- | The concatenation of the authentication device\'s serial number, a
-- space, and the value that is displayed on your authentication device.
dorqMFA :: Lens' DeleteObject (Maybe Text)
dorqMFA = lens _dorqMFA (\ s a -> s{_dorqMFA = a});

-- | FIXME: Undocumented member.
dorqRequestPayer :: Lens' DeleteObject (Maybe RequestPayer)
dorqRequestPayer = lens _dorqRequestPayer (\ s a -> s{_dorqRequestPayer = a});

-- | FIXME: Undocumented member.
dorqBucket :: Lens' DeleteObject BucketName
dorqBucket = lens _dorqBucket (\ s a -> s{_dorqBucket = a});

-- | FIXME: Undocumented member.
dorqKey :: Lens' DeleteObject ObjectKey
dorqKey = lens _dorqKey (\ s a -> s{_dorqKey = a});

instance AWSRequest DeleteObject where
        type Sv DeleteObject = S3
        type Rs DeleteObject = DeleteObjectResponse
        request = delete
        response
          = receiveXML
              (\ s h x ->
                 DeleteObjectResponse' <$>
                   (h .#? "x-amz-version-id") <*>
                     (h .#? "x-amz-request-charged")
                     <*> (h .#? "x-amz-delete-marker")
                     <*> (pure (fromEnum s)))

instance ToHeaders DeleteObject where
        toHeaders DeleteObject'{..}
          = mconcat
              ["x-amz-mfa" =# _dorqMFA,
               "x-amz-request-payer" =# _dorqRequestPayer]

instance ToPath DeleteObject where
        toPath DeleteObject'{..}
          = mconcat
              ["/", toText _dorqBucket, "/", toText _dorqKey]

instance ToQuery DeleteObject where
        toQuery DeleteObject'{..}
          = mconcat ["versionId" =: _dorqVersionId]

-- | /See:/ 'deleteObjectResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dorsVersionId'
--
-- * 'dorsRequestCharged'
--
-- * 'dorsDeleteMarker'
--
-- * 'dorsStatus'
data DeleteObjectResponse = DeleteObjectResponse'
    { _dorsVersionId      :: !(Maybe ObjectVersionId)
    , _dorsRequestCharged :: !(Maybe RequestCharged)
    , _dorsDeleteMarker   :: !(Maybe Bool)
    , _dorsStatus         :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'DeleteObjectResponse' smart constructor.
deleteObjectResponse :: Int -> DeleteObjectResponse
deleteObjectResponse pStatus =
    DeleteObjectResponse'
    { _dorsVersionId = Nothing
    , _dorsRequestCharged = Nothing
    , _dorsDeleteMarker = Nothing
    , _dorsStatus = pStatus
    }

-- | Returns the version ID of the delete marker created as a result of the
-- DELETE operation.
dorsVersionId :: Lens' DeleteObjectResponse (Maybe ObjectVersionId)
dorsVersionId = lens _dorsVersionId (\ s a -> s{_dorsVersionId = a});

-- | FIXME: Undocumented member.
dorsRequestCharged :: Lens' DeleteObjectResponse (Maybe RequestCharged)
dorsRequestCharged = lens _dorsRequestCharged (\ s a -> s{_dorsRequestCharged = a});

-- | Specifies whether the versioned object that was permanently deleted was
-- (true) or was not (false) a delete marker.
dorsDeleteMarker :: Lens' DeleteObjectResponse (Maybe Bool)
dorsDeleteMarker = lens _dorsDeleteMarker (\ s a -> s{_dorsDeleteMarker = a});

-- | FIXME: Undocumented member.
dorsStatus :: Lens' DeleteObjectResponse Int
dorsStatus = lens _dorsStatus (\ s a -> s{_dorsStatus = a});
