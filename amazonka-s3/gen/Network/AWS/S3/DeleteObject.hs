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
    , doVersionId
    , doMFA
    , doRequestPayer
    , doBucket
    , doKey

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
-- * 'doVersionId'
--
-- * 'doMFA'
--
-- * 'doRequestPayer'
--
-- * 'doBucket'
--
-- * 'doKey'
data DeleteObject = DeleteObject'
    { _doVersionId    :: !(Maybe ObjectVersionId)
    , _doMFA          :: !(Maybe Text)
    , _doRequestPayer :: !(Maybe RequestPayer)
    , _doBucket       :: !BucketName
    , _doKey          :: !ObjectKey
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteObject' smart constructor.
deleteObject :: BucketName -> ObjectKey -> DeleteObject
deleteObject pBucket_ pKey_ =
    DeleteObject'
    { _doVersionId = Nothing
    , _doMFA = Nothing
    , _doRequestPayer = Nothing
    , _doBucket = pBucket_
    , _doKey = pKey_
    }

-- | VersionId used to reference a specific version of the object.
doVersionId :: Lens' DeleteObject (Maybe ObjectVersionId)
doVersionId = lens _doVersionId (\ s a -> s{_doVersionId = a});

-- | The concatenation of the authentication device\'s serial number, a
-- space, and the value that is displayed on your authentication device.
doMFA :: Lens' DeleteObject (Maybe Text)
doMFA = lens _doMFA (\ s a -> s{_doMFA = a});

-- | FIXME: Undocumented member.
doRequestPayer :: Lens' DeleteObject (Maybe RequestPayer)
doRequestPayer = lens _doRequestPayer (\ s a -> s{_doRequestPayer = a});

-- | FIXME: Undocumented member.
doBucket :: Lens' DeleteObject BucketName
doBucket = lens _doBucket (\ s a -> s{_doBucket = a});

-- | FIXME: Undocumented member.
doKey :: Lens' DeleteObject ObjectKey
doKey = lens _doKey (\ s a -> s{_doKey = a});

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
              ["x-amz-mfa" =# _doMFA,
               "x-amz-request-payer" =# _doRequestPayer]

instance ToPath DeleteObject where
        toPath DeleteObject'{..}
          = mconcat ["/", toText _doBucket, "/", toText _doKey]

instance ToQuery DeleteObject where
        toQuery DeleteObject'{..}
          = mconcat ["versionId" =: _doVersionId]

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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteObjectResponse' smart constructor.
deleteObjectResponse :: Int -> DeleteObjectResponse
deleteObjectResponse pStatus_ =
    DeleteObjectResponse'
    { _dorsVersionId = Nothing
    , _dorsRequestCharged = Nothing
    , _dorsDeleteMarker = Nothing
    , _dorsStatus = pStatus_
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
