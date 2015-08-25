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
-- Module      : Network.AWS.S3.DeleteObject
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes the null version (if there is one) of an object and inserts a
-- delete marker, which becomes the latest version of the object. If there
-- isn\'t a null version, Amazon S3 does not remove any objects.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonS3/latest/API/DeleteObject.html AWS API Reference> for DeleteObject.
module Network.AWS.S3.DeleteObject
    (
    -- * Creating a Request
      deleteObject
    , DeleteObject
    -- * Request Lenses
    , doVersionId
    , doMFA
    , doRequestPayer
    , doBucket
    , doKey

    -- * Destructuring the Response
    , deleteObjectResponse
    , DeleteObjectResponse
    -- * Response Lenses
    , dorsVersionId
    , dorsRequestCharged
    , dorsDeleteMarker
    , dorsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types
import           Network.AWS.S3.Types.Product

-- | /See:/ 'deleteObject' smart constructor.
data DeleteObject = DeleteObject'
    { _doVersionId    :: !(Maybe ObjectVersionId)
    , _doMFA          :: !(Maybe Text)
    , _doRequestPayer :: !(Maybe RequestPayer)
    , _doBucket       :: !BucketName
    , _doKey          :: !ObjectKey
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
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
deleteObject
    :: BucketName -- ^ 'doBucket'
    -> ObjectKey -- ^ 'doKey'
    -> DeleteObject
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

-- | Undocumented member.
doRequestPayer :: Lens' DeleteObject (Maybe RequestPayer)
doRequestPayer = lens _doRequestPayer (\ s a -> s{_doRequestPayer = a});

-- | Undocumented member.
doBucket :: Lens' DeleteObject BucketName
doBucket = lens _doBucket (\ s a -> s{_doBucket = a});

-- | Undocumented member.
doKey :: Lens' DeleteObject ObjectKey
doKey = lens _doKey (\ s a -> s{_doKey = a});

instance AWSRequest DeleteObject where
        type Rs DeleteObject = DeleteObjectResponse
        request = delete s3
        response
          = receiveEmpty
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
          = mconcat ["/", toBS _doBucket, "/", toBS _doKey]

instance ToQuery DeleteObject where
        toQuery DeleteObject'{..}
          = mconcat ["versionId" =: _doVersionId]

-- | /See:/ 'deleteObjectResponse' smart constructor.
data DeleteObjectResponse = DeleteObjectResponse'
    { _dorsVersionId      :: !(Maybe ObjectVersionId)
    , _dorsRequestCharged :: !(Maybe RequestCharged)
    , _dorsDeleteMarker   :: !(Maybe Bool)
    , _dorsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dorsVersionId'
--
-- * 'dorsRequestCharged'
--
-- * 'dorsDeleteMarker'
--
-- * 'dorsStatus'
deleteObjectResponse
    :: Int -- ^ 'dorsStatus'
    -> DeleteObjectResponse
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

-- | Undocumented member.
dorsRequestCharged :: Lens' DeleteObjectResponse (Maybe RequestCharged)
dorsRequestCharged = lens _dorsRequestCharged (\ s a -> s{_dorsRequestCharged = a});

-- | Specifies whether the versioned object that was permanently deleted was
-- (true) or was not (false) a delete marker.
dorsDeleteMarker :: Lens' DeleteObjectResponse (Maybe Bool)
dorsDeleteMarker = lens _dorsDeleteMarker (\ s a -> s{_dorsDeleteMarker = a});

-- | The response status code.
dorsStatus :: Lens' DeleteObjectResponse Int
dorsStatus = lens _dorsStatus (\ s a -> s{_dorsStatus = a});
