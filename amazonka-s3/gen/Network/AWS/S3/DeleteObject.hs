{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.S3.DeleteObject
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Removes the null version (if there is one) of an object and inserts a
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
    , dorVersionId
    , dorRequestCharged
    , dorDeleteMarker
    , dorStatusCode
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types

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
data DeleteObject = DeleteObject'{_doVersionId :: Maybe ObjectVersionId, _doMFA :: Maybe Text, _doRequestPayer :: Maybe RequestPayer, _doBucket :: BucketName, _doKey :: ObjectKey} deriving (Eq, Read, Show)

-- | 'DeleteObject' smart constructor.
deleteObject :: BucketName -> ObjectKey -> DeleteObject
deleteObject pBucket pKey = DeleteObject'{_doVersionId = Nothing, _doMFA = Nothing, _doRequestPayer = Nothing, _doBucket = pBucket, _doKey = pKey};

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
-- * 'dorVersionId'
--
-- * 'dorRequestCharged'
--
-- * 'dorDeleteMarker'
--
-- * 'dorStatusCode'
data DeleteObjectResponse = DeleteObjectResponse'{_dorVersionId :: Maybe ObjectVersionId, _dorRequestCharged :: Maybe RequestCharged, _dorDeleteMarker :: Maybe Bool, _dorStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'DeleteObjectResponse' smart constructor.
deleteObjectResponse :: Int -> DeleteObjectResponse
deleteObjectResponse pStatusCode = DeleteObjectResponse'{_dorVersionId = Nothing, _dorRequestCharged = Nothing, _dorDeleteMarker = Nothing, _dorStatusCode = pStatusCode};

-- | Returns the version ID of the delete marker created as a result of the
-- DELETE operation.
dorVersionId :: Lens' DeleteObjectResponse (Maybe ObjectVersionId)
dorVersionId = lens _dorVersionId (\ s a -> s{_dorVersionId = a});

-- | FIXME: Undocumented member.
dorRequestCharged :: Lens' DeleteObjectResponse (Maybe RequestCharged)
dorRequestCharged = lens _dorRequestCharged (\ s a -> s{_dorRequestCharged = a});

-- | Specifies whether the versioned object that was permanently deleted was
-- (true) or was not (false) a delete marker.
dorDeleteMarker :: Lens' DeleteObjectResponse (Maybe Bool)
dorDeleteMarker = lens _dorDeleteMarker (\ s a -> s{_dorDeleteMarker = a});

-- | FIXME: Undocumented member.
dorStatusCode :: Lens' DeleteObjectResponse Int
dorStatusCode = lens _dorStatusCode (\ s a -> s{_dorStatusCode = a});
