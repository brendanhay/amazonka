{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.S3.DeleteObjects
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

-- | This operation enables you to delete multiple objects from a bucket
-- using a single HTTP request. You may specify up to 1000 keys.
--
-- <http://docs.aws.amazon.com/AmazonS3/latest/API/DeleteObjects.html>
module Network.AWS.S3.DeleteObjects
    (
    -- * Request
      DeleteObjects
    -- ** Request constructor
    , deleteObjects
    -- ** Request lenses
    , delMFA
    , delRequestPayer
    , delBucket
    , delDelete

    -- * Response
    , DeleteObjectsResponse
    -- ** Response constructor
    , deleteObjectsResponse
    -- ** Response lenses
    , delRequestCharged
    , delDeleted
    , delErrors
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.S3.Types

-- | /See:/ 'deleteObjects' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delMFA'
--
-- * 'delRequestPayer'
--
-- * 'delBucket'
--
-- * 'delDelete'
data DeleteObjects = DeleteObjects'{_delMFA :: Maybe Text, _delRequestPayer :: Maybe RequestPayer, _delBucket :: BucketName, _delDelete :: Delete} deriving (Eq, Read, Show)

-- | 'DeleteObjects' smart constructor.
deleteObjects :: BucketName -> Delete -> DeleteObjects
deleteObjects pBucket pDelete = DeleteObjects'{_delMFA = Nothing, _delRequestPayer = Nothing, _delBucket = pBucket, _delDelete = pDelete};

-- | The concatenation of the authentication device\'s serial number, a
-- space, and the value that is displayed on your authentication device.
delMFA :: Lens' DeleteObjects (Maybe Text)
delMFA = lens _delMFA (\ s a -> s{_delMFA = a});

-- | FIXME: Undocumented member.
delRequestPayer :: Lens' DeleteObjects (Maybe RequestPayer)
delRequestPayer = lens _delRequestPayer (\ s a -> s{_delRequestPayer = a});

-- | FIXME: Undocumented member.
delBucket :: Lens' DeleteObjects BucketName
delBucket = lens _delBucket (\ s a -> s{_delBucket = a});

-- | FIXME: Undocumented member.
delDelete :: Lens' DeleteObjects Delete
delDelete = lens _delDelete (\ s a -> s{_delDelete = a});

instance AWSRequest DeleteObjects where
        type Sv DeleteObjects = S3
        type Rs DeleteObjects = DeleteObjectsResponse
        request = postXML
        response
          = receiveXML
              (\ s h x ->
                 DeleteObjectsResponse' <$>
                   (h .#? "x-amz-request-charged") <*>
                     (may (parseXMLList "Deleted") x)
                     <*> (may (parseXMLList "Error") x))

instance ToElement DeleteObjects where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}Delete"
              .
              _delDelete

instance ToHeaders DeleteObjects where
        toHeaders DeleteObjects'{..}
          = mconcat
              ["x-amz-mfa" =# _delMFA,
               "x-amz-request-payer" =# _delRequestPayer]

instance ToPath DeleteObjects where
        toPath DeleteObjects'{..}
          = mconcat ["/", toText _delBucket]

instance ToQuery DeleteObjects where
        toQuery = const (mconcat ["delete"])

-- | /See:/ 'deleteObjectsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'delRequestCharged'
--
-- * 'delDeleted'
--
-- * 'delErrors'
data DeleteObjectsResponse = DeleteObjectsResponse'{_delRequestCharged :: Maybe RequestCharged, _delDeleted :: Maybe [DeletedObject], _delErrors :: Maybe [S3ServiceError]} deriving (Eq, Read, Show)

-- | 'DeleteObjectsResponse' smart constructor.
deleteObjectsResponse :: DeleteObjectsResponse
deleteObjectsResponse = DeleteObjectsResponse'{_delRequestCharged = Nothing, _delDeleted = Nothing, _delErrors = Nothing};

-- | FIXME: Undocumented member.
delRequestCharged :: Lens' DeleteObjectsResponse (Maybe RequestCharged)
delRequestCharged = lens _delRequestCharged (\ s a -> s{_delRequestCharged = a});

-- | FIXME: Undocumented member.
delDeleted :: Lens' DeleteObjectsResponse [DeletedObject]
delDeleted = lens _delDeleted (\ s a -> s{_delDeleted = a}) . _Default;

-- | FIXME: Undocumented member.
delErrors :: Lens' DeleteObjectsResponse [S3ServiceError]
delErrors = lens _delErrors (\ s a -> s{_delErrors = a}) . _Default;
