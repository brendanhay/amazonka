{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.DeleteObjects
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation enables you to delete multiple objects from a bucket
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
    , drqMFA
    , drqRequestPayer
    , drqBucket
    , drqDelete

    -- * Response
    , DeleteObjectsResponse
    -- ** Response constructor
    , deleteObjectsResponse
    -- ** Response lenses
    , drsRequestCharged
    , drsDeleted
    , drsErrors
    , drsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.S3.Types

-- | /See:/ 'deleteObjects' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drqMFA'
--
-- * 'drqRequestPayer'
--
-- * 'drqBucket'
--
-- * 'drqDelete'
data DeleteObjects = DeleteObjects'
    { _drqMFA          :: !(Maybe Text)
    , _drqRequestPayer :: !(Maybe RequestPayer)
    , _drqBucket       :: !BucketName
    , _drqDelete       :: !Delete
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'DeleteObjects' smart constructor.
deleteObjects :: BucketName -> Delete -> DeleteObjects
deleteObjects pBucket_ pDelete_ =
    DeleteObjects'
    { _drqMFA = Nothing
    , _drqRequestPayer = Nothing
    , _drqBucket = pBucket_
    , _drqDelete = pDelete_
    }

-- | The concatenation of the authentication device\'s serial number, a
-- space, and the value that is displayed on your authentication device.
drqMFA :: Lens' DeleteObjects (Maybe Text)
drqMFA = lens _drqMFA (\ s a -> s{_drqMFA = a});

-- | FIXME: Undocumented member.
drqRequestPayer :: Lens' DeleteObjects (Maybe RequestPayer)
drqRequestPayer = lens _drqRequestPayer (\ s a -> s{_drqRequestPayer = a});

-- | FIXME: Undocumented member.
drqBucket :: Lens' DeleteObjects BucketName
drqBucket = lens _drqBucket (\ s a -> s{_drqBucket = a});

-- | FIXME: Undocumented member.
drqDelete :: Lens' DeleteObjects Delete
drqDelete = lens _drqDelete (\ s a -> s{_drqDelete = a});

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
                     <*> (may (parseXMLList "Error") x)
                     <*> (pure (fromEnum s)))

instance ToElement DeleteObjects where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}Delete"
              .
              _drqDelete

instance ToHeaders DeleteObjects where
        toHeaders DeleteObjects'{..}
          = mconcat
              ["x-amz-mfa" =# _drqMFA,
               "x-amz-request-payer" =# _drqRequestPayer]

instance ToPath DeleteObjects where
        toPath DeleteObjects'{..}
          = mconcat ["/", toText _drqBucket]

instance ToQuery DeleteObjects where
        toQuery = const (mconcat ["delete"])

-- | /See:/ 'deleteObjectsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drsRequestCharged'
--
-- * 'drsDeleted'
--
-- * 'drsErrors'
--
-- * 'drsStatus'
data DeleteObjectsResponse = DeleteObjectsResponse'
    { _drsRequestCharged :: !(Maybe RequestCharged)
    , _drsDeleted        :: !(Maybe [DeletedObject])
    , _drsErrors         :: !(Maybe [S3ServiceError])
    , _drsStatus         :: !Int
    } deriving (Eq,Show,Data,Typeable,Generic)

-- | 'DeleteObjectsResponse' smart constructor.
deleteObjectsResponse :: Int -> DeleteObjectsResponse
deleteObjectsResponse pStatus_ =
    DeleteObjectsResponse'
    { _drsRequestCharged = Nothing
    , _drsDeleted = Nothing
    , _drsErrors = Nothing
    , _drsStatus = pStatus_
    }

-- | FIXME: Undocumented member.
drsRequestCharged :: Lens' DeleteObjectsResponse (Maybe RequestCharged)
drsRequestCharged = lens _drsRequestCharged (\ s a -> s{_drsRequestCharged = a});

-- | FIXME: Undocumented member.
drsDeleted :: Lens' DeleteObjectsResponse [DeletedObject]
drsDeleted = lens _drsDeleted (\ s a -> s{_drsDeleted = a}) . _Default;

-- | FIXME: Undocumented member.
drsErrors :: Lens' DeleteObjectsResponse [S3ServiceError]
drsErrors = lens _drsErrors (\ s a -> s{_drsErrors = a}) . _Default;

-- | FIXME: Undocumented member.
drsStatus :: Lens' DeleteObjectsResponse Int
drsStatus = lens _drsStatus (\ s a -> s{_drsStatus = a});
