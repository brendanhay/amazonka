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
-- Module      : Network.AWS.S3.DeleteObjects
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation enables you to delete multiple objects from a bucket using a single HTTP request. You may specify up to 1000 keys.
module Network.AWS.S3.DeleteObjects
    (
    -- * Creating a Request
      deleteObjects
    , DeleteObjects
    -- * Request Lenses
    , dosMFA
    , dosRequestPayer
    , dosBucket
    , dosDelete

    -- * Destructuring the Response
    , deleteObjectsResponse
    , DeleteObjectsResponse
    -- * Response Lenses
    , drsRequestCharged
    , drsDeleted
    , drsErrors
    , drsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'deleteObjects' smart constructor.
data DeleteObjects = DeleteObjects'
  { _dosMFA          :: !(Maybe Text)
  , _dosRequestPayer :: !(Maybe RequestPayer)
  , _dosBucket       :: !BucketName
  , _dosDelete       :: !Delete
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteObjects' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dosMFA' - The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device.
--
-- * 'dosRequestPayer' - Undocumented member.
--
-- * 'dosBucket' - Undocumented member.
--
-- * 'dosDelete' - Undocumented member.
deleteObjects
    :: BucketName -- ^ 'dosBucket'
    -> Delete -- ^ 'dosDelete'
    -> DeleteObjects
deleteObjects pBucket_ pDelete_ =
  DeleteObjects'
    { _dosMFA = Nothing
    , _dosRequestPayer = Nothing
    , _dosBucket = pBucket_
    , _dosDelete = pDelete_
    }


-- | The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device.
dosMFA :: Lens' DeleteObjects (Maybe Text)
dosMFA = lens _dosMFA (\ s a -> s{_dosMFA = a})

-- | Undocumented member.
dosRequestPayer :: Lens' DeleteObjects (Maybe RequestPayer)
dosRequestPayer = lens _dosRequestPayer (\ s a -> s{_dosRequestPayer = a})

-- | Undocumented member.
dosBucket :: Lens' DeleteObjects BucketName
dosBucket = lens _dosBucket (\ s a -> s{_dosBucket = a})

-- | Undocumented member.
dosDelete :: Lens' DeleteObjects Delete
dosDelete = lens _dosDelete (\ s a -> s{_dosDelete = a})

instance AWSRequest DeleteObjects where
        type Rs DeleteObjects = DeleteObjectsResponse
        request = contentMD5Header . postXML s3
        response
          = receiveXML
              (\ s h x ->
                 DeleteObjectsResponse' <$>
                   (h .#? "x-amz-request-charged") <*>
                     (may (parseXMLList "Deleted") x)
                     <*> (may (parseXMLList "Error") x)
                     <*> (pure (fromEnum s)))

instance Hashable DeleteObjects where

instance NFData DeleteObjects where

instance ToElement DeleteObjects where
        toElement
          = mkElement
              "{http://s3.amazonaws.com/doc/2006-03-01/}Delete"
              .
              _dosDelete

instance ToHeaders DeleteObjects where
        toHeaders DeleteObjects'{..}
          = mconcat
              ["x-amz-mfa" =# _dosMFA,
               "x-amz-request-payer" =# _dosRequestPayer]

instance ToPath DeleteObjects where
        toPath DeleteObjects'{..}
          = mconcat ["/", toBS _dosBucket]

instance ToQuery DeleteObjects where
        toQuery = const (mconcat ["delete"])

-- | /See:/ 'deleteObjectsResponse' smart constructor.
data DeleteObjectsResponse = DeleteObjectsResponse'
  { _drsRequestCharged :: !(Maybe RequestCharged)
  , _drsDeleted        :: !(Maybe [DeletedObject])
  , _drsErrors         :: !(Maybe [S3ServiceError])
  , _drsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteObjectsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drsRequestCharged' - Undocumented member.
--
-- * 'drsDeleted' - Undocumented member.
--
-- * 'drsErrors' - Undocumented member.
--
-- * 'drsResponseStatus' - -- | The response status code.
deleteObjectsResponse
    :: Int -- ^ 'drsResponseStatus'
    -> DeleteObjectsResponse
deleteObjectsResponse pResponseStatus_ =
  DeleteObjectsResponse'
    { _drsRequestCharged = Nothing
    , _drsDeleted = Nothing
    , _drsErrors = Nothing
    , _drsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
drsRequestCharged :: Lens' DeleteObjectsResponse (Maybe RequestCharged)
drsRequestCharged = lens _drsRequestCharged (\ s a -> s{_drsRequestCharged = a})

-- | Undocumented member.
drsDeleted :: Lens' DeleteObjectsResponse [DeletedObject]
drsDeleted = lens _drsDeleted (\ s a -> s{_drsDeleted = a}) . _Default . _Coerce

-- | Undocumented member.
drsErrors :: Lens' DeleteObjectsResponse [S3ServiceError]
drsErrors = lens _drsErrors (\ s a -> s{_drsErrors = a}) . _Default . _Coerce

-- | -- | The response status code.
drsResponseStatus :: Lens' DeleteObjectsResponse Int
drsResponseStatus = lens _drsResponseStatus (\ s a -> s{_drsResponseStatus = a})

instance NFData DeleteObjectsResponse where
