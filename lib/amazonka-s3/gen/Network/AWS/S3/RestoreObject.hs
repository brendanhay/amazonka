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
-- Module      : Network.AWS.S3.RestoreObject
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores an archived copy of an object back into Amazon S3
module Network.AWS.S3.RestoreObject
    (
    -- * Creating a Request
      restoreObject
    , RestoreObject
    -- * Request Lenses
    , roVersionId
    , roRequestPayer
    , roRestoreRequest
    , roBucket
    , roKey

    -- * Destructuring the Response
    , restoreObjectResponse
    , RestoreObjectResponse
    -- * Response Lenses
    , rorsRequestCharged
    , rorsRestoreOutputPath
    , rorsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.S3.Types
import Network.AWS.S3.Types.Product

-- | /See:/ 'restoreObject' smart constructor.
data RestoreObject = RestoreObject'
  { _roVersionId      :: !(Maybe ObjectVersionId)
  , _roRequestPayer   :: !(Maybe RequestPayer)
  , _roRestoreRequest :: !(Maybe RestoreRequest)
  , _roBucket         :: !BucketName
  , _roKey            :: !ObjectKey
  } deriving (Eq, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreObject' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'roVersionId' - Undocumented member.
--
-- * 'roRequestPayer' - Undocumented member.
--
-- * 'roRestoreRequest' - Undocumented member.
--
-- * 'roBucket' - Undocumented member.
--
-- * 'roKey' - Undocumented member.
restoreObject
    :: BucketName -- ^ 'roBucket'
    -> ObjectKey -- ^ 'roKey'
    -> RestoreObject
restoreObject pBucket_ pKey_ =
  RestoreObject'
    { _roVersionId = Nothing
    , _roRequestPayer = Nothing
    , _roRestoreRequest = Nothing
    , _roBucket = pBucket_
    , _roKey = pKey_
    }


-- | Undocumented member.
roVersionId :: Lens' RestoreObject (Maybe ObjectVersionId)
roVersionId = lens _roVersionId (\ s a -> s{_roVersionId = a})

-- | Undocumented member.
roRequestPayer :: Lens' RestoreObject (Maybe RequestPayer)
roRequestPayer = lens _roRequestPayer (\ s a -> s{_roRequestPayer = a})

-- | Undocumented member.
roRestoreRequest :: Lens' RestoreObject (Maybe RestoreRequest)
roRestoreRequest = lens _roRestoreRequest (\ s a -> s{_roRestoreRequest = a})

-- | Undocumented member.
roBucket :: Lens' RestoreObject BucketName
roBucket = lens _roBucket (\ s a -> s{_roBucket = a})

-- | Undocumented member.
roKey :: Lens' RestoreObject ObjectKey
roKey = lens _roKey (\ s a -> s{_roKey = a})

instance AWSRequest RestoreObject where
        type Rs RestoreObject = RestoreObjectResponse
        request = postXML s3
        response
          = receiveEmpty
              (\ s h x ->
                 RestoreObjectResponse' <$>
                   (h .#? "x-amz-request-charged") <*>
                     (h .#? "x-amz-restore-output-path")
                     <*> (pure (fromEnum s)))

instance Hashable RestoreObject where

instance NFData RestoreObject where

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
data RestoreObjectResponse = RestoreObjectResponse'
  { _rorsRequestCharged    :: !(Maybe RequestCharged)
  , _rorsRestoreOutputPath :: !(Maybe Text)
  , _rorsResponseStatus    :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'RestoreObjectResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rorsRequestCharged' - Undocumented member.
--
-- * 'rorsRestoreOutputPath' - Indicates the path in the provided S3 output location where Select results will be restored to.
--
-- * 'rorsResponseStatus' - -- | The response status code.
restoreObjectResponse
    :: Int -- ^ 'rorsResponseStatus'
    -> RestoreObjectResponse
restoreObjectResponse pResponseStatus_ =
  RestoreObjectResponse'
    { _rorsRequestCharged = Nothing
    , _rorsRestoreOutputPath = Nothing
    , _rorsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
rorsRequestCharged :: Lens' RestoreObjectResponse (Maybe RequestCharged)
rorsRequestCharged = lens _rorsRequestCharged (\ s a -> s{_rorsRequestCharged = a})

-- | Indicates the path in the provided S3 output location where Select results will be restored to.
rorsRestoreOutputPath :: Lens' RestoreObjectResponse (Maybe Text)
rorsRestoreOutputPath = lens _rorsRestoreOutputPath (\ s a -> s{_rorsRestoreOutputPath = a})

-- | -- | The response status code.
rorsResponseStatus :: Lens' RestoreObjectResponse Int
rorsResponseStatus = lens _rorsResponseStatus (\ s a -> s{_rorsResponseStatus = a})

instance NFData RestoreObjectResponse where
