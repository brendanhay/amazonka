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
-- Module      : Network.AWS.EC2.CancelImportTask
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels an in-process import virtual machine or import snapshot task.
--
--
module Network.AWS.EC2.CancelImportTask
    (
    -- * Creating a Request
      cancelImportTask
    , CancelImportTask
    -- * Request Lenses
    , citCancelReason
    , citImportTaskId
    , citDryRun

    -- * Destructuring the Response
    , cancelImportTaskResponse
    , CancelImportTaskResponse
    -- * Response Lenses
    , citrsState
    , citrsImportTaskId
    , citrsPreviousState
    , citrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for CancelImportTask.
--
--
--
-- /See:/ 'cancelImportTask' smart constructor.
data CancelImportTask = CancelImportTask'
  { _citCancelReason :: !(Maybe Text)
  , _citImportTaskId :: !(Maybe Text)
  , _citDryRun       :: !(Maybe Bool)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelImportTask' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'citCancelReason' - The reason for canceling the task.
--
-- * 'citImportTaskId' - The ID of the import image or import snapshot task to be canceled.
--
-- * 'citDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
cancelImportTask
    :: CancelImportTask
cancelImportTask =
  CancelImportTask'
    { _citCancelReason = Nothing
    , _citImportTaskId = Nothing
    , _citDryRun = Nothing
    }


-- | The reason for canceling the task.
citCancelReason :: Lens' CancelImportTask (Maybe Text)
citCancelReason = lens _citCancelReason (\ s a -> s{_citCancelReason = a})

-- | The ID of the import image or import snapshot task to be canceled.
citImportTaskId :: Lens' CancelImportTask (Maybe Text)
citImportTaskId = lens _citImportTaskId (\ s a -> s{_citImportTaskId = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
citDryRun :: Lens' CancelImportTask (Maybe Bool)
citDryRun = lens _citDryRun (\ s a -> s{_citDryRun = a})

instance AWSRequest CancelImportTask where
        type Rs CancelImportTask = CancelImportTaskResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 CancelImportTaskResponse' <$>
                   (x .@? "state") <*> (x .@? "importTaskId") <*>
                     (x .@? "previousState")
                     <*> (pure (fromEnum s)))

instance Hashable CancelImportTask where

instance NFData CancelImportTask where

instance ToHeaders CancelImportTask where
        toHeaders = const mempty

instance ToPath CancelImportTask where
        toPath = const "/"

instance ToQuery CancelImportTask where
        toQuery CancelImportTask'{..}
          = mconcat
              ["Action" =: ("CancelImportTask" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "CancelReason" =: _citCancelReason,
               "ImportTaskId" =: _citImportTaskId,
               "DryRun" =: _citDryRun]

-- | Contains the output for CancelImportTask.
--
--
--
-- /See:/ 'cancelImportTaskResponse' smart constructor.
data CancelImportTaskResponse = CancelImportTaskResponse'
  { _citrsState          :: !(Maybe Text)
  , _citrsImportTaskId   :: !(Maybe Text)
  , _citrsPreviousState  :: !(Maybe Text)
  , _citrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'CancelImportTaskResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'citrsState' - The current state of the task being canceled.
--
-- * 'citrsImportTaskId' - The ID of the task being canceled.
--
-- * 'citrsPreviousState' - The current state of the task being canceled.
--
-- * 'citrsResponseStatus' - -- | The response status code.
cancelImportTaskResponse
    :: Int -- ^ 'citrsResponseStatus'
    -> CancelImportTaskResponse
cancelImportTaskResponse pResponseStatus_ =
  CancelImportTaskResponse'
    { _citrsState = Nothing
    , _citrsImportTaskId = Nothing
    , _citrsPreviousState = Nothing
    , _citrsResponseStatus = pResponseStatus_
    }


-- | The current state of the task being canceled.
citrsState :: Lens' CancelImportTaskResponse (Maybe Text)
citrsState = lens _citrsState (\ s a -> s{_citrsState = a})

-- | The ID of the task being canceled.
citrsImportTaskId :: Lens' CancelImportTaskResponse (Maybe Text)
citrsImportTaskId = lens _citrsImportTaskId (\ s a -> s{_citrsImportTaskId = a})

-- | The current state of the task being canceled.
citrsPreviousState :: Lens' CancelImportTaskResponse (Maybe Text)
citrsPreviousState = lens _citrsPreviousState (\ s a -> s{_citrsPreviousState = a})

-- | -- | The response status code.
citrsResponseStatus :: Lens' CancelImportTaskResponse Int
citrsResponseStatus = lens _citrsResponseStatus (\ s a -> s{_citrsResponseStatus = a})

instance NFData CancelImportTaskResponse where
