{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.CancelImportTask
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Cancels an in-process import virtual machine or import snapshot task.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-CancelImportTask.html>
module Network.AWS.EC2.CancelImportTask
    (
    -- * Request
      CancelImportTask
    -- ** Request constructor
    , cancelImportTask
    -- ** Request lenses
    , citrqCancelReason
    , citrqImportTaskId
    , citrqDryRun

    -- * Response
    , CancelImportTaskResponse
    -- ** Response constructor
    , cancelImportTaskResponse
    -- ** Response lenses
    , citrsState
    , citrsImportTaskId
    , citrsPreviousState
    , citrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'cancelImportTask' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'citrqCancelReason'
--
-- * 'citrqImportTaskId'
--
-- * 'citrqDryRun'
data CancelImportTask = CancelImportTask'
    { _citrqCancelReason :: !(Maybe Text)
    , _citrqImportTaskId :: !(Maybe Text)
    , _citrqDryRun       :: !(Maybe Bool)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelImportTask' smart constructor.
cancelImportTask :: CancelImportTask
cancelImportTask =
    CancelImportTask'
    { _citrqCancelReason = Nothing
    , _citrqImportTaskId = Nothing
    , _citrqDryRun = Nothing
    }

-- | The reason for canceling the task.
citrqCancelReason :: Lens' CancelImportTask (Maybe Text)
citrqCancelReason = lens _citrqCancelReason (\ s a -> s{_citrqCancelReason = a});

-- | The ID of the import image or import snapshot task to be canceled.
citrqImportTaskId :: Lens' CancelImportTask (Maybe Text)
citrqImportTaskId = lens _citrqImportTaskId (\ s a -> s{_citrqImportTaskId = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
citrqDryRun :: Lens' CancelImportTask (Maybe Bool)
citrqDryRun = lens _citrqDryRun (\ s a -> s{_citrqDryRun = a});

instance AWSRequest CancelImportTask where
        type Sv CancelImportTask = EC2
        type Rs CancelImportTask = CancelImportTaskResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 CancelImportTaskResponse' <$>
                   (x .@? "state") <*> (x .@? "importTaskId") <*>
                     (x .@? "previousState")
                     <*> (pure (fromEnum s)))

instance ToHeaders CancelImportTask where
        toHeaders = const mempty

instance ToPath CancelImportTask where
        toPath = const "/"

instance ToQuery CancelImportTask where
        toQuery CancelImportTask'{..}
          = mconcat
              ["Action" =: ("CancelImportTask" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "CancelReason" =: _citrqCancelReason,
               "ImportTaskId" =: _citrqImportTaskId,
               "DryRun" =: _citrqDryRun]

-- | /See:/ 'cancelImportTaskResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'citrsState'
--
-- * 'citrsImportTaskId'
--
-- * 'citrsPreviousState'
--
-- * 'citrsStatus'
data CancelImportTaskResponse = CancelImportTaskResponse'
    { _citrsState         :: !(Maybe Text)
    , _citrsImportTaskId  :: !(Maybe Text)
    , _citrsPreviousState :: !(Maybe Text)
    , _citrsStatus        :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'CancelImportTaskResponse' smart constructor.
cancelImportTaskResponse :: Int -> CancelImportTaskResponse
cancelImportTaskResponse pStatus =
    CancelImportTaskResponse'
    { _citrsState = Nothing
    , _citrsImportTaskId = Nothing
    , _citrsPreviousState = Nothing
    , _citrsStatus = pStatus
    }

-- | The current state of the task being canceled.
citrsState :: Lens' CancelImportTaskResponse (Maybe Text)
citrsState = lens _citrsState (\ s a -> s{_citrsState = a});

-- | The ID of the task being canceled.
citrsImportTaskId :: Lens' CancelImportTaskResponse (Maybe Text)
citrsImportTaskId = lens _citrsImportTaskId (\ s a -> s{_citrsImportTaskId = a});

-- | The current state of the task being canceled.
citrsPreviousState :: Lens' CancelImportTaskResponse (Maybe Text)
citrsPreviousState = lens _citrsPreviousState (\ s a -> s{_citrsPreviousState = a});

-- | FIXME: Undocumented member.
citrsStatus :: Lens' CancelImportTaskResponse Int
citrsStatus = lens _citrsStatus (\ s a -> s{_citrsStatus = a});
