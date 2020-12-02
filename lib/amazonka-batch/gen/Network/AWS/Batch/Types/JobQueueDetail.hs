{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.JobQueueDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.JobQueueDetail where

import Network.AWS.Batch.Types.ComputeEnvironmentOrder
import Network.AWS.Batch.Types.JQState
import Network.AWS.Batch.Types.JQStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing the details of an AWS Batch job queue.
--
--
--
-- /See:/ 'jobQueueDetail' smart constructor.
data JobQueueDetail = JobQueueDetail'
  { _jqdStatus ::
      !(Maybe JQStatus),
    _jqdStatusReason :: !(Maybe Text),
    _jqdTags :: !(Maybe (Map Text (Text))),
    _jqdJobQueueName :: !Text,
    _jqdJobQueueARN :: !Text,
    _jqdState :: !JQState,
    _jqdPriority :: !Int,
    _jqdComputeEnvironmentOrder :: ![ComputeEnvironmentOrder]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'JobQueueDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'jqdStatus' - The status of the job queue (for example, @CREATING@ or @VALID@ ).
--
-- * 'jqdStatusReason' - A short, human-readable string to provide additional details about the current status of the job queue.
--
-- * 'jqdTags' - The tags applied to the job queue.
--
-- * 'jqdJobQueueName' - The name of the job queue.
--
-- * 'jqdJobQueueARN' - The Amazon Resource Name (ARN) of the job queue.
--
-- * 'jqdState' - Describes the ability of the queue to accept new jobs. If the job queue state is @ENABLED@ , it is able to accept jobs. If the job queue state is @DISABLED@ , new jobs cannot be added to the queue, but jobs already in the queue can finish.
--
-- * 'jqdPriority' - The priority of the job queue.
--
-- * 'jqdComputeEnvironmentOrder' - The compute environments that are attached to the job queue and the order in which job placement is preferred. Compute environments are selected for job placement in ascending order.
jobQueueDetail ::
  -- | 'jqdJobQueueName'
  Text ->
  -- | 'jqdJobQueueARN'
  Text ->
  -- | 'jqdState'
  JQState ->
  -- | 'jqdPriority'
  Int ->
  JobQueueDetail
jobQueueDetail pJobQueueName_ pJobQueueARN_ pState_ pPriority_ =
  JobQueueDetail'
    { _jqdStatus = Nothing,
      _jqdStatusReason = Nothing,
      _jqdTags = Nothing,
      _jqdJobQueueName = pJobQueueName_,
      _jqdJobQueueARN = pJobQueueARN_,
      _jqdState = pState_,
      _jqdPriority = pPriority_,
      _jqdComputeEnvironmentOrder = mempty
    }

-- | The status of the job queue (for example, @CREATING@ or @VALID@ ).
jqdStatus :: Lens' JobQueueDetail (Maybe JQStatus)
jqdStatus = lens _jqdStatus (\s a -> s {_jqdStatus = a})

-- | A short, human-readable string to provide additional details about the current status of the job queue.
jqdStatusReason :: Lens' JobQueueDetail (Maybe Text)
jqdStatusReason = lens _jqdStatusReason (\s a -> s {_jqdStatusReason = a})

-- | The tags applied to the job queue.
jqdTags :: Lens' JobQueueDetail (HashMap Text (Text))
jqdTags = lens _jqdTags (\s a -> s {_jqdTags = a}) . _Default . _Map

-- | The name of the job queue.
jqdJobQueueName :: Lens' JobQueueDetail Text
jqdJobQueueName = lens _jqdJobQueueName (\s a -> s {_jqdJobQueueName = a})

-- | The Amazon Resource Name (ARN) of the job queue.
jqdJobQueueARN :: Lens' JobQueueDetail Text
jqdJobQueueARN = lens _jqdJobQueueARN (\s a -> s {_jqdJobQueueARN = a})

-- | Describes the ability of the queue to accept new jobs. If the job queue state is @ENABLED@ , it is able to accept jobs. If the job queue state is @DISABLED@ , new jobs cannot be added to the queue, but jobs already in the queue can finish.
jqdState :: Lens' JobQueueDetail JQState
jqdState = lens _jqdState (\s a -> s {_jqdState = a})

-- | The priority of the job queue.
jqdPriority :: Lens' JobQueueDetail Int
jqdPriority = lens _jqdPriority (\s a -> s {_jqdPriority = a})

-- | The compute environments that are attached to the job queue and the order in which job placement is preferred. Compute environments are selected for job placement in ascending order.
jqdComputeEnvironmentOrder :: Lens' JobQueueDetail [ComputeEnvironmentOrder]
jqdComputeEnvironmentOrder = lens _jqdComputeEnvironmentOrder (\s a -> s {_jqdComputeEnvironmentOrder = a}) . _Coerce

instance FromJSON JobQueueDetail where
  parseJSON =
    withObject
      "JobQueueDetail"
      ( \x ->
          JobQueueDetail'
            <$> (x .:? "status")
            <*> (x .:? "statusReason")
            <*> (x .:? "tags" .!= mempty)
            <*> (x .: "jobQueueName")
            <*> (x .: "jobQueueArn")
            <*> (x .: "state")
            <*> (x .: "priority")
            <*> (x .:? "computeEnvironmentOrder" .!= mempty)
      )

instance Hashable JobQueueDetail

instance NFData JobQueueDetail
