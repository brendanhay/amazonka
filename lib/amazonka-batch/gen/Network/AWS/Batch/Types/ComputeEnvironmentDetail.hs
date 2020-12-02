{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Batch.Types.ComputeEnvironmentDetail
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Batch.Types.ComputeEnvironmentDetail where

import Network.AWS.Batch.Types.CEState
import Network.AWS.Batch.Types.CEStatus
import Network.AWS.Batch.Types.CEType
import Network.AWS.Batch.Types.ComputeResource
import Network.AWS.Lens
import Network.AWS.Prelude

-- | An object representing an AWS Batch compute environment.
--
--
--
-- /See:/ 'computeEnvironmentDetail' smart constructor.
data ComputeEnvironmentDetail = ComputeEnvironmentDetail'
  { _cedStatus ::
      !(Maybe CEStatus),
    _cedState :: !(Maybe CEState),
    _cedComputeResources ::
      !(Maybe ComputeResource),
    _cedStatusReason :: !(Maybe Text),
    _cedType :: !(Maybe CEType),
    _cedServiceRole :: !(Maybe Text),
    _cedTags :: !(Maybe (Map Text (Text))),
    _cedComputeEnvironmentName :: !Text,
    _cedComputeEnvironmentARN :: !Text,
    _cedEcsClusterARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ComputeEnvironmentDetail' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cedStatus' - The current status of the compute environment (for example, @CREATING@ or @VALID@ ).
--
-- * 'cedState' - The state of the compute environment. The valid values are @ENABLED@ or @DISABLED@ . If the state is @ENABLED@ , then the AWS Batch scheduler can attempt to place jobs from an associated job queue on the compute resources within the environment. If the compute environment is managed, then it can scale its instances out or in automatically, based on the job queue demand. If the state is @DISABLED@ , then the AWS Batch scheduler does not attempt to place jobs within the environment. Jobs in a @STARTING@ or @RUNNING@ state continue to progress normally. Managed compute environments in the @DISABLED@ state do not scale out. However, they scale in to @minvCpus@ value after instances become idle.
--
-- * 'cedComputeResources' - The compute resources defined for the compute environment.
--
-- * 'cedStatusReason' - A short, human-readable string to provide additional details about the current status of the compute environment.
--
-- * 'cedType' - The type of the compute environment.
--
-- * 'cedServiceRole' - The service role associated with the compute environment that allows AWS Batch to make calls to AWS API operations on your behalf.
--
-- * 'cedTags' - The tags applied to the compute environment.
--
-- * 'cedComputeEnvironmentName' - The name of the compute environment.
--
-- * 'cedComputeEnvironmentARN' - The Amazon Resource Name (ARN) of the compute environment.
--
-- * 'cedEcsClusterARN' - The Amazon Resource Name (ARN) of the underlying Amazon ECS cluster used by the compute environment.
computeEnvironmentDetail ::
  -- | 'cedComputeEnvironmentName'
  Text ->
  -- | 'cedComputeEnvironmentARN'
  Text ->
  -- | 'cedEcsClusterARN'
  Text ->
  ComputeEnvironmentDetail
computeEnvironmentDetail
  pComputeEnvironmentName_
  pComputeEnvironmentARN_
  pEcsClusterARN_ =
    ComputeEnvironmentDetail'
      { _cedStatus = Nothing,
        _cedState = Nothing,
        _cedComputeResources = Nothing,
        _cedStatusReason = Nothing,
        _cedType = Nothing,
        _cedServiceRole = Nothing,
        _cedTags = Nothing,
        _cedComputeEnvironmentName = pComputeEnvironmentName_,
        _cedComputeEnvironmentARN = pComputeEnvironmentARN_,
        _cedEcsClusterARN = pEcsClusterARN_
      }

-- | The current status of the compute environment (for example, @CREATING@ or @VALID@ ).
cedStatus :: Lens' ComputeEnvironmentDetail (Maybe CEStatus)
cedStatus = lens _cedStatus (\s a -> s {_cedStatus = a})

-- | The state of the compute environment. The valid values are @ENABLED@ or @DISABLED@ . If the state is @ENABLED@ , then the AWS Batch scheduler can attempt to place jobs from an associated job queue on the compute resources within the environment. If the compute environment is managed, then it can scale its instances out or in automatically, based on the job queue demand. If the state is @DISABLED@ , then the AWS Batch scheduler does not attempt to place jobs within the environment. Jobs in a @STARTING@ or @RUNNING@ state continue to progress normally. Managed compute environments in the @DISABLED@ state do not scale out. However, they scale in to @minvCpus@ value after instances become idle.
cedState :: Lens' ComputeEnvironmentDetail (Maybe CEState)
cedState = lens _cedState (\s a -> s {_cedState = a})

-- | The compute resources defined for the compute environment.
cedComputeResources :: Lens' ComputeEnvironmentDetail (Maybe ComputeResource)
cedComputeResources = lens _cedComputeResources (\s a -> s {_cedComputeResources = a})

-- | A short, human-readable string to provide additional details about the current status of the compute environment.
cedStatusReason :: Lens' ComputeEnvironmentDetail (Maybe Text)
cedStatusReason = lens _cedStatusReason (\s a -> s {_cedStatusReason = a})

-- | The type of the compute environment.
cedType :: Lens' ComputeEnvironmentDetail (Maybe CEType)
cedType = lens _cedType (\s a -> s {_cedType = a})

-- | The service role associated with the compute environment that allows AWS Batch to make calls to AWS API operations on your behalf.
cedServiceRole :: Lens' ComputeEnvironmentDetail (Maybe Text)
cedServiceRole = lens _cedServiceRole (\s a -> s {_cedServiceRole = a})

-- | The tags applied to the compute environment.
cedTags :: Lens' ComputeEnvironmentDetail (HashMap Text (Text))
cedTags = lens _cedTags (\s a -> s {_cedTags = a}) . _Default . _Map

-- | The name of the compute environment.
cedComputeEnvironmentName :: Lens' ComputeEnvironmentDetail Text
cedComputeEnvironmentName = lens _cedComputeEnvironmentName (\s a -> s {_cedComputeEnvironmentName = a})

-- | The Amazon Resource Name (ARN) of the compute environment.
cedComputeEnvironmentARN :: Lens' ComputeEnvironmentDetail Text
cedComputeEnvironmentARN = lens _cedComputeEnvironmentARN (\s a -> s {_cedComputeEnvironmentARN = a})

-- | The Amazon Resource Name (ARN) of the underlying Amazon ECS cluster used by the compute environment.
cedEcsClusterARN :: Lens' ComputeEnvironmentDetail Text
cedEcsClusterARN = lens _cedEcsClusterARN (\s a -> s {_cedEcsClusterARN = a})

instance FromJSON ComputeEnvironmentDetail where
  parseJSON =
    withObject
      "ComputeEnvironmentDetail"
      ( \x ->
          ComputeEnvironmentDetail'
            <$> (x .:? "status")
            <*> (x .:? "state")
            <*> (x .:? "computeResources")
            <*> (x .:? "statusReason")
            <*> (x .:? "type")
            <*> (x .:? "serviceRole")
            <*> (x .:? "tags" .!= mempty)
            <*> (x .: "computeEnvironmentName")
            <*> (x .: "computeEnvironmentArn")
            <*> (x .: "ecsClusterArn")
      )

instance Hashable ComputeEnvironmentDetail

instance NFData ComputeEnvironmentDetail
