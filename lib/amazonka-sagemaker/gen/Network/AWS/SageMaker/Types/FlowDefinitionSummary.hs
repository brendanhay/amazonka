{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.FlowDefinitionSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.FlowDefinitionSummary where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.Types.FlowDefinitionStatus

-- | Contains summary information about the flow definition.
--
--
--
-- /See:/ 'flowDefinitionSummary' smart constructor.
data FlowDefinitionSummary = FlowDefinitionSummary'
  { _fdsFailureReason ::
      !(Maybe Text),
    _fdsFlowDefinitionName :: !Text,
    _fdsFlowDefinitionARN :: !Text,
    _fdsFlowDefinitionStatus ::
      !FlowDefinitionStatus,
    _fdsCreationTime :: !POSIX
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'FlowDefinitionSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'fdsFailureReason' - The reason why the flow definition creation failed. A failure reason is returned only when the flow definition status is @Failed@ .
--
-- * 'fdsFlowDefinitionName' - The name of the flow definition.
--
-- * 'fdsFlowDefinitionARN' - The Amazon Resource Name (ARN) of the flow definition.
--
-- * 'fdsFlowDefinitionStatus' - The status of the flow definition. Valid values:
--
-- * 'fdsCreationTime' - The timestamp when SageMaker created the flow definition.
flowDefinitionSummary ::
  -- | 'fdsFlowDefinitionName'
  Text ->
  -- | 'fdsFlowDefinitionARN'
  Text ->
  -- | 'fdsFlowDefinitionStatus'
  FlowDefinitionStatus ->
  -- | 'fdsCreationTime'
  UTCTime ->
  FlowDefinitionSummary
flowDefinitionSummary
  pFlowDefinitionName_
  pFlowDefinitionARN_
  pFlowDefinitionStatus_
  pCreationTime_ =
    FlowDefinitionSummary'
      { _fdsFailureReason = Nothing,
        _fdsFlowDefinitionName = pFlowDefinitionName_,
        _fdsFlowDefinitionARN = pFlowDefinitionARN_,
        _fdsFlowDefinitionStatus = pFlowDefinitionStatus_,
        _fdsCreationTime = _Time # pCreationTime_
      }

-- | The reason why the flow definition creation failed. A failure reason is returned only when the flow definition status is @Failed@ .
fdsFailureReason :: Lens' FlowDefinitionSummary (Maybe Text)
fdsFailureReason = lens _fdsFailureReason (\s a -> s {_fdsFailureReason = a})

-- | The name of the flow definition.
fdsFlowDefinitionName :: Lens' FlowDefinitionSummary Text
fdsFlowDefinitionName = lens _fdsFlowDefinitionName (\s a -> s {_fdsFlowDefinitionName = a})

-- | The Amazon Resource Name (ARN) of the flow definition.
fdsFlowDefinitionARN :: Lens' FlowDefinitionSummary Text
fdsFlowDefinitionARN = lens _fdsFlowDefinitionARN (\s a -> s {_fdsFlowDefinitionARN = a})

-- | The status of the flow definition. Valid values:
fdsFlowDefinitionStatus :: Lens' FlowDefinitionSummary FlowDefinitionStatus
fdsFlowDefinitionStatus = lens _fdsFlowDefinitionStatus (\s a -> s {_fdsFlowDefinitionStatus = a})

-- | The timestamp when SageMaker created the flow definition.
fdsCreationTime :: Lens' FlowDefinitionSummary UTCTime
fdsCreationTime = lens _fdsCreationTime (\s a -> s {_fdsCreationTime = a}) . _Time

instance FromJSON FlowDefinitionSummary where
  parseJSON =
    withObject
      "FlowDefinitionSummary"
      ( \x ->
          FlowDefinitionSummary'
            <$> (x .:? "FailureReason")
            <*> (x .: "FlowDefinitionName")
            <*> (x .: "FlowDefinitionArn")
            <*> (x .: "FlowDefinitionStatus")
            <*> (x .: "CreationTime")
      )

instance Hashable FlowDefinitionSummary

instance NFData FlowDefinitionSummary
