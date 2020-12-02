{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.Types.RemediationExecutionStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Config.Types.RemediationExecutionStatus where

import Network.AWS.Config.Types.RemediationExecutionState
import Network.AWS.Config.Types.RemediationExecutionStep
import Network.AWS.Config.Types.ResourceKey
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides details of the current status of the invoked remediation action for that resource.
--
--
--
-- /See:/ 'remediationExecutionStatus' smart constructor.
data RemediationExecutionStatus = RemediationExecutionStatus'
  { _rState ::
      !(Maybe RemediationExecutionState),
    _rLastUpdatedTime :: !(Maybe POSIX),
    _rResourceKey :: !(Maybe ResourceKey),
    _rStepDetails ::
      !(Maybe [RemediationExecutionStep]),
    _rInvocationTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RemediationExecutionStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rState' - ENUM of the values.
--
-- * 'rLastUpdatedTime' - The time when the remediation execution was last updated.
--
-- * 'rResourceKey' - Undocumented member.
--
-- * 'rStepDetails' - Details of every step.
--
-- * 'rInvocationTime' - Start time when the remediation was executed.
remediationExecutionStatus ::
  RemediationExecutionStatus
remediationExecutionStatus =
  RemediationExecutionStatus'
    { _rState = Nothing,
      _rLastUpdatedTime = Nothing,
      _rResourceKey = Nothing,
      _rStepDetails = Nothing,
      _rInvocationTime = Nothing
    }

-- | ENUM of the values.
rState :: Lens' RemediationExecutionStatus (Maybe RemediationExecutionState)
rState = lens _rState (\s a -> s {_rState = a})

-- | The time when the remediation execution was last updated.
rLastUpdatedTime :: Lens' RemediationExecutionStatus (Maybe UTCTime)
rLastUpdatedTime = lens _rLastUpdatedTime (\s a -> s {_rLastUpdatedTime = a}) . mapping _Time

-- | Undocumented member.
rResourceKey :: Lens' RemediationExecutionStatus (Maybe ResourceKey)
rResourceKey = lens _rResourceKey (\s a -> s {_rResourceKey = a})

-- | Details of every step.
rStepDetails :: Lens' RemediationExecutionStatus [RemediationExecutionStep]
rStepDetails = lens _rStepDetails (\s a -> s {_rStepDetails = a}) . _Default . _Coerce

-- | Start time when the remediation was executed.
rInvocationTime :: Lens' RemediationExecutionStatus (Maybe UTCTime)
rInvocationTime = lens _rInvocationTime (\s a -> s {_rInvocationTime = a}) . mapping _Time

instance FromJSON RemediationExecutionStatus where
  parseJSON =
    withObject
      "RemediationExecutionStatus"
      ( \x ->
          RemediationExecutionStatus'
            <$> (x .:? "State")
            <*> (x .:? "LastUpdatedTime")
            <*> (x .:? "ResourceKey")
            <*> (x .:? "StepDetails" .!= mempty)
            <*> (x .:? "InvocationTime")
      )

instance Hashable RemediationExecutionStatus

instance NFData RemediationExecutionStatus
