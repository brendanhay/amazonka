{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepSummary where

import Network.AWS.EMR.Types.ActionOnFailure
import Network.AWS.EMR.Types.HadoopStepConfig
import Network.AWS.EMR.Types.StepStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The summary of the cluster step.
--
--
--
-- /See:/ 'stepSummary' smart constructor.
data StepSummary = StepSummary'
  { _steStatus :: !(Maybe StepStatus),
    _steActionOnFailure :: !(Maybe ActionOnFailure),
    _steConfig :: !(Maybe HadoopStepConfig),
    _steName :: !(Maybe Text),
    _steId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StepSummary' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'steStatus' - The current execution status details of the cluster step.
--
-- * 'steActionOnFailure' - The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is available for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
--
-- * 'steConfig' - The Hadoop job configuration of the cluster step.
--
-- * 'steName' - The name of the cluster step.
--
-- * 'steId' - The identifier of the cluster step.
stepSummary ::
  StepSummary
stepSummary =
  StepSummary'
    { _steStatus = Nothing,
      _steActionOnFailure = Nothing,
      _steConfig = Nothing,
      _steName = Nothing,
      _steId = Nothing
    }

-- | The current execution status details of the cluster step.
steStatus :: Lens' StepSummary (Maybe StepStatus)
steStatus = lens _steStatus (\s a -> s {_steStatus = a})

-- | The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is available for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
steActionOnFailure :: Lens' StepSummary (Maybe ActionOnFailure)
steActionOnFailure = lens _steActionOnFailure (\s a -> s {_steActionOnFailure = a})

-- | The Hadoop job configuration of the cluster step.
steConfig :: Lens' StepSummary (Maybe HadoopStepConfig)
steConfig = lens _steConfig (\s a -> s {_steConfig = a})

-- | The name of the cluster step.
steName :: Lens' StepSummary (Maybe Text)
steName = lens _steName (\s a -> s {_steName = a})

-- | The identifier of the cluster step.
steId :: Lens' StepSummary (Maybe Text)
steId = lens _steId (\s a -> s {_steId = a})

instance FromJSON StepSummary where
  parseJSON =
    withObject
      "StepSummary"
      ( \x ->
          StepSummary'
            <$> (x .:? "Status")
            <*> (x .:? "ActionOnFailure")
            <*> (x .:? "Config")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
      )

instance Hashable StepSummary

instance NFData StepSummary
