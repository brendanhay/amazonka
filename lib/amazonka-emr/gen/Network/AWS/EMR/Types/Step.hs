{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.Step
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.Step where

import Network.AWS.EMR.Types.ActionOnFailure
import Network.AWS.EMR.Types.HadoopStepConfig
import Network.AWS.EMR.Types.StepStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | This represents a step in a cluster.
--
--
--
-- /See:/ 'step' smart constructor.
data Step = Step'
  { _sStatus :: !(Maybe StepStatus),
    _sActionOnFailure :: !(Maybe ActionOnFailure),
    _sConfig :: !(Maybe HadoopStepConfig),
    _sName :: !(Maybe Text),
    _sId :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Step' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'sStatus' - The current execution status details of the cluster step.
--
-- * 'sActionOnFailure' - The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is provided for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
--
-- * 'sConfig' - The Hadoop job configuration of the cluster step.
--
-- * 'sName' - The name of the cluster step.
--
-- * 'sId' - The identifier of the cluster step.
step ::
  Step
step =
  Step'
    { _sStatus = Nothing,
      _sActionOnFailure = Nothing,
      _sConfig = Nothing,
      _sName = Nothing,
      _sId = Nothing
    }

-- | The current execution status details of the cluster step.
sStatus :: Lens' Step (Maybe StepStatus)
sStatus = lens _sStatus (\s a -> s {_sStatus = a})

-- | The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is provided for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
sActionOnFailure :: Lens' Step (Maybe ActionOnFailure)
sActionOnFailure = lens _sActionOnFailure (\s a -> s {_sActionOnFailure = a})

-- | The Hadoop job configuration of the cluster step.
sConfig :: Lens' Step (Maybe HadoopStepConfig)
sConfig = lens _sConfig (\s a -> s {_sConfig = a})

-- | The name of the cluster step.
sName :: Lens' Step (Maybe Text)
sName = lens _sName (\s a -> s {_sName = a})

-- | The identifier of the cluster step.
sId :: Lens' Step (Maybe Text)
sId = lens _sId (\s a -> s {_sId = a})

instance FromJSON Step where
  parseJSON =
    withObject
      "Step"
      ( \x ->
          Step'
            <$> (x .:? "Status")
            <*> (x .:? "ActionOnFailure")
            <*> (x .:? "Config")
            <*> (x .:? "Name")
            <*> (x .:? "Id")
      )

instance Hashable Step

instance NFData Step
