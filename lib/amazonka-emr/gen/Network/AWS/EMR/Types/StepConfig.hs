{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.StepConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.StepConfig where

import Network.AWS.EMR.Types.ActionOnFailure
import Network.AWS.EMR.Types.HadoopJARStepConfig
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Specification of a cluster (job flow) step.
--
--
--
-- /See:/ 'stepConfig' smart constructor.
data StepConfig = StepConfig'
  { _scActionOnFailure ::
      !(Maybe ActionOnFailure),
    _scName :: !Text,
    _scHadoopJARStep :: !HadoopJARStepConfig
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'StepConfig' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'scActionOnFailure' - The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is provided for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
--
-- * 'scName' - The name of the step.
--
-- * 'scHadoopJARStep' - The JAR file used for the step.
stepConfig ::
  -- | 'scName'
  Text ->
  -- | 'scHadoopJARStep'
  HadoopJARStepConfig ->
  StepConfig
stepConfig pName_ pHadoopJARStep_ =
  StepConfig'
    { _scActionOnFailure = Nothing,
      _scName = pName_,
      _scHadoopJARStep = pHadoopJARStep_
    }

-- | The action to take when the cluster step fails. Possible values are TERMINATE_CLUSTER, CANCEL_AND_WAIT, and CONTINUE. TERMINATE_JOB_FLOW is provided for backward compatibility. We recommend using TERMINATE_CLUSTER instead.
scActionOnFailure :: Lens' StepConfig (Maybe ActionOnFailure)
scActionOnFailure = lens _scActionOnFailure (\s a -> s {_scActionOnFailure = a})

-- | The name of the step.
scName :: Lens' StepConfig Text
scName = lens _scName (\s a -> s {_scName = a})

-- | The JAR file used for the step.
scHadoopJARStep :: Lens' StepConfig HadoopJARStepConfig
scHadoopJARStep = lens _scHadoopJARStep (\s a -> s {_scHadoopJARStep = a})

instance Hashable StepConfig

instance NFData StepConfig

instance ToJSON StepConfig where
  toJSON StepConfig' {..} =
    object
      ( catMaybes
          [ ("ActionOnFailure" .=) <$> _scActionOnFailure,
            Just ("Name" .= _scName),
            Just ("HadoopJarStep" .= _scHadoopJARStep)
          ]
      )
