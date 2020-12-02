{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowTaskInvocationParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowTaskInvocationParameters where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SSM.Types.MaintenanceWindowAutomationParameters
import Network.AWS.SSM.Types.MaintenanceWindowLambdaParameters
import Network.AWS.SSM.Types.MaintenanceWindowRunCommandParameters
import Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsParameters

-- | The parameters for task execution.
--
--
--
-- /See:/ 'maintenanceWindowTaskInvocationParameters' smart constructor.
data MaintenanceWindowTaskInvocationParameters = MaintenanceWindowTaskInvocationParameters'
  { _mwtipAutomation ::
      !( Maybe
           MaintenanceWindowAutomationParameters
       ),
    _mwtipStepFunctions ::
      !( Maybe
           MaintenanceWindowStepFunctionsParameters
       ),
    _mwtipRunCommand ::
      !( Maybe
           MaintenanceWindowRunCommandParameters
       ),
    _mwtipLambda ::
      !( Maybe
           MaintenanceWindowLambdaParameters
       )
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'MaintenanceWindowTaskInvocationParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwtipAutomation' - The parameters for an AUTOMATION task type.
--
-- * 'mwtipStepFunctions' - The parameters for a STEP_FUNCTIONS task type.
--
-- * 'mwtipRunCommand' - The parameters for a RUN_COMMAND task type.
--
-- * 'mwtipLambda' - The parameters for a LAMBDA task type.
maintenanceWindowTaskInvocationParameters ::
  MaintenanceWindowTaskInvocationParameters
maintenanceWindowTaskInvocationParameters =
  MaintenanceWindowTaskInvocationParameters'
    { _mwtipAutomation =
        Nothing,
      _mwtipStepFunctions = Nothing,
      _mwtipRunCommand = Nothing,
      _mwtipLambda = Nothing
    }

-- | The parameters for an AUTOMATION task type.
mwtipAutomation :: Lens' MaintenanceWindowTaskInvocationParameters (Maybe MaintenanceWindowAutomationParameters)
mwtipAutomation = lens _mwtipAutomation (\s a -> s {_mwtipAutomation = a})

-- | The parameters for a STEP_FUNCTIONS task type.
mwtipStepFunctions :: Lens' MaintenanceWindowTaskInvocationParameters (Maybe MaintenanceWindowStepFunctionsParameters)
mwtipStepFunctions = lens _mwtipStepFunctions (\s a -> s {_mwtipStepFunctions = a})

-- | The parameters for a RUN_COMMAND task type.
mwtipRunCommand :: Lens' MaintenanceWindowTaskInvocationParameters (Maybe MaintenanceWindowRunCommandParameters)
mwtipRunCommand = lens _mwtipRunCommand (\s a -> s {_mwtipRunCommand = a})

-- | The parameters for a LAMBDA task type.
mwtipLambda :: Lens' MaintenanceWindowTaskInvocationParameters (Maybe MaintenanceWindowLambdaParameters)
mwtipLambda = lens _mwtipLambda (\s a -> s {_mwtipLambda = a})

instance FromJSON MaintenanceWindowTaskInvocationParameters where
  parseJSON =
    withObject
      "MaintenanceWindowTaskInvocationParameters"
      ( \x ->
          MaintenanceWindowTaskInvocationParameters'
            <$> (x .:? "Automation")
            <*> (x .:? "StepFunctions")
            <*> (x .:? "RunCommand")
            <*> (x .:? "Lambda")
      )

instance Hashable MaintenanceWindowTaskInvocationParameters

instance NFData MaintenanceWindowTaskInvocationParameters

instance ToJSON MaintenanceWindowTaskInvocationParameters where
  toJSON MaintenanceWindowTaskInvocationParameters' {..} =
    object
      ( catMaybes
          [ ("Automation" .=) <$> _mwtipAutomation,
            ("StepFunctions" .=) <$> _mwtipStepFunctions,
            ("RunCommand" .=) <$> _mwtipRunCommand,
            ("Lambda" .=) <$> _mwtipLambda
          ]
      )
