{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowStepFunctionsParameters where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The parameters for a STEP_FUNCTIONS task.
--
--
-- For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
--
--
-- /See:/ 'maintenanceWindowStepFunctionsParameters' smart constructor.
data MaintenanceWindowStepFunctionsParameters = MaintenanceWindowStepFunctionsParameters'
  { _mwsfpInput ::
      !( Maybe
           ( Sensitive
               Text
           )
       ),
    _mwsfpName ::
      !( Maybe
           Text
       )
  }
  deriving (Eq, Show, Data, Typeable, Generic)

-- | Creates a value of 'MaintenanceWindowStepFunctionsParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwsfpInput' - The inputs for the STEP_FUNCTIONS task.
--
-- * 'mwsfpName' - The name of the STEP_FUNCTIONS task.
maintenanceWindowStepFunctionsParameters ::
  MaintenanceWindowStepFunctionsParameters
maintenanceWindowStepFunctionsParameters =
  MaintenanceWindowStepFunctionsParameters'
    { _mwsfpInput = Nothing,
      _mwsfpName = Nothing
    }

-- | The inputs for the STEP_FUNCTIONS task.
mwsfpInput :: Lens' MaintenanceWindowStepFunctionsParameters (Maybe Text)
mwsfpInput = lens _mwsfpInput (\s a -> s {_mwsfpInput = a}) . mapping _Sensitive

-- | The name of the STEP_FUNCTIONS task.
mwsfpName :: Lens' MaintenanceWindowStepFunctionsParameters (Maybe Text)
mwsfpName = lens _mwsfpName (\s a -> s {_mwsfpName = a})

instance FromJSON MaintenanceWindowStepFunctionsParameters where
  parseJSON =
    withObject
      "MaintenanceWindowStepFunctionsParameters"
      ( \x ->
          MaintenanceWindowStepFunctionsParameters'
            <$> (x .:? "Input") <*> (x .:? "Name")
      )

instance Hashable MaintenanceWindowStepFunctionsParameters

instance NFData MaintenanceWindowStepFunctionsParameters

instance ToJSON MaintenanceWindowStepFunctionsParameters where
  toJSON MaintenanceWindowStepFunctionsParameters' {..} =
    object
      ( catMaybes
          [("Input" .=) <$> _mwsfpInput, ("Name" .=) <$> _mwsfpName]
      )
