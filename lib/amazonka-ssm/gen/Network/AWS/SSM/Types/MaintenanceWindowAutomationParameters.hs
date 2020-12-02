{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.Types.MaintenanceWindowAutomationParameters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SSM.Types.MaintenanceWindowAutomationParameters where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | The parameters for an AUTOMATION task type.
--
--
--
-- /See:/ 'maintenanceWindowAutomationParameters' smart constructor.
data MaintenanceWindowAutomationParameters = MaintenanceWindowAutomationParameters'
  { _mwapParameters ::
      !( Maybe
           ( Map
               Text
               ([Text])
           )
       ),
    _mwapDocumentVersion ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'MaintenanceWindowAutomationParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mwapParameters' - The parameters for the AUTOMATION task. For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
--
-- * 'mwapDocumentVersion' - The version of an Automation document to use during task execution.
maintenanceWindowAutomationParameters ::
  MaintenanceWindowAutomationParameters
maintenanceWindowAutomationParameters =
  MaintenanceWindowAutomationParameters'
    { _mwapParameters = Nothing,
      _mwapDocumentVersion = Nothing
    }

-- | The parameters for the AUTOMATION task. For information about specifying and updating task parameters, see 'RegisterTaskWithMaintenanceWindow' and 'UpdateMaintenanceWindowTask' .
mwapParameters :: Lens' MaintenanceWindowAutomationParameters (HashMap Text ([Text]))
mwapParameters = lens _mwapParameters (\s a -> s {_mwapParameters = a}) . _Default . _Map

-- | The version of an Automation document to use during task execution.
mwapDocumentVersion :: Lens' MaintenanceWindowAutomationParameters (Maybe Text)
mwapDocumentVersion = lens _mwapDocumentVersion (\s a -> s {_mwapDocumentVersion = a})

instance FromJSON MaintenanceWindowAutomationParameters where
  parseJSON =
    withObject
      "MaintenanceWindowAutomationParameters"
      ( \x ->
          MaintenanceWindowAutomationParameters'
            <$> (x .:? "Parameters" .!= mempty) <*> (x .:? "DocumentVersion")
      )

instance Hashable MaintenanceWindowAutomationParameters

instance NFData MaintenanceWindowAutomationParameters

instance ToJSON MaintenanceWindowAutomationParameters where
  toJSON MaintenanceWindowAutomationParameters' {..} =
    object
      ( catMaybes
          [ ("Parameters" .=) <$> _mwapParameters,
            ("DocumentVersion" .=) <$> _mwapDocumentVersion
          ]
      )
