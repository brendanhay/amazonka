{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.Command
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.Command where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a command.
--
--
--
-- /See:/ 'command' smart constructor.
data Command = Command'
  { _cDeploymentId :: !(Maybe Text),
    _cInstanceId :: !(Maybe Text),
    _cStatus :: !(Maybe Text),
    _cLogURL :: !(Maybe Text),
    _cCreatedAt :: !(Maybe Text),
    _cCommandId :: !(Maybe Text),
    _cExitCode :: !(Maybe Int),
    _cType :: !(Maybe Text),
    _cCompletedAt :: !(Maybe Text),
    _cAcknowledgedAt :: !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Command' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'cDeploymentId' - The command deployment ID.
--
-- * 'cInstanceId' - The ID of the instance where the command was executed.
--
-- * 'cStatus' - The command status:     * failed     * successful     * skipped     * pending
--
-- * 'cLogURL' - The URL of the command log.
--
-- * 'cCreatedAt' - Date and time when the command was run.
--
-- * 'cCommandId' - The command ID.
--
-- * 'cExitCode' - The command exit code.
--
-- * 'cType' - The command type:     * @configure@      * @deploy@      * @execute_recipes@      * @install_dependencies@      * @restart@      * @rollback@      * @setup@      * @start@      * @stop@      * @undeploy@      * @update_custom_cookbooks@      * @update_dependencies@
--
-- * 'cCompletedAt' - Date when the command completed.
--
-- * 'cAcknowledgedAt' - Date and time when the command was acknowledged.
command ::
  Command
command =
  Command'
    { _cDeploymentId = Nothing,
      _cInstanceId = Nothing,
      _cStatus = Nothing,
      _cLogURL = Nothing,
      _cCreatedAt = Nothing,
      _cCommandId = Nothing,
      _cExitCode = Nothing,
      _cType = Nothing,
      _cCompletedAt = Nothing,
      _cAcknowledgedAt = Nothing
    }

-- | The command deployment ID.
cDeploymentId :: Lens' Command (Maybe Text)
cDeploymentId = lens _cDeploymentId (\s a -> s {_cDeploymentId = a})

-- | The ID of the instance where the command was executed.
cInstanceId :: Lens' Command (Maybe Text)
cInstanceId = lens _cInstanceId (\s a -> s {_cInstanceId = a})

-- | The command status:     * failed     * successful     * skipped     * pending
cStatus :: Lens' Command (Maybe Text)
cStatus = lens _cStatus (\s a -> s {_cStatus = a})

-- | The URL of the command log.
cLogURL :: Lens' Command (Maybe Text)
cLogURL = lens _cLogURL (\s a -> s {_cLogURL = a})

-- | Date and time when the command was run.
cCreatedAt :: Lens' Command (Maybe Text)
cCreatedAt = lens _cCreatedAt (\s a -> s {_cCreatedAt = a})

-- | The command ID.
cCommandId :: Lens' Command (Maybe Text)
cCommandId = lens _cCommandId (\s a -> s {_cCommandId = a})

-- | The command exit code.
cExitCode :: Lens' Command (Maybe Int)
cExitCode = lens _cExitCode (\s a -> s {_cExitCode = a})

-- | The command type:     * @configure@      * @deploy@      * @execute_recipes@      * @install_dependencies@      * @restart@      * @rollback@      * @setup@      * @start@      * @stop@      * @undeploy@      * @update_custom_cookbooks@      * @update_dependencies@
cType :: Lens' Command (Maybe Text)
cType = lens _cType (\s a -> s {_cType = a})

-- | Date when the command completed.
cCompletedAt :: Lens' Command (Maybe Text)
cCompletedAt = lens _cCompletedAt (\s a -> s {_cCompletedAt = a})

-- | Date and time when the command was acknowledged.
cAcknowledgedAt :: Lens' Command (Maybe Text)
cAcknowledgedAt = lens _cAcknowledgedAt (\s a -> s {_cAcknowledgedAt = a})

instance FromJSON Command where
  parseJSON =
    withObject
      "Command"
      ( \x ->
          Command'
            <$> (x .:? "DeploymentId")
            <*> (x .:? "InstanceId")
            <*> (x .:? "Status")
            <*> (x .:? "LogUrl")
            <*> (x .:? "CreatedAt")
            <*> (x .:? "CommandId")
            <*> (x .:? "ExitCode")
            <*> (x .:? "Type")
            <*> (x .:? "CompletedAt")
            <*> (x .:? "AcknowledgedAt")
      )

instance Hashable Command

instance NFData Command
