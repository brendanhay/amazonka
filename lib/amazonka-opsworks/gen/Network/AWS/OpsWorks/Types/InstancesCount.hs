{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.InstancesCount
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.InstancesCount where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes how many instances a stack has for each status.
--
--
--
-- /See:/ 'instancesCount' smart constructor.
data InstancesCount = InstancesCount'
  { _icTerminating ::
      !(Maybe Int),
    _icPending :: !(Maybe Int),
    _icOnline :: !(Maybe Int),
    _icUnassigning :: !(Maybe Int),
    _icDeregistering :: !(Maybe Int),
    _icRunningSetup :: !(Maybe Int),
    _icRequested :: !(Maybe Int),
    _icStopFailed :: !(Maybe Int),
    _icBooting :: !(Maybe Int),
    _icStopped :: !(Maybe Int),
    _icRebooting :: !(Maybe Int),
    _icAssigning :: !(Maybe Int),
    _icShuttingDown :: !(Maybe Int),
    _icSetupFailed :: !(Maybe Int),
    _icConnectionLost :: !(Maybe Int),
    _icTerminated :: !(Maybe Int),
    _icStopping :: !(Maybe Int),
    _icRegistered :: !(Maybe Int),
    _icStartFailed :: !(Maybe Int),
    _icRegistering :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'InstancesCount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'icTerminating' - The number of instances with @terminating@ status.
--
-- * 'icPending' - The number of instances with @pending@ status.
--
-- * 'icOnline' - The number of instances with @online@ status.
--
-- * 'icUnassigning' - The number of instances in the Unassigning state.
--
-- * 'icDeregistering' - The number of instances in the Deregistering state.
--
-- * 'icRunningSetup' - The number of instances with @running_setup@ status.
--
-- * 'icRequested' - The number of instances with @requested@ status.
--
-- * 'icStopFailed' - The number of instances with @stop_failed@ status.
--
-- * 'icBooting' - The number of instances with @booting@ status.
--
-- * 'icStopped' - The number of instances with @stopped@ status.
--
-- * 'icRebooting' - The number of instances with @rebooting@ status.
--
-- * 'icAssigning' - The number of instances in the Assigning state.
--
-- * 'icShuttingDown' - The number of instances with @shutting_down@ status.
--
-- * 'icSetupFailed' - The number of instances with @setup_failed@ status.
--
-- * 'icConnectionLost' - The number of instances with @connection_lost@ status.
--
-- * 'icTerminated' - The number of instances with @terminated@ status.
--
-- * 'icStopping' - The number of instances with @stopping@ status.
--
-- * 'icRegistered' - The number of instances in the Registered state.
--
-- * 'icStartFailed' - The number of instances with @start_failed@ status.
--
-- * 'icRegistering' - The number of instances in the Registering state.
instancesCount ::
  InstancesCount
instancesCount =
  InstancesCount'
    { _icTerminating = Nothing,
      _icPending = Nothing,
      _icOnline = Nothing,
      _icUnassigning = Nothing,
      _icDeregistering = Nothing,
      _icRunningSetup = Nothing,
      _icRequested = Nothing,
      _icStopFailed = Nothing,
      _icBooting = Nothing,
      _icStopped = Nothing,
      _icRebooting = Nothing,
      _icAssigning = Nothing,
      _icShuttingDown = Nothing,
      _icSetupFailed = Nothing,
      _icConnectionLost = Nothing,
      _icTerminated = Nothing,
      _icStopping = Nothing,
      _icRegistered = Nothing,
      _icStartFailed = Nothing,
      _icRegistering = Nothing
    }

-- | The number of instances with @terminating@ status.
icTerminating :: Lens' InstancesCount (Maybe Int)
icTerminating = lens _icTerminating (\s a -> s {_icTerminating = a})

-- | The number of instances with @pending@ status.
icPending :: Lens' InstancesCount (Maybe Int)
icPending = lens _icPending (\s a -> s {_icPending = a})

-- | The number of instances with @online@ status.
icOnline :: Lens' InstancesCount (Maybe Int)
icOnline = lens _icOnline (\s a -> s {_icOnline = a})

-- | The number of instances in the Unassigning state.
icUnassigning :: Lens' InstancesCount (Maybe Int)
icUnassigning = lens _icUnassigning (\s a -> s {_icUnassigning = a})

-- | The number of instances in the Deregistering state.
icDeregistering :: Lens' InstancesCount (Maybe Int)
icDeregistering = lens _icDeregistering (\s a -> s {_icDeregistering = a})

-- | The number of instances with @running_setup@ status.
icRunningSetup :: Lens' InstancesCount (Maybe Int)
icRunningSetup = lens _icRunningSetup (\s a -> s {_icRunningSetup = a})

-- | The number of instances with @requested@ status.
icRequested :: Lens' InstancesCount (Maybe Int)
icRequested = lens _icRequested (\s a -> s {_icRequested = a})

-- | The number of instances with @stop_failed@ status.
icStopFailed :: Lens' InstancesCount (Maybe Int)
icStopFailed = lens _icStopFailed (\s a -> s {_icStopFailed = a})

-- | The number of instances with @booting@ status.
icBooting :: Lens' InstancesCount (Maybe Int)
icBooting = lens _icBooting (\s a -> s {_icBooting = a})

-- | The number of instances with @stopped@ status.
icStopped :: Lens' InstancesCount (Maybe Int)
icStopped = lens _icStopped (\s a -> s {_icStopped = a})

-- | The number of instances with @rebooting@ status.
icRebooting :: Lens' InstancesCount (Maybe Int)
icRebooting = lens _icRebooting (\s a -> s {_icRebooting = a})

-- | The number of instances in the Assigning state.
icAssigning :: Lens' InstancesCount (Maybe Int)
icAssigning = lens _icAssigning (\s a -> s {_icAssigning = a})

-- | The number of instances with @shutting_down@ status.
icShuttingDown :: Lens' InstancesCount (Maybe Int)
icShuttingDown = lens _icShuttingDown (\s a -> s {_icShuttingDown = a})

-- | The number of instances with @setup_failed@ status.
icSetupFailed :: Lens' InstancesCount (Maybe Int)
icSetupFailed = lens _icSetupFailed (\s a -> s {_icSetupFailed = a})

-- | The number of instances with @connection_lost@ status.
icConnectionLost :: Lens' InstancesCount (Maybe Int)
icConnectionLost = lens _icConnectionLost (\s a -> s {_icConnectionLost = a})

-- | The number of instances with @terminated@ status.
icTerminated :: Lens' InstancesCount (Maybe Int)
icTerminated = lens _icTerminated (\s a -> s {_icTerminated = a})

-- | The number of instances with @stopping@ status.
icStopping :: Lens' InstancesCount (Maybe Int)
icStopping = lens _icStopping (\s a -> s {_icStopping = a})

-- | The number of instances in the Registered state.
icRegistered :: Lens' InstancesCount (Maybe Int)
icRegistered = lens _icRegistered (\s a -> s {_icRegistered = a})

-- | The number of instances with @start_failed@ status.
icStartFailed :: Lens' InstancesCount (Maybe Int)
icStartFailed = lens _icStartFailed (\s a -> s {_icStartFailed = a})

-- | The number of instances in the Registering state.
icRegistering :: Lens' InstancesCount (Maybe Int)
icRegistering = lens _icRegistering (\s a -> s {_icRegistering = a})

instance FromJSON InstancesCount where
  parseJSON =
    withObject
      "InstancesCount"
      ( \x ->
          InstancesCount'
            <$> (x .:? "Terminating")
            <*> (x .:? "Pending")
            <*> (x .:? "Online")
            <*> (x .:? "Unassigning")
            <*> (x .:? "Deregistering")
            <*> (x .:? "RunningSetup")
            <*> (x .:? "Requested")
            <*> (x .:? "StopFailed")
            <*> (x .:? "Booting")
            <*> (x .:? "Stopped")
            <*> (x .:? "Rebooting")
            <*> (x .:? "Assigning")
            <*> (x .:? "ShuttingDown")
            <*> (x .:? "SetupFailed")
            <*> (x .:? "ConnectionLost")
            <*> (x .:? "Terminated")
            <*> (x .:? "Stopping")
            <*> (x .:? "Registered")
            <*> (x .:? "StartFailed")
            <*> (x .:? "Registering")
      )

instance Hashable InstancesCount

instance NFData InstancesCount
