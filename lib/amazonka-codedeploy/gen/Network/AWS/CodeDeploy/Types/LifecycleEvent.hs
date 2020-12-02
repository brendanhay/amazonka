{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodeDeploy.Types.LifecycleEvent
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodeDeploy.Types.LifecycleEvent where

import Network.AWS.CodeDeploy.Types.Diagnostics
import Network.AWS.CodeDeploy.Types.LifecycleEventStatus
import Network.AWS.Lens
import Network.AWS.Prelude

-- | Information about a deployment lifecycle event.
--
--
--
-- /See:/ 'lifecycleEvent' smart constructor.
data LifecycleEvent = LifecycleEvent'
  { _leStatus ::
      !(Maybe LifecycleEventStatus),
    _leLifecycleEventName :: !(Maybe Text),
    _leStartTime :: !(Maybe POSIX),
    _leDiagnostics :: !(Maybe Diagnostics),
    _leEndTime :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LifecycleEvent' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'leStatus' - The deployment lifecycle event status:     * Pending: The deployment lifecycle event is pending.     * InProgress: The deployment lifecycle event is in progress.     * Succeeded: The deployment lifecycle event ran successfully.     * Failed: The deployment lifecycle event has failed.     * Skipped: The deployment lifecycle event has been skipped.     * Unknown: The deployment lifecycle event is unknown.
--
-- * 'leLifecycleEventName' - The deployment lifecycle event name, such as @ApplicationStop@ , @BeforeInstall@ , @AfterInstall@ , @ApplicationStart@ , or @ValidateService@ .
--
-- * 'leStartTime' - A timestamp that indicates when the deployment lifecycle event started.
--
-- * 'leDiagnostics' - Diagnostic information about the deployment lifecycle event.
--
-- * 'leEndTime' - A timestamp that indicates when the deployment lifecycle event ended.
lifecycleEvent ::
  LifecycleEvent
lifecycleEvent =
  LifecycleEvent'
    { _leStatus = Nothing,
      _leLifecycleEventName = Nothing,
      _leStartTime = Nothing,
      _leDiagnostics = Nothing,
      _leEndTime = Nothing
    }

-- | The deployment lifecycle event status:     * Pending: The deployment lifecycle event is pending.     * InProgress: The deployment lifecycle event is in progress.     * Succeeded: The deployment lifecycle event ran successfully.     * Failed: The deployment lifecycle event has failed.     * Skipped: The deployment lifecycle event has been skipped.     * Unknown: The deployment lifecycle event is unknown.
leStatus :: Lens' LifecycleEvent (Maybe LifecycleEventStatus)
leStatus = lens _leStatus (\s a -> s {_leStatus = a})

-- | The deployment lifecycle event name, such as @ApplicationStop@ , @BeforeInstall@ , @AfterInstall@ , @ApplicationStart@ , or @ValidateService@ .
leLifecycleEventName :: Lens' LifecycleEvent (Maybe Text)
leLifecycleEventName = lens _leLifecycleEventName (\s a -> s {_leLifecycleEventName = a})

-- | A timestamp that indicates when the deployment lifecycle event started.
leStartTime :: Lens' LifecycleEvent (Maybe UTCTime)
leStartTime = lens _leStartTime (\s a -> s {_leStartTime = a}) . mapping _Time

-- | Diagnostic information about the deployment lifecycle event.
leDiagnostics :: Lens' LifecycleEvent (Maybe Diagnostics)
leDiagnostics = lens _leDiagnostics (\s a -> s {_leDiagnostics = a})

-- | A timestamp that indicates when the deployment lifecycle event ended.
leEndTime :: Lens' LifecycleEvent (Maybe UTCTime)
leEndTime = lens _leEndTime (\s a -> s {_leEndTime = a}) . mapping _Time

instance FromJSON LifecycleEvent where
  parseJSON =
    withObject
      "LifecycleEvent"
      ( \x ->
          LifecycleEvent'
            <$> (x .:? "status")
            <*> (x .:? "lifecycleEventName")
            <*> (x .:? "startTime")
            <*> (x .:? "diagnostics")
            <*> (x .:? "endTime")
      )

instance Hashable LifecycleEvent

instance NFData LifecycleEvent
