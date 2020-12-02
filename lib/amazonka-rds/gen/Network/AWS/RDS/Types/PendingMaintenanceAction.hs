{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.PendingMaintenanceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.PendingMaintenanceAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Provides information about a pending maintenance action for a resource.
--
--
--
-- /See:/ 'pendingMaintenanceAction' smart constructor.
data PendingMaintenanceAction = PendingMaintenanceAction'
  { _pmaAutoAppliedAfterDate ::
      !(Maybe ISO8601),
    _pmaAction :: !(Maybe Text),
    _pmaOptInStatus :: !(Maybe Text),
    _pmaDescription :: !(Maybe Text),
    _pmaForcedApplyDate :: !(Maybe ISO8601),
    _pmaCurrentApplyDate :: !(Maybe ISO8601)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PendingMaintenanceAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmaAutoAppliedAfterDate' - The date of the maintenance window when the action is applied. The maintenance action is applied to the resource during its first maintenance window after this date.
--
-- * 'pmaAction' - The type of pending maintenance action that is available for the resource. Valid actions are @system-update@ , @db-upgrade@ , @hardware-maintenance@ , and @ca-certificate-rotation@ .
--
-- * 'pmaOptInStatus' - Indicates the type of opt-in request that has been received for the resource.
--
-- * 'pmaDescription' - A description providing more detail about the maintenance action.
--
-- * 'pmaForcedApplyDate' - The date when the maintenance action is automatically applied. On this date, the maintenance action is applied to the resource as soon as possible, regardless of the maintenance window for the resource. There might be a delay of one or more days from this date before the maintenance action is applied.
--
-- * 'pmaCurrentApplyDate' - The effective date when the pending maintenance action is applied to the resource. This date takes into account opt-in requests received from the @ApplyPendingMaintenanceAction@ API, the @AutoAppliedAfterDate@ , and the @ForcedApplyDate@ . This value is blank if an opt-in request has not been received and nothing has been specified as @AutoAppliedAfterDate@ or @ForcedApplyDate@ .
pendingMaintenanceAction ::
  PendingMaintenanceAction
pendingMaintenanceAction =
  PendingMaintenanceAction'
    { _pmaAutoAppliedAfterDate = Nothing,
      _pmaAction = Nothing,
      _pmaOptInStatus = Nothing,
      _pmaDescription = Nothing,
      _pmaForcedApplyDate = Nothing,
      _pmaCurrentApplyDate = Nothing
    }

-- | The date of the maintenance window when the action is applied. The maintenance action is applied to the resource during its first maintenance window after this date.
pmaAutoAppliedAfterDate :: Lens' PendingMaintenanceAction (Maybe UTCTime)
pmaAutoAppliedAfterDate = lens _pmaAutoAppliedAfterDate (\s a -> s {_pmaAutoAppliedAfterDate = a}) . mapping _Time

-- | The type of pending maintenance action that is available for the resource. Valid actions are @system-update@ , @db-upgrade@ , @hardware-maintenance@ , and @ca-certificate-rotation@ .
pmaAction :: Lens' PendingMaintenanceAction (Maybe Text)
pmaAction = lens _pmaAction (\s a -> s {_pmaAction = a})

-- | Indicates the type of opt-in request that has been received for the resource.
pmaOptInStatus :: Lens' PendingMaintenanceAction (Maybe Text)
pmaOptInStatus = lens _pmaOptInStatus (\s a -> s {_pmaOptInStatus = a})

-- | A description providing more detail about the maintenance action.
pmaDescription :: Lens' PendingMaintenanceAction (Maybe Text)
pmaDescription = lens _pmaDescription (\s a -> s {_pmaDescription = a})

-- | The date when the maintenance action is automatically applied. On this date, the maintenance action is applied to the resource as soon as possible, regardless of the maintenance window for the resource. There might be a delay of one or more days from this date before the maintenance action is applied.
pmaForcedApplyDate :: Lens' PendingMaintenanceAction (Maybe UTCTime)
pmaForcedApplyDate = lens _pmaForcedApplyDate (\s a -> s {_pmaForcedApplyDate = a}) . mapping _Time

-- | The effective date when the pending maintenance action is applied to the resource. This date takes into account opt-in requests received from the @ApplyPendingMaintenanceAction@ API, the @AutoAppliedAfterDate@ , and the @ForcedApplyDate@ . This value is blank if an opt-in request has not been received and nothing has been specified as @AutoAppliedAfterDate@ or @ForcedApplyDate@ .
pmaCurrentApplyDate :: Lens' PendingMaintenanceAction (Maybe UTCTime)
pmaCurrentApplyDate = lens _pmaCurrentApplyDate (\s a -> s {_pmaCurrentApplyDate = a}) . mapping _Time

instance FromXML PendingMaintenanceAction where
  parseXML x =
    PendingMaintenanceAction'
      <$> (x .@? "AutoAppliedAfterDate")
      <*> (x .@? "Action")
      <*> (x .@? "OptInStatus")
      <*> (x .@? "Description")
      <*> (x .@? "ForcedApplyDate")
      <*> (x .@? "CurrentApplyDate")

instance Hashable PendingMaintenanceAction

instance NFData PendingMaintenanceAction
