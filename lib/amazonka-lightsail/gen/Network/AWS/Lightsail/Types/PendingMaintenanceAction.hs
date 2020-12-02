{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.PendingMaintenanceAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.PendingMaintenanceAction where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Describes a pending database maintenance action.
--
--
--
-- /See:/ 'pendingMaintenanceAction' smart constructor.
data PendingMaintenanceAction = PendingMaintenanceAction'
  { _pmaAction ::
      !(Maybe Text),
    _pmaDescription :: !(Maybe Text),
    _pmaCurrentApplyDate :: !(Maybe POSIX)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'PendingMaintenanceAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'pmaAction' - The type of pending database maintenance action.
--
-- * 'pmaDescription' - Additional detail about the pending database maintenance action.
--
-- * 'pmaCurrentApplyDate' - The effective date of the pending database maintenance action.
pendingMaintenanceAction ::
  PendingMaintenanceAction
pendingMaintenanceAction =
  PendingMaintenanceAction'
    { _pmaAction = Nothing,
      _pmaDescription = Nothing,
      _pmaCurrentApplyDate = Nothing
    }

-- | The type of pending database maintenance action.
pmaAction :: Lens' PendingMaintenanceAction (Maybe Text)
pmaAction = lens _pmaAction (\s a -> s {_pmaAction = a})

-- | Additional detail about the pending database maintenance action.
pmaDescription :: Lens' PendingMaintenanceAction (Maybe Text)
pmaDescription = lens _pmaDescription (\s a -> s {_pmaDescription = a})

-- | The effective date of the pending database maintenance action.
pmaCurrentApplyDate :: Lens' PendingMaintenanceAction (Maybe UTCTime)
pmaCurrentApplyDate = lens _pmaCurrentApplyDate (\s a -> s {_pmaCurrentApplyDate = a}) . mapping _Time

instance FromJSON PendingMaintenanceAction where
  parseJSON =
    withObject
      "PendingMaintenanceAction"
      ( \x ->
          PendingMaintenanceAction'
            <$> (x .:? "action")
            <*> (x .:? "description")
            <*> (x .:? "currentApplyDate")
      )

instance Hashable PendingMaintenanceAction

instance NFData PendingMaintenanceAction
