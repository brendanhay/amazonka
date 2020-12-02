{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.UpdateTarget
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.UpdateTarget where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.SupportedOperation

-- | A maintenance track that you can switch the current track to.
--
--
--
-- /See:/ 'updateTarget' smart constructor.
data UpdateTarget = UpdateTarget'
  { _utDatabaseVersion ::
      !(Maybe Text),
    _utMaintenanceTrackName :: !(Maybe Text),
    _utSupportedOperations :: !(Maybe [SupportedOperation])
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateTarget' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'utDatabaseVersion' - The cluster version for the new maintenance track.
--
-- * 'utMaintenanceTrackName' - The name of the new maintenance track.
--
-- * 'utSupportedOperations' - A list of operations supported by the maintenance track.
updateTarget ::
  UpdateTarget
updateTarget =
  UpdateTarget'
    { _utDatabaseVersion = Nothing,
      _utMaintenanceTrackName = Nothing,
      _utSupportedOperations = Nothing
    }

-- | The cluster version for the new maintenance track.
utDatabaseVersion :: Lens' UpdateTarget (Maybe Text)
utDatabaseVersion = lens _utDatabaseVersion (\s a -> s {_utDatabaseVersion = a})

-- | The name of the new maintenance track.
utMaintenanceTrackName :: Lens' UpdateTarget (Maybe Text)
utMaintenanceTrackName = lens _utMaintenanceTrackName (\s a -> s {_utMaintenanceTrackName = a})

-- | A list of operations supported by the maintenance track.
utSupportedOperations :: Lens' UpdateTarget [SupportedOperation]
utSupportedOperations = lens _utSupportedOperations (\s a -> s {_utSupportedOperations = a}) . _Default . _Coerce

instance FromXML UpdateTarget where
  parseXML x =
    UpdateTarget'
      <$> (x .@? "DatabaseVersion")
      <*> (x .@? "MaintenanceTrackName")
      <*> ( x .@? "SupportedOperations" .!@ mempty
              >>= may (parseXMLList "SupportedOperation")
          )

instance Hashable UpdateTarget

instance NFData UpdateTarget
