{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.DeferredMaintenanceWindow
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.DeferredMaintenanceWindow where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Redshift.Internal

-- | Describes a deferred maintenance window
--
--
--
-- /See:/ 'deferredMaintenanceWindow' smart constructor.
data DeferredMaintenanceWindow = DeferredMaintenanceWindow'
  { _dmwDeferMaintenanceEndTime ::
      !(Maybe ISO8601),
    _dmwDeferMaintenanceStartTime ::
      !(Maybe ISO8601),
    _dmwDeferMaintenanceIdentifier ::
      !(Maybe Text)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeferredMaintenanceWindow' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmwDeferMaintenanceEndTime' - A timestamp for the end of the time period when we defer maintenance.
--
-- * 'dmwDeferMaintenanceStartTime' - A timestamp for the beginning of the time period when we defer maintenance.
--
-- * 'dmwDeferMaintenanceIdentifier' - A unique identifier for the maintenance window.
deferredMaintenanceWindow ::
  DeferredMaintenanceWindow
deferredMaintenanceWindow =
  DeferredMaintenanceWindow'
    { _dmwDeferMaintenanceEndTime = Nothing,
      _dmwDeferMaintenanceStartTime = Nothing,
      _dmwDeferMaintenanceIdentifier = Nothing
    }

-- | A timestamp for the end of the time period when we defer maintenance.
dmwDeferMaintenanceEndTime :: Lens' DeferredMaintenanceWindow (Maybe UTCTime)
dmwDeferMaintenanceEndTime = lens _dmwDeferMaintenanceEndTime (\s a -> s {_dmwDeferMaintenanceEndTime = a}) . mapping _Time

-- | A timestamp for the beginning of the time period when we defer maintenance.
dmwDeferMaintenanceStartTime :: Lens' DeferredMaintenanceWindow (Maybe UTCTime)
dmwDeferMaintenanceStartTime = lens _dmwDeferMaintenanceStartTime (\s a -> s {_dmwDeferMaintenanceStartTime = a}) . mapping _Time

-- | A unique identifier for the maintenance window.
dmwDeferMaintenanceIdentifier :: Lens' DeferredMaintenanceWindow (Maybe Text)
dmwDeferMaintenanceIdentifier = lens _dmwDeferMaintenanceIdentifier (\s a -> s {_dmwDeferMaintenanceIdentifier = a})

instance FromXML DeferredMaintenanceWindow where
  parseXML x =
    DeferredMaintenanceWindow'
      <$> (x .@? "DeferMaintenanceEndTime")
      <*> (x .@? "DeferMaintenanceStartTime")
      <*> (x .@? "DeferMaintenanceIdentifier")

instance Hashable DeferredMaintenanceWindow

instance NFData DeferredMaintenanceWindow
