{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.ReshardingStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.ReshardingStatus where

import Network.AWS.ElastiCache.Types.SlotMigration
import Network.AWS.Lens
import Network.AWS.Prelude

-- | The status of an online resharding operation.
--
--
--
-- /See:/ 'reshardingStatus' smart constructor.
newtype ReshardingStatus = ReshardingStatus'
  { _rsSlotMigration ::
      Maybe SlotMigration
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'ReshardingStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsSlotMigration' - Represents the progress of an online resharding operation.
reshardingStatus ::
  ReshardingStatus
reshardingStatus = ReshardingStatus' {_rsSlotMigration = Nothing}

-- | Represents the progress of an online resharding operation.
rsSlotMigration :: Lens' ReshardingStatus (Maybe SlotMigration)
rsSlotMigration = lens _rsSlotMigration (\s a -> s {_rsSlotMigration = a})

instance FromXML ReshardingStatus where
  parseXML x = ReshardingStatus' <$> (x .@? "SlotMigration")

instance Hashable ReshardingStatus

instance NFData ReshardingStatus
