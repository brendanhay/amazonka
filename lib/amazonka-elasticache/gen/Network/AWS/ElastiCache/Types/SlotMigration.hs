{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.SlotMigration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.SlotMigration where

import Network.AWS.Lens
import Network.AWS.Prelude

-- | Represents the progress of an online resharding operation.
--
--
--
-- /See:/ 'slotMigration' smart constructor.
newtype SlotMigration = SlotMigration'
  { _smProgressPercentage ::
      Maybe Double
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SlotMigration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'smProgressPercentage' - The percentage of the slot migration that is complete.
slotMigration ::
  SlotMigration
slotMigration = SlotMigration' {_smProgressPercentage = Nothing}

-- | The percentage of the slot migration that is complete.
smProgressPercentage :: Lens' SlotMigration (Maybe Double)
smProgressPercentage = lens _smProgressPercentage (\s a -> s {_smProgressPercentage = a})

instance FromXML SlotMigration where
  parseXML x = SlotMigration' <$> (x .@? "ProgressPercentage")

instance Hashable SlotMigration

instance NFData SlotMigration
