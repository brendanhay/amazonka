{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.SlotMigration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.SlotMigration where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the progress of an online resharding operation.
--
-- /See:/ 'newSlotMigration' smart constructor.
data SlotMigration = SlotMigration'
  { -- | The percentage of the slot migration that is complete.
    progressPercentage :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SlotMigration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'progressPercentage', 'slotMigration_progressPercentage' - The percentage of the slot migration that is complete.
newSlotMigration ::
  SlotMigration
newSlotMigration =
  SlotMigration'
    { progressPercentage =
        Prelude.Nothing
    }

-- | The percentage of the slot migration that is complete.
slotMigration_progressPercentage :: Lens.Lens' SlotMigration (Prelude.Maybe Prelude.Double)
slotMigration_progressPercentage = Lens.lens (\SlotMigration' {progressPercentage} -> progressPercentage) (\s@SlotMigration' {} a -> s {progressPercentage = a} :: SlotMigration)

instance Prelude.FromXML SlotMigration where
  parseXML x =
    SlotMigration'
      Prelude.<$> (x Prelude..@? "ProgressPercentage")

instance Prelude.Hashable SlotMigration

instance Prelude.NFData SlotMigration
