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
-- Module      : Network.AWS.S3.Types.InventorySchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.InventorySchedule where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.InventoryFrequency

-- | Specifies the schedule for generating inventory results.
--
-- /See:/ 'newInventorySchedule' smart constructor.
data InventorySchedule = InventorySchedule'
  { -- | Specifies how frequently inventory results are produced.
    frequency :: InventoryFrequency
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'InventorySchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'frequency', 'inventorySchedule_frequency' - Specifies how frequently inventory results are produced.
newInventorySchedule ::
  -- | 'frequency'
  InventoryFrequency ->
  InventorySchedule
newInventorySchedule pFrequency_ =
  InventorySchedule' {frequency = pFrequency_}

-- | Specifies how frequently inventory results are produced.
inventorySchedule_frequency :: Lens.Lens' InventorySchedule InventoryFrequency
inventorySchedule_frequency = Lens.lens (\InventorySchedule' {frequency} -> frequency) (\s@InventorySchedule' {} a -> s {frequency = a} :: InventorySchedule)

instance Prelude.FromXML InventorySchedule where
  parseXML x =
    InventorySchedule'
      Prelude.<$> (x Prelude..@ "Frequency")

instance Prelude.Hashable InventorySchedule

instance Prelude.NFData InventorySchedule

instance Prelude.ToXML InventorySchedule where
  toXML InventorySchedule' {..} =
    Prelude.mconcat ["Frequency" Prelude.@= frequency]
