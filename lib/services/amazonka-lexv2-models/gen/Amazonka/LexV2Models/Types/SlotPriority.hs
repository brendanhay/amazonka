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
-- Module      : Amazonka.LexV2Models.Types.SlotPriority
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SlotPriority where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Sets the priority that Amazon Lex should use when eliciting slot values
-- from a user.
--
-- /See:/ 'newSlotPriority' smart constructor.
data SlotPriority = SlotPriority'
  { -- | The priority that a slot should be elicited.
    priority :: Prelude.Natural,
    -- | The unique identifier of the slot.
    slotId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotPriority' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'priority', 'slotPriority_priority' - The priority that a slot should be elicited.
--
-- 'slotId', 'slotPriority_slotId' - The unique identifier of the slot.
newSlotPriority ::
  -- | 'priority'
  Prelude.Natural ->
  -- | 'slotId'
  Prelude.Text ->
  SlotPriority
newSlotPriority pPriority_ pSlotId_ =
  SlotPriority'
    { priority = pPriority_,
      slotId = pSlotId_
    }

-- | The priority that a slot should be elicited.
slotPriority_priority :: Lens.Lens' SlotPriority Prelude.Natural
slotPriority_priority = Lens.lens (\SlotPriority' {priority} -> priority) (\s@SlotPriority' {} a -> s {priority = a} :: SlotPriority)

-- | The unique identifier of the slot.
slotPriority_slotId :: Lens.Lens' SlotPriority Prelude.Text
slotPriority_slotId = Lens.lens (\SlotPriority' {slotId} -> slotId) (\s@SlotPriority' {} a -> s {slotId = a} :: SlotPriority)

instance Data.FromJSON SlotPriority where
  parseJSON =
    Data.withObject
      "SlotPriority"
      ( \x ->
          SlotPriority'
            Prelude.<$> (x Data..: "priority")
            Prelude.<*> (x Data..: "slotId")
      )

instance Prelude.Hashable SlotPriority where
  hashWithSalt _salt SlotPriority' {..} =
    _salt
      `Prelude.hashWithSalt` priority
      `Prelude.hashWithSalt` slotId

instance Prelude.NFData SlotPriority where
  rnf SlotPriority' {..} =
    Prelude.rnf priority `Prelude.seq`
      Prelude.rnf slotId

instance Data.ToJSON SlotPriority where
  toJSON SlotPriority' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("priority" Data..= priority),
            Prelude.Just ("slotId" Data..= slotId)
          ]
      )
