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
-- Module      : Amazonka.LexV2Models.Types.SlotSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SlotSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexV2Models.Types.PromptSpecification
import Amazonka.LexV2Models.Types.SlotConstraint
import qualified Amazonka.Prelude as Prelude

-- | Summary information about a slot, a value that the bot elicits from the
-- user.
--
-- /See:/ 'newSlotSummary' smart constructor.
data SlotSummary = SlotSummary'
  { -- | The description of the slot.
    description :: Prelude.Maybe Prelude.Text,
    -- | The timestamp of the last date and time that the slot was updated.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | Whether the slot is required or optional. An intent is complete when all
    -- required slots are filled.
    slotConstraint :: Prelude.Maybe SlotConstraint,
    -- | The unique identifier of the slot.
    slotId :: Prelude.Maybe Prelude.Text,
    -- | The name given to the slot.
    slotName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the slot type that defines the values for the
    -- slot.
    slotTypeId :: Prelude.Maybe Prelude.Text,
    -- | Prompts that are sent to the user to elicit a value for the slot.
    valueElicitationPromptSpecification :: Prelude.Maybe PromptSpecification
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'slotSummary_description' - The description of the slot.
--
-- 'lastUpdatedDateTime', 'slotSummary_lastUpdatedDateTime' - The timestamp of the last date and time that the slot was updated.
--
-- 'slotConstraint', 'slotSummary_slotConstraint' - Whether the slot is required or optional. An intent is complete when all
-- required slots are filled.
--
-- 'slotId', 'slotSummary_slotId' - The unique identifier of the slot.
--
-- 'slotName', 'slotSummary_slotName' - The name given to the slot.
--
-- 'slotTypeId', 'slotSummary_slotTypeId' - The unique identifier for the slot type that defines the values for the
-- slot.
--
-- 'valueElicitationPromptSpecification', 'slotSummary_valueElicitationPromptSpecification' - Prompts that are sent to the user to elicit a value for the slot.
newSlotSummary ::
  SlotSummary
newSlotSummary =
  SlotSummary'
    { description = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      slotConstraint = Prelude.Nothing,
      slotId = Prelude.Nothing,
      slotName = Prelude.Nothing,
      slotTypeId = Prelude.Nothing,
      valueElicitationPromptSpecification =
        Prelude.Nothing
    }

-- | The description of the slot.
slotSummary_description :: Lens.Lens' SlotSummary (Prelude.Maybe Prelude.Text)
slotSummary_description = Lens.lens (\SlotSummary' {description} -> description) (\s@SlotSummary' {} a -> s {description = a} :: SlotSummary)

-- | The timestamp of the last date and time that the slot was updated.
slotSummary_lastUpdatedDateTime :: Lens.Lens' SlotSummary (Prelude.Maybe Prelude.UTCTime)
slotSummary_lastUpdatedDateTime = Lens.lens (\SlotSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@SlotSummary' {} a -> s {lastUpdatedDateTime = a} :: SlotSummary) Prelude.. Lens.mapping Data._Time

-- | Whether the slot is required or optional. An intent is complete when all
-- required slots are filled.
slotSummary_slotConstraint :: Lens.Lens' SlotSummary (Prelude.Maybe SlotConstraint)
slotSummary_slotConstraint = Lens.lens (\SlotSummary' {slotConstraint} -> slotConstraint) (\s@SlotSummary' {} a -> s {slotConstraint = a} :: SlotSummary)

-- | The unique identifier of the slot.
slotSummary_slotId :: Lens.Lens' SlotSummary (Prelude.Maybe Prelude.Text)
slotSummary_slotId = Lens.lens (\SlotSummary' {slotId} -> slotId) (\s@SlotSummary' {} a -> s {slotId = a} :: SlotSummary)

-- | The name given to the slot.
slotSummary_slotName :: Lens.Lens' SlotSummary (Prelude.Maybe Prelude.Text)
slotSummary_slotName = Lens.lens (\SlotSummary' {slotName} -> slotName) (\s@SlotSummary' {} a -> s {slotName = a} :: SlotSummary)

-- | The unique identifier for the slot type that defines the values for the
-- slot.
slotSummary_slotTypeId :: Lens.Lens' SlotSummary (Prelude.Maybe Prelude.Text)
slotSummary_slotTypeId = Lens.lens (\SlotSummary' {slotTypeId} -> slotTypeId) (\s@SlotSummary' {} a -> s {slotTypeId = a} :: SlotSummary)

-- | Prompts that are sent to the user to elicit a value for the slot.
slotSummary_valueElicitationPromptSpecification :: Lens.Lens' SlotSummary (Prelude.Maybe PromptSpecification)
slotSummary_valueElicitationPromptSpecification = Lens.lens (\SlotSummary' {valueElicitationPromptSpecification} -> valueElicitationPromptSpecification) (\s@SlotSummary' {} a -> s {valueElicitationPromptSpecification = a} :: SlotSummary)

instance Data.FromJSON SlotSummary where
  parseJSON =
    Data.withObject
      "SlotSummary"
      ( \x ->
          SlotSummary'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "slotConstraint")
            Prelude.<*> (x Data..:? "slotId")
            Prelude.<*> (x Data..:? "slotName")
            Prelude.<*> (x Data..:? "slotTypeId")
            Prelude.<*> (x Data..:? "valueElicitationPromptSpecification")
      )

instance Prelude.Hashable SlotSummary where
  hashWithSalt _salt SlotSummary' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` slotConstraint
      `Prelude.hashWithSalt` slotId
      `Prelude.hashWithSalt` slotName
      `Prelude.hashWithSalt` slotTypeId
      `Prelude.hashWithSalt` valueElicitationPromptSpecification

instance Prelude.NFData SlotSummary where
  rnf SlotSummary' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
      `Prelude.seq` Prelude.rnf slotConstraint
      `Prelude.seq` Prelude.rnf slotId
      `Prelude.seq` Prelude.rnf slotName
      `Prelude.seq` Prelude.rnf slotTypeId
      `Prelude.seq` Prelude.rnf valueElicitationPromptSpecification
