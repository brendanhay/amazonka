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
-- Module      : Network.AWS.LexV2Models.Types.SlotTypeSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.LexV2Models.Types.SlotTypeSummary where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Provides summary information about a slot type.
--
-- /See:/ 'newSlotTypeSummary' smart constructor.
data SlotTypeSummary = SlotTypeSummary'
  { -- | If the slot type is derived from a built-on slot type, the name of the
    -- parent slot type.
    parentSlotTypeSignature :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the slot type was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX,
    -- | The name of the slot type.
    slotTypeName :: Prelude.Maybe Prelude.Text,
    -- | The description of the slot type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier assigned to the slot type.
    slotTypeId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotTypeSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parentSlotTypeSignature', 'slotTypeSummary_parentSlotTypeSignature' - If the slot type is derived from a built-on slot type, the name of the
-- parent slot type.
--
-- 'lastUpdatedDateTime', 'slotTypeSummary_lastUpdatedDateTime' - A timestamp of the date and time that the slot type was last updated.
--
-- 'slotTypeName', 'slotTypeSummary_slotTypeName' - The name of the slot type.
--
-- 'description', 'slotTypeSummary_description' - The description of the slot type.
--
-- 'slotTypeId', 'slotTypeSummary_slotTypeId' - The unique identifier assigned to the slot type.
newSlotTypeSummary ::
  SlotTypeSummary
newSlotTypeSummary =
  SlotTypeSummary'
    { parentSlotTypeSignature =
        Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      slotTypeName = Prelude.Nothing,
      description = Prelude.Nothing,
      slotTypeId = Prelude.Nothing
    }

-- | If the slot type is derived from a built-on slot type, the name of the
-- parent slot type.
slotTypeSummary_parentSlotTypeSignature :: Lens.Lens' SlotTypeSummary (Prelude.Maybe Prelude.Text)
slotTypeSummary_parentSlotTypeSignature = Lens.lens (\SlotTypeSummary' {parentSlotTypeSignature} -> parentSlotTypeSignature) (\s@SlotTypeSummary' {} a -> s {parentSlotTypeSignature = a} :: SlotTypeSummary)

-- | A timestamp of the date and time that the slot type was last updated.
slotTypeSummary_lastUpdatedDateTime :: Lens.Lens' SlotTypeSummary (Prelude.Maybe Prelude.UTCTime)
slotTypeSummary_lastUpdatedDateTime = Lens.lens (\SlotTypeSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@SlotTypeSummary' {} a -> s {lastUpdatedDateTime = a} :: SlotTypeSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the slot type.
slotTypeSummary_slotTypeName :: Lens.Lens' SlotTypeSummary (Prelude.Maybe Prelude.Text)
slotTypeSummary_slotTypeName = Lens.lens (\SlotTypeSummary' {slotTypeName} -> slotTypeName) (\s@SlotTypeSummary' {} a -> s {slotTypeName = a} :: SlotTypeSummary)

-- | The description of the slot type.
slotTypeSummary_description :: Lens.Lens' SlotTypeSummary (Prelude.Maybe Prelude.Text)
slotTypeSummary_description = Lens.lens (\SlotTypeSummary' {description} -> description) (\s@SlotTypeSummary' {} a -> s {description = a} :: SlotTypeSummary)

-- | The unique identifier assigned to the slot type.
slotTypeSummary_slotTypeId :: Lens.Lens' SlotTypeSummary (Prelude.Maybe Prelude.Text)
slotTypeSummary_slotTypeId = Lens.lens (\SlotTypeSummary' {slotTypeId} -> slotTypeId) (\s@SlotTypeSummary' {} a -> s {slotTypeId = a} :: SlotTypeSummary)

instance Core.FromJSON SlotTypeSummary where
  parseJSON =
    Core.withObject
      "SlotTypeSummary"
      ( \x ->
          SlotTypeSummary'
            Prelude.<$> (x Core..:? "parentSlotTypeSignature")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
            Prelude.<*> (x Core..:? "slotTypeName")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "slotTypeId")
      )

instance Prelude.Hashable SlotTypeSummary

instance Prelude.NFData SlotTypeSummary
