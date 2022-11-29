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
-- Module      : Amazonka.LexV2Models.Types.SlotTypeSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.SlotTypeSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LexV2Models.Types.SlotTypeCategory
import qualified Amazonka.Prelude as Prelude

-- | Provides summary information about a slot type.
--
-- /See:/ 'newSlotTypeSummary' smart constructor.
data SlotTypeSummary = SlotTypeSummary'
  { -- | Indicates the type of the slot type.
    --
    -- -   @Custom@ - A slot type that you created using custom values. For
    --     more information, see
    --     <https://docs.aws.amazon.com/lexv2/latest/dg/custom-slot-types.html Creating custom slot types>.
    --
    -- -   @Extended@ - A slot type created by extending the
    --     AMAZON.AlphaNumeric built-in slot type. For more information, see
    --     <https://docs.aws.amazon.com/lexv2/latest/dg/built-in-slot-alphanumerice.html AMAZON.AlphaNumeric>.
    --
    -- -   @ExternalGrammar@ - A slot type using a custom GRXML grammar to
    --     define values. For more information, see
    --     <https://docs.aws.amazon.com/lexv2/latest/dg/building-grxml.html Using a custom grammar slot type>.
    slotTypeCategory :: Prelude.Maybe SlotTypeCategory,
    -- | The description of the slot type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the slot type.
    slotTypeName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier assigned to the slot type.
    slotTypeId :: Prelude.Maybe Prelude.Text,
    -- | If the slot type is derived from a built-on slot type, the name of the
    -- parent slot type.
    parentSlotTypeSignature :: Prelude.Maybe Prelude.Text,
    -- | A timestamp of the date and time that the slot type was last updated.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX
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
-- 'slotTypeCategory', 'slotTypeSummary_slotTypeCategory' - Indicates the type of the slot type.
--
-- -   @Custom@ - A slot type that you created using custom values. For
--     more information, see
--     <https://docs.aws.amazon.com/lexv2/latest/dg/custom-slot-types.html Creating custom slot types>.
--
-- -   @Extended@ - A slot type created by extending the
--     AMAZON.AlphaNumeric built-in slot type. For more information, see
--     <https://docs.aws.amazon.com/lexv2/latest/dg/built-in-slot-alphanumerice.html AMAZON.AlphaNumeric>.
--
-- -   @ExternalGrammar@ - A slot type using a custom GRXML grammar to
--     define values. For more information, see
--     <https://docs.aws.amazon.com/lexv2/latest/dg/building-grxml.html Using a custom grammar slot type>.
--
-- 'description', 'slotTypeSummary_description' - The description of the slot type.
--
-- 'slotTypeName', 'slotTypeSummary_slotTypeName' - The name of the slot type.
--
-- 'slotTypeId', 'slotTypeSummary_slotTypeId' - The unique identifier assigned to the slot type.
--
-- 'parentSlotTypeSignature', 'slotTypeSummary_parentSlotTypeSignature' - If the slot type is derived from a built-on slot type, the name of the
-- parent slot type.
--
-- 'lastUpdatedDateTime', 'slotTypeSummary_lastUpdatedDateTime' - A timestamp of the date and time that the slot type was last updated.
newSlotTypeSummary ::
  SlotTypeSummary
newSlotTypeSummary =
  SlotTypeSummary'
    { slotTypeCategory =
        Prelude.Nothing,
      description = Prelude.Nothing,
      slotTypeName = Prelude.Nothing,
      slotTypeId = Prelude.Nothing,
      parentSlotTypeSignature = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing
    }

-- | Indicates the type of the slot type.
--
-- -   @Custom@ - A slot type that you created using custom values. For
--     more information, see
--     <https://docs.aws.amazon.com/lexv2/latest/dg/custom-slot-types.html Creating custom slot types>.
--
-- -   @Extended@ - A slot type created by extending the
--     AMAZON.AlphaNumeric built-in slot type. For more information, see
--     <https://docs.aws.amazon.com/lexv2/latest/dg/built-in-slot-alphanumerice.html AMAZON.AlphaNumeric>.
--
-- -   @ExternalGrammar@ - A slot type using a custom GRXML grammar to
--     define values. For more information, see
--     <https://docs.aws.amazon.com/lexv2/latest/dg/building-grxml.html Using a custom grammar slot type>.
slotTypeSummary_slotTypeCategory :: Lens.Lens' SlotTypeSummary (Prelude.Maybe SlotTypeCategory)
slotTypeSummary_slotTypeCategory = Lens.lens (\SlotTypeSummary' {slotTypeCategory} -> slotTypeCategory) (\s@SlotTypeSummary' {} a -> s {slotTypeCategory = a} :: SlotTypeSummary)

-- | The description of the slot type.
slotTypeSummary_description :: Lens.Lens' SlotTypeSummary (Prelude.Maybe Prelude.Text)
slotTypeSummary_description = Lens.lens (\SlotTypeSummary' {description} -> description) (\s@SlotTypeSummary' {} a -> s {description = a} :: SlotTypeSummary)

-- | The name of the slot type.
slotTypeSummary_slotTypeName :: Lens.Lens' SlotTypeSummary (Prelude.Maybe Prelude.Text)
slotTypeSummary_slotTypeName = Lens.lens (\SlotTypeSummary' {slotTypeName} -> slotTypeName) (\s@SlotTypeSummary' {} a -> s {slotTypeName = a} :: SlotTypeSummary)

-- | The unique identifier assigned to the slot type.
slotTypeSummary_slotTypeId :: Lens.Lens' SlotTypeSummary (Prelude.Maybe Prelude.Text)
slotTypeSummary_slotTypeId = Lens.lens (\SlotTypeSummary' {slotTypeId} -> slotTypeId) (\s@SlotTypeSummary' {} a -> s {slotTypeId = a} :: SlotTypeSummary)

-- | If the slot type is derived from a built-on slot type, the name of the
-- parent slot type.
slotTypeSummary_parentSlotTypeSignature :: Lens.Lens' SlotTypeSummary (Prelude.Maybe Prelude.Text)
slotTypeSummary_parentSlotTypeSignature = Lens.lens (\SlotTypeSummary' {parentSlotTypeSignature} -> parentSlotTypeSignature) (\s@SlotTypeSummary' {} a -> s {parentSlotTypeSignature = a} :: SlotTypeSummary)

-- | A timestamp of the date and time that the slot type was last updated.
slotTypeSummary_lastUpdatedDateTime :: Lens.Lens' SlotTypeSummary (Prelude.Maybe Prelude.UTCTime)
slotTypeSummary_lastUpdatedDateTime = Lens.lens (\SlotTypeSummary' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@SlotTypeSummary' {} a -> s {lastUpdatedDateTime = a} :: SlotTypeSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON SlotTypeSummary where
  parseJSON =
    Core.withObject
      "SlotTypeSummary"
      ( \x ->
          SlotTypeSummary'
            Prelude.<$> (x Core..:? "slotTypeCategory")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "slotTypeName")
            Prelude.<*> (x Core..:? "slotTypeId")
            Prelude.<*> (x Core..:? "parentSlotTypeSignature")
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
      )

instance Prelude.Hashable SlotTypeSummary where
  hashWithSalt _salt SlotTypeSummary' {..} =
    _salt `Prelude.hashWithSalt` slotTypeCategory
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` slotTypeName
      `Prelude.hashWithSalt` slotTypeId
      `Prelude.hashWithSalt` parentSlotTypeSignature
      `Prelude.hashWithSalt` lastUpdatedDateTime

instance Prelude.NFData SlotTypeSummary where
  rnf SlotTypeSummary' {..} =
    Prelude.rnf slotTypeCategory
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf slotTypeName
      `Prelude.seq` Prelude.rnf slotTypeId
      `Prelude.seq` Prelude.rnf parentSlotTypeSignature
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
