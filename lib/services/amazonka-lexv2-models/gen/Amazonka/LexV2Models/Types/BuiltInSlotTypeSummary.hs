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
-- Module      : Amazonka.LexV2Models.Types.BuiltInSlotTypeSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexV2Models.Types.BuiltInSlotTypeSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides summary information about a built-in slot type for the
-- <https://docs.aws.amazon.com/lexv2/latest/APIReference/API_ListBuiltInSlotTypes.html ListBuiltInSlotTypes>
-- operation.
--
-- /See:/ 'newBuiltInSlotTypeSummary' smart constructor.
data BuiltInSlotTypeSummary = BuiltInSlotTypeSummary'
  { -- | The description of the built-in slot type.
    description :: Prelude.Maybe Prelude.Text,
    -- | The signature of the built-in slot type. Use this to specify the parent
    -- slot type of a derived slot type.
    slotTypeSignature :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BuiltInSlotTypeSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'builtInSlotTypeSummary_description' - The description of the built-in slot type.
--
-- 'slotTypeSignature', 'builtInSlotTypeSummary_slotTypeSignature' - The signature of the built-in slot type. Use this to specify the parent
-- slot type of a derived slot type.
newBuiltInSlotTypeSummary ::
  BuiltInSlotTypeSummary
newBuiltInSlotTypeSummary =
  BuiltInSlotTypeSummary'
    { description =
        Prelude.Nothing,
      slotTypeSignature = Prelude.Nothing
    }

-- | The description of the built-in slot type.
builtInSlotTypeSummary_description :: Lens.Lens' BuiltInSlotTypeSummary (Prelude.Maybe Prelude.Text)
builtInSlotTypeSummary_description = Lens.lens (\BuiltInSlotTypeSummary' {description} -> description) (\s@BuiltInSlotTypeSummary' {} a -> s {description = a} :: BuiltInSlotTypeSummary)

-- | The signature of the built-in slot type. Use this to specify the parent
-- slot type of a derived slot type.
builtInSlotTypeSummary_slotTypeSignature :: Lens.Lens' BuiltInSlotTypeSummary (Prelude.Maybe Prelude.Text)
builtInSlotTypeSummary_slotTypeSignature = Lens.lens (\BuiltInSlotTypeSummary' {slotTypeSignature} -> slotTypeSignature) (\s@BuiltInSlotTypeSummary' {} a -> s {slotTypeSignature = a} :: BuiltInSlotTypeSummary)

instance Data.FromJSON BuiltInSlotTypeSummary where
  parseJSON =
    Data.withObject
      "BuiltInSlotTypeSummary"
      ( \x ->
          BuiltInSlotTypeSummary'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "slotTypeSignature")
      )

instance Prelude.Hashable BuiltInSlotTypeSummary where
  hashWithSalt _salt BuiltInSlotTypeSummary' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` slotTypeSignature

instance Prelude.NFData BuiltInSlotTypeSummary where
  rnf BuiltInSlotTypeSummary' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf slotTypeSignature
