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
-- Module      : Amazonka.LexModels.Types.SlotTypeConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LexModels.Types.SlotTypeConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.LexModels.Types.SlotTypeRegexConfiguration
import qualified Amazonka.Prelude as Prelude

-- | Provides configuration information for a slot type.
--
-- /See:/ 'newSlotTypeConfiguration' smart constructor.
data SlotTypeConfiguration = SlotTypeConfiguration'
  { -- | A regular expression used to validate the value of a slot.
    regexConfiguration :: Prelude.Maybe SlotTypeRegexConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SlotTypeConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'regexConfiguration', 'slotTypeConfiguration_regexConfiguration' - A regular expression used to validate the value of a slot.
newSlotTypeConfiguration ::
  SlotTypeConfiguration
newSlotTypeConfiguration =
  SlotTypeConfiguration'
    { regexConfiguration =
        Prelude.Nothing
    }

-- | A regular expression used to validate the value of a slot.
slotTypeConfiguration_regexConfiguration :: Lens.Lens' SlotTypeConfiguration (Prelude.Maybe SlotTypeRegexConfiguration)
slotTypeConfiguration_regexConfiguration = Lens.lens (\SlotTypeConfiguration' {regexConfiguration} -> regexConfiguration) (\s@SlotTypeConfiguration' {} a -> s {regexConfiguration = a} :: SlotTypeConfiguration)

instance Data.FromJSON SlotTypeConfiguration where
  parseJSON =
    Data.withObject
      "SlotTypeConfiguration"
      ( \x ->
          SlotTypeConfiguration'
            Prelude.<$> (x Data..:? "regexConfiguration")
      )

instance Prelude.Hashable SlotTypeConfiguration where
  hashWithSalt _salt SlotTypeConfiguration' {..} =
    _salt `Prelude.hashWithSalt` regexConfiguration

instance Prelude.NFData SlotTypeConfiguration where
  rnf SlotTypeConfiguration' {..} =
    Prelude.rnf regexConfiguration

instance Data.ToJSON SlotTypeConfiguration where
  toJSON SlotTypeConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("regexConfiguration" Data..=)
              Prelude.<$> regexConfiguration
          ]
      )
