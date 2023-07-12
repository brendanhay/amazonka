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
-- Module      : Amazonka.QuickSight.Types.KPIConditionalFormatting
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.KPIConditionalFormatting where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.KPIConditionalFormattingOption

-- | The conditional formatting of a KPI visual.
--
-- /See:/ 'newKPIConditionalFormatting' smart constructor.
data KPIConditionalFormatting = KPIConditionalFormatting'
  { -- | The conditional formatting options of a KPI visual.
    conditionalFormattingOptions :: Prelude.Maybe [KPIConditionalFormattingOption]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'KPIConditionalFormatting' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'conditionalFormattingOptions', 'kPIConditionalFormatting_conditionalFormattingOptions' - The conditional formatting options of a KPI visual.
newKPIConditionalFormatting ::
  KPIConditionalFormatting
newKPIConditionalFormatting =
  KPIConditionalFormatting'
    { conditionalFormattingOptions =
        Prelude.Nothing
    }

-- | The conditional formatting options of a KPI visual.
kPIConditionalFormatting_conditionalFormattingOptions :: Lens.Lens' KPIConditionalFormatting (Prelude.Maybe [KPIConditionalFormattingOption])
kPIConditionalFormatting_conditionalFormattingOptions = Lens.lens (\KPIConditionalFormatting' {conditionalFormattingOptions} -> conditionalFormattingOptions) (\s@KPIConditionalFormatting' {} a -> s {conditionalFormattingOptions = a} :: KPIConditionalFormatting) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON KPIConditionalFormatting where
  parseJSON =
    Data.withObject
      "KPIConditionalFormatting"
      ( \x ->
          KPIConditionalFormatting'
            Prelude.<$> ( x
                            Data..:? "ConditionalFormattingOptions"
                            Data..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable KPIConditionalFormatting where
  hashWithSalt _salt KPIConditionalFormatting' {..} =
    _salt
      `Prelude.hashWithSalt` conditionalFormattingOptions

instance Prelude.NFData KPIConditionalFormatting where
  rnf KPIConditionalFormatting' {..} =
    Prelude.rnf conditionalFormattingOptions

instance Data.ToJSON KPIConditionalFormatting where
  toJSON KPIConditionalFormatting' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ConditionalFormattingOptions" Data..=)
              Prelude.<$> conditionalFormattingOptions
          ]
      )
