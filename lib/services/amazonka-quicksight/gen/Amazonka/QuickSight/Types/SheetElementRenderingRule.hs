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
-- Module      : Amazonka.QuickSight.Types.SheetElementRenderingRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.SheetElementRenderingRule where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.SheetElementConfigurationOverrides

-- | The rendering rules of a sheet that uses a free-form layout.
--
-- /See:/ 'newSheetElementRenderingRule' smart constructor.
data SheetElementRenderingRule = SheetElementRenderingRule'
  { -- | The expression of the rendering rules of a sheet.
    expression :: Data.Sensitive Prelude.Text,
    -- | The override configuration of the rendering rules of a sheet.
    configurationOverrides :: SheetElementConfigurationOverrides
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SheetElementRenderingRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expression', 'sheetElementRenderingRule_expression' - The expression of the rendering rules of a sheet.
--
-- 'configurationOverrides', 'sheetElementRenderingRule_configurationOverrides' - The override configuration of the rendering rules of a sheet.
newSheetElementRenderingRule ::
  -- | 'expression'
  Prelude.Text ->
  -- | 'configurationOverrides'
  SheetElementConfigurationOverrides ->
  SheetElementRenderingRule
newSheetElementRenderingRule
  pExpression_
  pConfigurationOverrides_ =
    SheetElementRenderingRule'
      { expression =
          Data._Sensitive Lens.# pExpression_,
        configurationOverrides =
          pConfigurationOverrides_
      }

-- | The expression of the rendering rules of a sheet.
sheetElementRenderingRule_expression :: Lens.Lens' SheetElementRenderingRule Prelude.Text
sheetElementRenderingRule_expression = Lens.lens (\SheetElementRenderingRule' {expression} -> expression) (\s@SheetElementRenderingRule' {} a -> s {expression = a} :: SheetElementRenderingRule) Prelude.. Data._Sensitive

-- | The override configuration of the rendering rules of a sheet.
sheetElementRenderingRule_configurationOverrides :: Lens.Lens' SheetElementRenderingRule SheetElementConfigurationOverrides
sheetElementRenderingRule_configurationOverrides = Lens.lens (\SheetElementRenderingRule' {configurationOverrides} -> configurationOverrides) (\s@SheetElementRenderingRule' {} a -> s {configurationOverrides = a} :: SheetElementRenderingRule)

instance Data.FromJSON SheetElementRenderingRule where
  parseJSON =
    Data.withObject
      "SheetElementRenderingRule"
      ( \x ->
          SheetElementRenderingRule'
            Prelude.<$> (x Data..: "Expression")
            Prelude.<*> (x Data..: "ConfigurationOverrides")
      )

instance Prelude.Hashable SheetElementRenderingRule where
  hashWithSalt _salt SheetElementRenderingRule' {..} =
    _salt `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` configurationOverrides

instance Prelude.NFData SheetElementRenderingRule where
  rnf SheetElementRenderingRule' {..} =
    Prelude.rnf expression
      `Prelude.seq` Prelude.rnf configurationOverrides

instance Data.ToJSON SheetElementRenderingRule where
  toJSON SheetElementRenderingRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Expression" Data..= expression),
            Prelude.Just
              ( "ConfigurationOverrides"
                  Data..= configurationOverrides
              )
          ]
      )
