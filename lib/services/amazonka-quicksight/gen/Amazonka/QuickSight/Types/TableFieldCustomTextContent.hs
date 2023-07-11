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
-- Module      : Amazonka.QuickSight.Types.TableFieldCustomTextContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.TableFieldCustomTextContent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.FontConfiguration

-- | The custom text content (value, font configuration) for the table link
-- content configuration.
--
-- /See:/ 'newTableFieldCustomTextContent' smart constructor.
data TableFieldCustomTextContent = TableFieldCustomTextContent'
  { -- | The string value of the custom text content for the table URL link
    -- content.
    value :: Prelude.Maybe Prelude.Text,
    -- | The font configuration of the custom text content for the table URL link
    -- content.
    fontConfiguration :: FontConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableFieldCustomTextContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'tableFieldCustomTextContent_value' - The string value of the custom text content for the table URL link
-- content.
--
-- 'fontConfiguration', 'tableFieldCustomTextContent_fontConfiguration' - The font configuration of the custom text content for the table URL link
-- content.
newTableFieldCustomTextContent ::
  -- | 'fontConfiguration'
  FontConfiguration ->
  TableFieldCustomTextContent
newTableFieldCustomTextContent pFontConfiguration_ =
  TableFieldCustomTextContent'
    { value =
        Prelude.Nothing,
      fontConfiguration = pFontConfiguration_
    }

-- | The string value of the custom text content for the table URL link
-- content.
tableFieldCustomTextContent_value :: Lens.Lens' TableFieldCustomTextContent (Prelude.Maybe Prelude.Text)
tableFieldCustomTextContent_value = Lens.lens (\TableFieldCustomTextContent' {value} -> value) (\s@TableFieldCustomTextContent' {} a -> s {value = a} :: TableFieldCustomTextContent)

-- | The font configuration of the custom text content for the table URL link
-- content.
tableFieldCustomTextContent_fontConfiguration :: Lens.Lens' TableFieldCustomTextContent FontConfiguration
tableFieldCustomTextContent_fontConfiguration = Lens.lens (\TableFieldCustomTextContent' {fontConfiguration} -> fontConfiguration) (\s@TableFieldCustomTextContent' {} a -> s {fontConfiguration = a} :: TableFieldCustomTextContent)

instance Data.FromJSON TableFieldCustomTextContent where
  parseJSON =
    Data.withObject
      "TableFieldCustomTextContent"
      ( \x ->
          TableFieldCustomTextContent'
            Prelude.<$> (x Data..:? "Value")
            Prelude.<*> (x Data..: "FontConfiguration")
      )

instance Prelude.Hashable TableFieldCustomTextContent where
  hashWithSalt _salt TableFieldCustomTextContent' {..} =
    _salt
      `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` fontConfiguration

instance Prelude.NFData TableFieldCustomTextContent where
  rnf TableFieldCustomTextContent' {..} =
    Prelude.rnf value
      `Prelude.seq` Prelude.rnf fontConfiguration

instance Data.ToJSON TableFieldCustomTextContent where
  toJSON TableFieldCustomTextContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Value" Data..=) Prelude.<$> value,
            Prelude.Just
              ("FontConfiguration" Data..= fontConfiguration)
          ]
      )
