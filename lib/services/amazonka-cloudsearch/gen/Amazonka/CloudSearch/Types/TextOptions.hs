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
-- Module      : Amazonka.CloudSearch.Types.TextOptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearch.Types.TextOptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Options for text field. Present if @IndexFieldType@ specifies the field
-- is of type @text@. A @text@ field is always searchable. All options are
-- enabled by default.
--
-- /See:/ 'newTextOptions' smart constructor.
data TextOptions = TextOptions'
  { sourceField :: Prelude.Maybe Prelude.Text,
    -- | The name of an analysis scheme for a @text@ field.
    analysisScheme :: Prelude.Maybe Prelude.Text,
    -- | Whether the field can be used to sort the search results.
    sortEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A value to use for the field if the field isn\'t specified for a
    -- document.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether highlights can be returned for the field.
    highlightEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TextOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceField', 'textOptions_sourceField' - Undocumented member.
--
-- 'analysisScheme', 'textOptions_analysisScheme' - The name of an analysis scheme for a @text@ field.
--
-- 'sortEnabled', 'textOptions_sortEnabled' - Whether the field can be used to sort the search results.
--
-- 'defaultValue', 'textOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document.
--
-- 'returnEnabled', 'textOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'highlightEnabled', 'textOptions_highlightEnabled' - Whether highlights can be returned for the field.
newTextOptions ::
  TextOptions
newTextOptions =
  TextOptions'
    { sourceField = Prelude.Nothing,
      analysisScheme = Prelude.Nothing,
      sortEnabled = Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      returnEnabled = Prelude.Nothing,
      highlightEnabled = Prelude.Nothing
    }

-- | Undocumented member.
textOptions_sourceField :: Lens.Lens' TextOptions (Prelude.Maybe Prelude.Text)
textOptions_sourceField = Lens.lens (\TextOptions' {sourceField} -> sourceField) (\s@TextOptions' {} a -> s {sourceField = a} :: TextOptions)

-- | The name of an analysis scheme for a @text@ field.
textOptions_analysisScheme :: Lens.Lens' TextOptions (Prelude.Maybe Prelude.Text)
textOptions_analysisScheme = Lens.lens (\TextOptions' {analysisScheme} -> analysisScheme) (\s@TextOptions' {} a -> s {analysisScheme = a} :: TextOptions)

-- | Whether the field can be used to sort the search results.
textOptions_sortEnabled :: Lens.Lens' TextOptions (Prelude.Maybe Prelude.Bool)
textOptions_sortEnabled = Lens.lens (\TextOptions' {sortEnabled} -> sortEnabled) (\s@TextOptions' {} a -> s {sortEnabled = a} :: TextOptions)

-- | A value to use for the field if the field isn\'t specified for a
-- document.
textOptions_defaultValue :: Lens.Lens' TextOptions (Prelude.Maybe Prelude.Text)
textOptions_defaultValue = Lens.lens (\TextOptions' {defaultValue} -> defaultValue) (\s@TextOptions' {} a -> s {defaultValue = a} :: TextOptions)

-- | Whether the contents of the field can be returned in the search results.
textOptions_returnEnabled :: Lens.Lens' TextOptions (Prelude.Maybe Prelude.Bool)
textOptions_returnEnabled = Lens.lens (\TextOptions' {returnEnabled} -> returnEnabled) (\s@TextOptions' {} a -> s {returnEnabled = a} :: TextOptions)

-- | Whether highlights can be returned for the field.
textOptions_highlightEnabled :: Lens.Lens' TextOptions (Prelude.Maybe Prelude.Bool)
textOptions_highlightEnabled = Lens.lens (\TextOptions' {highlightEnabled} -> highlightEnabled) (\s@TextOptions' {} a -> s {highlightEnabled = a} :: TextOptions)

instance Data.FromXML TextOptions where
  parseXML x =
    TextOptions'
      Prelude.<$> (x Data..@? "SourceField")
      Prelude.<*> (x Data..@? "AnalysisScheme")
      Prelude.<*> (x Data..@? "SortEnabled")
      Prelude.<*> (x Data..@? "DefaultValue")
      Prelude.<*> (x Data..@? "ReturnEnabled")
      Prelude.<*> (x Data..@? "HighlightEnabled")

instance Prelude.Hashable TextOptions where
  hashWithSalt _salt TextOptions' {..} =
    _salt `Prelude.hashWithSalt` sourceField
      `Prelude.hashWithSalt` analysisScheme
      `Prelude.hashWithSalt` sortEnabled
      `Prelude.hashWithSalt` defaultValue
      `Prelude.hashWithSalt` returnEnabled
      `Prelude.hashWithSalt` highlightEnabled

instance Prelude.NFData TextOptions where
  rnf TextOptions' {..} =
    Prelude.rnf sourceField
      `Prelude.seq` Prelude.rnf analysisScheme
      `Prelude.seq` Prelude.rnf sortEnabled
      `Prelude.seq` Prelude.rnf defaultValue
      `Prelude.seq` Prelude.rnf returnEnabled
      `Prelude.seq` Prelude.rnf highlightEnabled

instance Data.ToQuery TextOptions where
  toQuery TextOptions' {..} =
    Prelude.mconcat
      [ "SourceField" Data.=: sourceField,
        "AnalysisScheme" Data.=: analysisScheme,
        "SortEnabled" Data.=: sortEnabled,
        "DefaultValue" Data.=: defaultValue,
        "ReturnEnabled" Data.=: returnEnabled,
        "HighlightEnabled" Data.=: highlightEnabled
      ]
