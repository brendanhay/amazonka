{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CloudSearch.Types.TextOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.TextOptions where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Options for text field. Present if @IndexFieldType@ specifies the field
-- is of type @text@. A @text@ field is always searchable. All options are
-- enabled by default.
--
-- /See:/ 'newTextOptions' smart constructor.
data TextOptions = TextOptions'
  { -- | Whether the field can be used to sort the search results.
    sortEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of an analysis scheme for a @text@ field.
    analysisScheme :: Prelude.Maybe Prelude.Text,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Prelude.Maybe Prelude.Bool,
    sourceField :: Prelude.Maybe Prelude.Text,
    -- | A value to use for the field if the field isn\'t specified for a
    -- document.
    defaultValue :: Prelude.Maybe Prelude.Text,
    -- | Whether highlights can be returned for the field.
    highlightEnabled :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TextOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortEnabled', 'textOptions_sortEnabled' - Whether the field can be used to sort the search results.
--
-- 'analysisScheme', 'textOptions_analysisScheme' - The name of an analysis scheme for a @text@ field.
--
-- 'returnEnabled', 'textOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'sourceField', 'textOptions_sourceField' - Undocumented member.
--
-- 'defaultValue', 'textOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document.
--
-- 'highlightEnabled', 'textOptions_highlightEnabled' - Whether highlights can be returned for the field.
newTextOptions ::
  TextOptions
newTextOptions =
  TextOptions'
    { sortEnabled = Prelude.Nothing,
      analysisScheme = Prelude.Nothing,
      returnEnabled = Prelude.Nothing,
      sourceField = Prelude.Nothing,
      defaultValue = Prelude.Nothing,
      highlightEnabled = Prelude.Nothing
    }

-- | Whether the field can be used to sort the search results.
textOptions_sortEnabled :: Lens.Lens' TextOptions (Prelude.Maybe Prelude.Bool)
textOptions_sortEnabled = Lens.lens (\TextOptions' {sortEnabled} -> sortEnabled) (\s@TextOptions' {} a -> s {sortEnabled = a} :: TextOptions)

-- | The name of an analysis scheme for a @text@ field.
textOptions_analysisScheme :: Lens.Lens' TextOptions (Prelude.Maybe Prelude.Text)
textOptions_analysisScheme = Lens.lens (\TextOptions' {analysisScheme} -> analysisScheme) (\s@TextOptions' {} a -> s {analysisScheme = a} :: TextOptions)

-- | Whether the contents of the field can be returned in the search results.
textOptions_returnEnabled :: Lens.Lens' TextOptions (Prelude.Maybe Prelude.Bool)
textOptions_returnEnabled = Lens.lens (\TextOptions' {returnEnabled} -> returnEnabled) (\s@TextOptions' {} a -> s {returnEnabled = a} :: TextOptions)

-- | Undocumented member.
textOptions_sourceField :: Lens.Lens' TextOptions (Prelude.Maybe Prelude.Text)
textOptions_sourceField = Lens.lens (\TextOptions' {sourceField} -> sourceField) (\s@TextOptions' {} a -> s {sourceField = a} :: TextOptions)

-- | A value to use for the field if the field isn\'t specified for a
-- document.
textOptions_defaultValue :: Lens.Lens' TextOptions (Prelude.Maybe Prelude.Text)
textOptions_defaultValue = Lens.lens (\TextOptions' {defaultValue} -> defaultValue) (\s@TextOptions' {} a -> s {defaultValue = a} :: TextOptions)

-- | Whether highlights can be returned for the field.
textOptions_highlightEnabled :: Lens.Lens' TextOptions (Prelude.Maybe Prelude.Bool)
textOptions_highlightEnabled = Lens.lens (\TextOptions' {highlightEnabled} -> highlightEnabled) (\s@TextOptions' {} a -> s {highlightEnabled = a} :: TextOptions)

instance Prelude.FromXML TextOptions where
  parseXML x =
    TextOptions'
      Prelude.<$> (x Prelude..@? "SortEnabled")
      Prelude.<*> (x Prelude..@? "AnalysisScheme")
      Prelude.<*> (x Prelude..@? "ReturnEnabled")
      Prelude.<*> (x Prelude..@? "SourceField")
      Prelude.<*> (x Prelude..@? "DefaultValue")
      Prelude.<*> (x Prelude..@? "HighlightEnabled")

instance Prelude.Hashable TextOptions

instance Prelude.NFData TextOptions

instance Prelude.ToQuery TextOptions where
  toQuery TextOptions' {..} =
    Prelude.mconcat
      [ "SortEnabled" Prelude.=: sortEnabled,
        "AnalysisScheme" Prelude.=: analysisScheme,
        "ReturnEnabled" Prelude.=: returnEnabled,
        "SourceField" Prelude.=: sourceField,
        "DefaultValue" Prelude.=: defaultValue,
        "HighlightEnabled" Prelude.=: highlightEnabled
      ]
