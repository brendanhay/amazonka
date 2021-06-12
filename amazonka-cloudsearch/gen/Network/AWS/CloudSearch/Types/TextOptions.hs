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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Options for text field. Present if @IndexFieldType@ specifies the field
-- is of type @text@. A @text@ field is always searchable. All options are
-- enabled by default.
--
-- /See:/ 'newTextOptions' smart constructor.
data TextOptions = TextOptions'
  { -- | Whether the field can be used to sort the search results.
    sortEnabled :: Core.Maybe Core.Bool,
    -- | The name of an analysis scheme for a @text@ field.
    analysisScheme :: Core.Maybe Core.Text,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Core.Maybe Core.Bool,
    sourceField :: Core.Maybe Core.Text,
    -- | A value to use for the field if the field isn\'t specified for a
    -- document.
    defaultValue :: Core.Maybe Core.Text,
    -- | Whether highlights can be returned for the field.
    highlightEnabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { sortEnabled = Core.Nothing,
      analysisScheme = Core.Nothing,
      returnEnabled = Core.Nothing,
      sourceField = Core.Nothing,
      defaultValue = Core.Nothing,
      highlightEnabled = Core.Nothing
    }

-- | Whether the field can be used to sort the search results.
textOptions_sortEnabled :: Lens.Lens' TextOptions (Core.Maybe Core.Bool)
textOptions_sortEnabled = Lens.lens (\TextOptions' {sortEnabled} -> sortEnabled) (\s@TextOptions' {} a -> s {sortEnabled = a} :: TextOptions)

-- | The name of an analysis scheme for a @text@ field.
textOptions_analysisScheme :: Lens.Lens' TextOptions (Core.Maybe Core.Text)
textOptions_analysisScheme = Lens.lens (\TextOptions' {analysisScheme} -> analysisScheme) (\s@TextOptions' {} a -> s {analysisScheme = a} :: TextOptions)

-- | Whether the contents of the field can be returned in the search results.
textOptions_returnEnabled :: Lens.Lens' TextOptions (Core.Maybe Core.Bool)
textOptions_returnEnabled = Lens.lens (\TextOptions' {returnEnabled} -> returnEnabled) (\s@TextOptions' {} a -> s {returnEnabled = a} :: TextOptions)

-- | Undocumented member.
textOptions_sourceField :: Lens.Lens' TextOptions (Core.Maybe Core.Text)
textOptions_sourceField = Lens.lens (\TextOptions' {sourceField} -> sourceField) (\s@TextOptions' {} a -> s {sourceField = a} :: TextOptions)

-- | A value to use for the field if the field isn\'t specified for a
-- document.
textOptions_defaultValue :: Lens.Lens' TextOptions (Core.Maybe Core.Text)
textOptions_defaultValue = Lens.lens (\TextOptions' {defaultValue} -> defaultValue) (\s@TextOptions' {} a -> s {defaultValue = a} :: TextOptions)

-- | Whether highlights can be returned for the field.
textOptions_highlightEnabled :: Lens.Lens' TextOptions (Core.Maybe Core.Bool)
textOptions_highlightEnabled = Lens.lens (\TextOptions' {highlightEnabled} -> highlightEnabled) (\s@TextOptions' {} a -> s {highlightEnabled = a} :: TextOptions)

instance Core.FromXML TextOptions where
  parseXML x =
    TextOptions'
      Core.<$> (x Core..@? "SortEnabled")
      Core.<*> (x Core..@? "AnalysisScheme")
      Core.<*> (x Core..@? "ReturnEnabled")
      Core.<*> (x Core..@? "SourceField")
      Core.<*> (x Core..@? "DefaultValue")
      Core.<*> (x Core..@? "HighlightEnabled")

instance Core.Hashable TextOptions

instance Core.NFData TextOptions

instance Core.ToQuery TextOptions where
  toQuery TextOptions' {..} =
    Core.mconcat
      [ "SortEnabled" Core.=: sortEnabled,
        "AnalysisScheme" Core.=: analysisScheme,
        "ReturnEnabled" Core.=: returnEnabled,
        "SourceField" Core.=: sourceField,
        "DefaultValue" Core.=: defaultValue,
        "HighlightEnabled" Core.=: highlightEnabled
      ]
