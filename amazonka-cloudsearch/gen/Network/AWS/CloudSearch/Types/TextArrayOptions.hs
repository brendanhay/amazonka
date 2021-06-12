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
-- Module      : Network.AWS.CloudSearch.Types.TextArrayOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.TextArrayOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Options for a field that contains an array of text strings. Present if
-- @IndexFieldType@ specifies the field is of type @text-array@. A
-- @text-array@ field is always searchable. All options are enabled by
-- default.
--
-- /See:/ 'newTextArrayOptions' smart constructor.
data TextArrayOptions = TextArrayOptions'
  { -- | The name of an analysis scheme for a @text-array@ field.
    analysisScheme :: Core.Maybe Core.Text,
    -- | A list of source fields to map to the field.
    sourceFields :: Core.Maybe Core.Text,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Core.Maybe Core.Bool,
    -- | A value to use for the field if the field isn\'t specified for a
    -- document.
    defaultValue :: Core.Maybe Core.Text,
    -- | Whether highlights can be returned for the field.
    highlightEnabled :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TextArrayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'analysisScheme', 'textArrayOptions_analysisScheme' - The name of an analysis scheme for a @text-array@ field.
--
-- 'sourceFields', 'textArrayOptions_sourceFields' - A list of source fields to map to the field.
--
-- 'returnEnabled', 'textArrayOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'defaultValue', 'textArrayOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document.
--
-- 'highlightEnabled', 'textArrayOptions_highlightEnabled' - Whether highlights can be returned for the field.
newTextArrayOptions ::
  TextArrayOptions
newTextArrayOptions =
  TextArrayOptions'
    { analysisScheme = Core.Nothing,
      sourceFields = Core.Nothing,
      returnEnabled = Core.Nothing,
      defaultValue = Core.Nothing,
      highlightEnabled = Core.Nothing
    }

-- | The name of an analysis scheme for a @text-array@ field.
textArrayOptions_analysisScheme :: Lens.Lens' TextArrayOptions (Core.Maybe Core.Text)
textArrayOptions_analysisScheme = Lens.lens (\TextArrayOptions' {analysisScheme} -> analysisScheme) (\s@TextArrayOptions' {} a -> s {analysisScheme = a} :: TextArrayOptions)

-- | A list of source fields to map to the field.
textArrayOptions_sourceFields :: Lens.Lens' TextArrayOptions (Core.Maybe Core.Text)
textArrayOptions_sourceFields = Lens.lens (\TextArrayOptions' {sourceFields} -> sourceFields) (\s@TextArrayOptions' {} a -> s {sourceFields = a} :: TextArrayOptions)

-- | Whether the contents of the field can be returned in the search results.
textArrayOptions_returnEnabled :: Lens.Lens' TextArrayOptions (Core.Maybe Core.Bool)
textArrayOptions_returnEnabled = Lens.lens (\TextArrayOptions' {returnEnabled} -> returnEnabled) (\s@TextArrayOptions' {} a -> s {returnEnabled = a} :: TextArrayOptions)

-- | A value to use for the field if the field isn\'t specified for a
-- document.
textArrayOptions_defaultValue :: Lens.Lens' TextArrayOptions (Core.Maybe Core.Text)
textArrayOptions_defaultValue = Lens.lens (\TextArrayOptions' {defaultValue} -> defaultValue) (\s@TextArrayOptions' {} a -> s {defaultValue = a} :: TextArrayOptions)

-- | Whether highlights can be returned for the field.
textArrayOptions_highlightEnabled :: Lens.Lens' TextArrayOptions (Core.Maybe Core.Bool)
textArrayOptions_highlightEnabled = Lens.lens (\TextArrayOptions' {highlightEnabled} -> highlightEnabled) (\s@TextArrayOptions' {} a -> s {highlightEnabled = a} :: TextArrayOptions)

instance Core.FromXML TextArrayOptions where
  parseXML x =
    TextArrayOptions'
      Core.<$> (x Core..@? "AnalysisScheme")
      Core.<*> (x Core..@? "SourceFields")
      Core.<*> (x Core..@? "ReturnEnabled")
      Core.<*> (x Core..@? "DefaultValue")
      Core.<*> (x Core..@? "HighlightEnabled")

instance Core.Hashable TextArrayOptions

instance Core.NFData TextArrayOptions

instance Core.ToQuery TextArrayOptions where
  toQuery TextArrayOptions' {..} =
    Core.mconcat
      [ "AnalysisScheme" Core.=: analysisScheme,
        "SourceFields" Core.=: sourceFields,
        "ReturnEnabled" Core.=: returnEnabled,
        "DefaultValue" Core.=: defaultValue,
        "HighlightEnabled" Core.=: highlightEnabled
      ]
