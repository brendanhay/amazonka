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
-- Module      : Network.AWS.CloudSearch.Types.LiteralArrayOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.LiteralArrayOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Options for a field that contains an array of literal strings. Present
-- if @IndexFieldType@ specifies the field is of type @literal-array@. All
-- options are enabled by default.
--
-- /See:/ 'newLiteralArrayOptions' smart constructor.
data LiteralArrayOptions = LiteralArrayOptions'
  { -- | A list of source fields to map to the field.
    sourceFields :: Core.Maybe Core.Text,
    -- | Whether facet information can be returned for the field.
    facetEnabled :: Core.Maybe Core.Bool,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Core.Maybe Core.Bool,
    -- | Whether the contents of the field are searchable.
    searchEnabled :: Core.Maybe Core.Bool,
    -- | A value to use for the field if the field isn\'t specified for a
    -- document.
    defaultValue :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LiteralArrayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceFields', 'literalArrayOptions_sourceFields' - A list of source fields to map to the field.
--
-- 'facetEnabled', 'literalArrayOptions_facetEnabled' - Whether facet information can be returned for the field.
--
-- 'returnEnabled', 'literalArrayOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'searchEnabled', 'literalArrayOptions_searchEnabled' - Whether the contents of the field are searchable.
--
-- 'defaultValue', 'literalArrayOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document.
newLiteralArrayOptions ::
  LiteralArrayOptions
newLiteralArrayOptions =
  LiteralArrayOptions'
    { sourceFields = Core.Nothing,
      facetEnabled = Core.Nothing,
      returnEnabled = Core.Nothing,
      searchEnabled = Core.Nothing,
      defaultValue = Core.Nothing
    }

-- | A list of source fields to map to the field.
literalArrayOptions_sourceFields :: Lens.Lens' LiteralArrayOptions (Core.Maybe Core.Text)
literalArrayOptions_sourceFields = Lens.lens (\LiteralArrayOptions' {sourceFields} -> sourceFields) (\s@LiteralArrayOptions' {} a -> s {sourceFields = a} :: LiteralArrayOptions)

-- | Whether facet information can be returned for the field.
literalArrayOptions_facetEnabled :: Lens.Lens' LiteralArrayOptions (Core.Maybe Core.Bool)
literalArrayOptions_facetEnabled = Lens.lens (\LiteralArrayOptions' {facetEnabled} -> facetEnabled) (\s@LiteralArrayOptions' {} a -> s {facetEnabled = a} :: LiteralArrayOptions)

-- | Whether the contents of the field can be returned in the search results.
literalArrayOptions_returnEnabled :: Lens.Lens' LiteralArrayOptions (Core.Maybe Core.Bool)
literalArrayOptions_returnEnabled = Lens.lens (\LiteralArrayOptions' {returnEnabled} -> returnEnabled) (\s@LiteralArrayOptions' {} a -> s {returnEnabled = a} :: LiteralArrayOptions)

-- | Whether the contents of the field are searchable.
literalArrayOptions_searchEnabled :: Lens.Lens' LiteralArrayOptions (Core.Maybe Core.Bool)
literalArrayOptions_searchEnabled = Lens.lens (\LiteralArrayOptions' {searchEnabled} -> searchEnabled) (\s@LiteralArrayOptions' {} a -> s {searchEnabled = a} :: LiteralArrayOptions)

-- | A value to use for the field if the field isn\'t specified for a
-- document.
literalArrayOptions_defaultValue :: Lens.Lens' LiteralArrayOptions (Core.Maybe Core.Text)
literalArrayOptions_defaultValue = Lens.lens (\LiteralArrayOptions' {defaultValue} -> defaultValue) (\s@LiteralArrayOptions' {} a -> s {defaultValue = a} :: LiteralArrayOptions)

instance Core.FromXML LiteralArrayOptions where
  parseXML x =
    LiteralArrayOptions'
      Core.<$> (x Core..@? "SourceFields")
      Core.<*> (x Core..@? "FacetEnabled")
      Core.<*> (x Core..@? "ReturnEnabled")
      Core.<*> (x Core..@? "SearchEnabled")
      Core.<*> (x Core..@? "DefaultValue")

instance Core.Hashable LiteralArrayOptions

instance Core.NFData LiteralArrayOptions

instance Core.ToQuery LiteralArrayOptions where
  toQuery LiteralArrayOptions' {..} =
    Core.mconcat
      [ "SourceFields" Core.=: sourceFields,
        "FacetEnabled" Core.=: facetEnabled,
        "ReturnEnabled" Core.=: returnEnabled,
        "SearchEnabled" Core.=: searchEnabled,
        "DefaultValue" Core.=: defaultValue
      ]
