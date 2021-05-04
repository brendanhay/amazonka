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
-- Module      : Network.AWS.CloudSearch.Types.DateArrayOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DateArrayOptions where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Options for a field that contains an array of dates. Present if
-- @IndexFieldType@ specifies the field is of type @date-array@. All
-- options are enabled by default.
--
-- /See:/ 'newDateArrayOptions' smart constructor.
data DateArrayOptions = DateArrayOptions'
  { -- | A list of source fields to map to the field.
    sourceFields :: Prelude.Maybe Prelude.Text,
    -- | Whether facet information can be returned for the field.
    facetEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether the contents of the field are searchable.
    searchEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A value to use for the field if the field isn\'t specified for a
    -- document.
    defaultValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DateArrayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceFields', 'dateArrayOptions_sourceFields' - A list of source fields to map to the field.
--
-- 'facetEnabled', 'dateArrayOptions_facetEnabled' - Whether facet information can be returned for the field.
--
-- 'returnEnabled', 'dateArrayOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'searchEnabled', 'dateArrayOptions_searchEnabled' - Whether the contents of the field are searchable.
--
-- 'defaultValue', 'dateArrayOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document.
newDateArrayOptions ::
  DateArrayOptions
newDateArrayOptions =
  DateArrayOptions'
    { sourceFields = Prelude.Nothing,
      facetEnabled = Prelude.Nothing,
      returnEnabled = Prelude.Nothing,
      searchEnabled = Prelude.Nothing,
      defaultValue = Prelude.Nothing
    }

-- | A list of source fields to map to the field.
dateArrayOptions_sourceFields :: Lens.Lens' DateArrayOptions (Prelude.Maybe Prelude.Text)
dateArrayOptions_sourceFields = Lens.lens (\DateArrayOptions' {sourceFields} -> sourceFields) (\s@DateArrayOptions' {} a -> s {sourceFields = a} :: DateArrayOptions)

-- | Whether facet information can be returned for the field.
dateArrayOptions_facetEnabled :: Lens.Lens' DateArrayOptions (Prelude.Maybe Prelude.Bool)
dateArrayOptions_facetEnabled = Lens.lens (\DateArrayOptions' {facetEnabled} -> facetEnabled) (\s@DateArrayOptions' {} a -> s {facetEnabled = a} :: DateArrayOptions)

-- | Whether the contents of the field can be returned in the search results.
dateArrayOptions_returnEnabled :: Lens.Lens' DateArrayOptions (Prelude.Maybe Prelude.Bool)
dateArrayOptions_returnEnabled = Lens.lens (\DateArrayOptions' {returnEnabled} -> returnEnabled) (\s@DateArrayOptions' {} a -> s {returnEnabled = a} :: DateArrayOptions)

-- | Whether the contents of the field are searchable.
dateArrayOptions_searchEnabled :: Lens.Lens' DateArrayOptions (Prelude.Maybe Prelude.Bool)
dateArrayOptions_searchEnabled = Lens.lens (\DateArrayOptions' {searchEnabled} -> searchEnabled) (\s@DateArrayOptions' {} a -> s {searchEnabled = a} :: DateArrayOptions)

-- | A value to use for the field if the field isn\'t specified for a
-- document.
dateArrayOptions_defaultValue :: Lens.Lens' DateArrayOptions (Prelude.Maybe Prelude.Text)
dateArrayOptions_defaultValue = Lens.lens (\DateArrayOptions' {defaultValue} -> defaultValue) (\s@DateArrayOptions' {} a -> s {defaultValue = a} :: DateArrayOptions)

instance Prelude.FromXML DateArrayOptions where
  parseXML x =
    DateArrayOptions'
      Prelude.<$> (x Prelude..@? "SourceFields")
      Prelude.<*> (x Prelude..@? "FacetEnabled")
      Prelude.<*> (x Prelude..@? "ReturnEnabled")
      Prelude.<*> (x Prelude..@? "SearchEnabled")
      Prelude.<*> (x Prelude..@? "DefaultValue")

instance Prelude.Hashable DateArrayOptions

instance Prelude.NFData DateArrayOptions

instance Prelude.ToQuery DateArrayOptions where
  toQuery DateArrayOptions' {..} =
    Prelude.mconcat
      [ "SourceFields" Prelude.=: sourceFields,
        "FacetEnabled" Prelude.=: facetEnabled,
        "ReturnEnabled" Prelude.=: returnEnabled,
        "SearchEnabled" Prelude.=: searchEnabled,
        "DefaultValue" Prelude.=: defaultValue
      ]
