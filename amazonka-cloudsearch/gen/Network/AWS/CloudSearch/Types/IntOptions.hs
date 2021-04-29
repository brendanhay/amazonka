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
-- Module      : Network.AWS.CloudSearch.Types.IntOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.IntOptions where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Options for a 64-bit signed integer field. Present if @IndexFieldType@
-- specifies the field is of type @int@. All options are enabled by
-- default.
--
-- /See:/ 'newIntOptions' smart constructor.
data IntOptions = IntOptions'
  { -- | Whether the field can be used to sort the search results.
    sortEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether facet information can be returned for the field.
    facetEnabled :: Prelude.Maybe Prelude.Bool,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the source field to map to the field.
    sourceField :: Prelude.Maybe Prelude.Text,
    -- | Whether the contents of the field are searchable.
    searchEnabled :: Prelude.Maybe Prelude.Bool,
    -- | A value to use for the field if the field isn\'t specified for a
    -- document. This can be important if you are using the field in an
    -- expression and that field is not present in every document.
    defaultValue :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'IntOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortEnabled', 'intOptions_sortEnabled' - Whether the field can be used to sort the search results.
--
-- 'facetEnabled', 'intOptions_facetEnabled' - Whether facet information can be returned for the field.
--
-- 'returnEnabled', 'intOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'sourceField', 'intOptions_sourceField' - The name of the source field to map to the field.
--
-- 'searchEnabled', 'intOptions_searchEnabled' - Whether the contents of the field are searchable.
--
-- 'defaultValue', 'intOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document. This can be important if you are using the field in an
-- expression and that field is not present in every document.
newIntOptions ::
  IntOptions
newIntOptions =
  IntOptions'
    { sortEnabled = Prelude.Nothing,
      facetEnabled = Prelude.Nothing,
      returnEnabled = Prelude.Nothing,
      sourceField = Prelude.Nothing,
      searchEnabled = Prelude.Nothing,
      defaultValue = Prelude.Nothing
    }

-- | Whether the field can be used to sort the search results.
intOptions_sortEnabled :: Lens.Lens' IntOptions (Prelude.Maybe Prelude.Bool)
intOptions_sortEnabled = Lens.lens (\IntOptions' {sortEnabled} -> sortEnabled) (\s@IntOptions' {} a -> s {sortEnabled = a} :: IntOptions)

-- | Whether facet information can be returned for the field.
intOptions_facetEnabled :: Lens.Lens' IntOptions (Prelude.Maybe Prelude.Bool)
intOptions_facetEnabled = Lens.lens (\IntOptions' {facetEnabled} -> facetEnabled) (\s@IntOptions' {} a -> s {facetEnabled = a} :: IntOptions)

-- | Whether the contents of the field can be returned in the search results.
intOptions_returnEnabled :: Lens.Lens' IntOptions (Prelude.Maybe Prelude.Bool)
intOptions_returnEnabled = Lens.lens (\IntOptions' {returnEnabled} -> returnEnabled) (\s@IntOptions' {} a -> s {returnEnabled = a} :: IntOptions)

-- | The name of the source field to map to the field.
intOptions_sourceField :: Lens.Lens' IntOptions (Prelude.Maybe Prelude.Text)
intOptions_sourceField = Lens.lens (\IntOptions' {sourceField} -> sourceField) (\s@IntOptions' {} a -> s {sourceField = a} :: IntOptions)

-- | Whether the contents of the field are searchable.
intOptions_searchEnabled :: Lens.Lens' IntOptions (Prelude.Maybe Prelude.Bool)
intOptions_searchEnabled = Lens.lens (\IntOptions' {searchEnabled} -> searchEnabled) (\s@IntOptions' {} a -> s {searchEnabled = a} :: IntOptions)

-- | A value to use for the field if the field isn\'t specified for a
-- document. This can be important if you are using the field in an
-- expression and that field is not present in every document.
intOptions_defaultValue :: Lens.Lens' IntOptions (Prelude.Maybe Prelude.Integer)
intOptions_defaultValue = Lens.lens (\IntOptions' {defaultValue} -> defaultValue) (\s@IntOptions' {} a -> s {defaultValue = a} :: IntOptions)

instance Prelude.FromXML IntOptions where
  parseXML x =
    IntOptions'
      Prelude.<$> (x Prelude..@? "SortEnabled")
      Prelude.<*> (x Prelude..@? "FacetEnabled")
      Prelude.<*> (x Prelude..@? "ReturnEnabled")
      Prelude.<*> (x Prelude..@? "SourceField")
      Prelude.<*> (x Prelude..@? "SearchEnabled")
      Prelude.<*> (x Prelude..@? "DefaultValue")

instance Prelude.Hashable IntOptions

instance Prelude.NFData IntOptions

instance Prelude.ToQuery IntOptions where
  toQuery IntOptions' {..} =
    Prelude.mconcat
      [ "SortEnabled" Prelude.=: sortEnabled,
        "FacetEnabled" Prelude.=: facetEnabled,
        "ReturnEnabled" Prelude.=: returnEnabled,
        "SourceField" Prelude.=: sourceField,
        "SearchEnabled" Prelude.=: searchEnabled,
        "DefaultValue" Prelude.=: defaultValue
      ]
