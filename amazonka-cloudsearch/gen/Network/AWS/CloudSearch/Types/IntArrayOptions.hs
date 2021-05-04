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
-- Module      : Network.AWS.CloudSearch.Types.IntArrayOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.IntArrayOptions where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Options for a field that contains an array of 64-bit signed integers.
-- Present if @IndexFieldType@ specifies the field is of type @int-array@.
-- All options are enabled by default.
--
-- /See:/ 'newIntArrayOptions' smart constructor.
data IntArrayOptions = IntArrayOptions'
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
    defaultValue :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'IntArrayOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceFields', 'intArrayOptions_sourceFields' - A list of source fields to map to the field.
--
-- 'facetEnabled', 'intArrayOptions_facetEnabled' - Whether facet information can be returned for the field.
--
-- 'returnEnabled', 'intArrayOptions_returnEnabled' - Whether the contents of the field can be returned in the search results.
--
-- 'searchEnabled', 'intArrayOptions_searchEnabled' - Whether the contents of the field are searchable.
--
-- 'defaultValue', 'intArrayOptions_defaultValue' - A value to use for the field if the field isn\'t specified for a
-- document.
newIntArrayOptions ::
  IntArrayOptions
newIntArrayOptions =
  IntArrayOptions'
    { sourceFields = Prelude.Nothing,
      facetEnabled = Prelude.Nothing,
      returnEnabled = Prelude.Nothing,
      searchEnabled = Prelude.Nothing,
      defaultValue = Prelude.Nothing
    }

-- | A list of source fields to map to the field.
intArrayOptions_sourceFields :: Lens.Lens' IntArrayOptions (Prelude.Maybe Prelude.Text)
intArrayOptions_sourceFields = Lens.lens (\IntArrayOptions' {sourceFields} -> sourceFields) (\s@IntArrayOptions' {} a -> s {sourceFields = a} :: IntArrayOptions)

-- | Whether facet information can be returned for the field.
intArrayOptions_facetEnabled :: Lens.Lens' IntArrayOptions (Prelude.Maybe Prelude.Bool)
intArrayOptions_facetEnabled = Lens.lens (\IntArrayOptions' {facetEnabled} -> facetEnabled) (\s@IntArrayOptions' {} a -> s {facetEnabled = a} :: IntArrayOptions)

-- | Whether the contents of the field can be returned in the search results.
intArrayOptions_returnEnabled :: Lens.Lens' IntArrayOptions (Prelude.Maybe Prelude.Bool)
intArrayOptions_returnEnabled = Lens.lens (\IntArrayOptions' {returnEnabled} -> returnEnabled) (\s@IntArrayOptions' {} a -> s {returnEnabled = a} :: IntArrayOptions)

-- | Whether the contents of the field are searchable.
intArrayOptions_searchEnabled :: Lens.Lens' IntArrayOptions (Prelude.Maybe Prelude.Bool)
intArrayOptions_searchEnabled = Lens.lens (\IntArrayOptions' {searchEnabled} -> searchEnabled) (\s@IntArrayOptions' {} a -> s {searchEnabled = a} :: IntArrayOptions)

-- | A value to use for the field if the field isn\'t specified for a
-- document.
intArrayOptions_defaultValue :: Lens.Lens' IntArrayOptions (Prelude.Maybe Prelude.Integer)
intArrayOptions_defaultValue = Lens.lens (\IntArrayOptions' {defaultValue} -> defaultValue) (\s@IntArrayOptions' {} a -> s {defaultValue = a} :: IntArrayOptions)

instance Prelude.FromXML IntArrayOptions where
  parseXML x =
    IntArrayOptions'
      Prelude.<$> (x Prelude..@? "SourceFields")
      Prelude.<*> (x Prelude..@? "FacetEnabled")
      Prelude.<*> (x Prelude..@? "ReturnEnabled")
      Prelude.<*> (x Prelude..@? "SearchEnabled")
      Prelude.<*> (x Prelude..@? "DefaultValue")

instance Prelude.Hashable IntArrayOptions

instance Prelude.NFData IntArrayOptions

instance Prelude.ToQuery IntArrayOptions where
  toQuery IntArrayOptions' {..} =
    Prelude.mconcat
      [ "SourceFields" Prelude.=: sourceFields,
        "FacetEnabled" Prelude.=: facetEnabled,
        "ReturnEnabled" Prelude.=: returnEnabled,
        "SearchEnabled" Prelude.=: searchEnabled,
        "DefaultValue" Prelude.=: defaultValue
      ]
