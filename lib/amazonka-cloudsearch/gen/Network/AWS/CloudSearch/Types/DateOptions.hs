{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DateOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DateOptions
  ( DateOptions (..),

    -- * Smart constructor
    mkDateOptions,

    -- * Lenses
    doSourceField,
    doReturnEnabled,
    doFacetEnabled,
    doSearchEnabled,
    doSortEnabled,
    doDefaultValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Options for a date field. Dates and times are specified in UTC (Coordinated Universal Time) according to IETF RFC3339: yyyy-mm-ddT00:00:00Z. Present if @IndexFieldType@ specifies the field is of type @date@ . All options are enabled by default.
--
-- /See:/ 'mkDateOptions' smart constructor.
data DateOptions = DateOptions'
  { sourceField :: Lude.Maybe Lude.Text,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Lude.Maybe Lude.Bool,
    -- | Whether facet information can be returned for the field.
    facetEnabled :: Lude.Maybe Lude.Bool,
    -- | Whether the contents of the field are searchable.
    searchEnabled :: Lude.Maybe Lude.Bool,
    -- | Whether the field can be used to sort the search results.
    sortEnabled :: Lude.Maybe Lude.Bool,
    -- | A value to use for the field if the field isn't specified for a document.
    defaultValue :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DateOptions' with the minimum fields required to make a request.
--
-- * 'sourceField' -
-- * 'returnEnabled' - Whether the contents of the field can be returned in the search results.
-- * 'facetEnabled' - Whether facet information can be returned for the field.
-- * 'searchEnabled' - Whether the contents of the field are searchable.
-- * 'sortEnabled' - Whether the field can be used to sort the search results.
-- * 'defaultValue' - A value to use for the field if the field isn't specified for a document.
mkDateOptions ::
  DateOptions
mkDateOptions =
  DateOptions'
    { sourceField = Lude.Nothing,
      returnEnabled = Lude.Nothing,
      facetEnabled = Lude.Nothing,
      searchEnabled = Lude.Nothing,
      sortEnabled = Lude.Nothing,
      defaultValue = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'sourceField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doSourceField :: Lens.Lens' DateOptions (Lude.Maybe Lude.Text)
doSourceField = Lens.lens (sourceField :: DateOptions -> Lude.Maybe Lude.Text) (\s a -> s {sourceField = a} :: DateOptions)
{-# DEPRECATED doSourceField "Use generic-lens or generic-optics with 'sourceField' instead." #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doReturnEnabled :: Lens.Lens' DateOptions (Lude.Maybe Lude.Bool)
doReturnEnabled = Lens.lens (returnEnabled :: DateOptions -> Lude.Maybe Lude.Bool) (\s a -> s {returnEnabled = a} :: DateOptions)
{-# DEPRECATED doReturnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead." #-}

-- | Whether facet information can be returned for the field.
--
-- /Note:/ Consider using 'facetEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doFacetEnabled :: Lens.Lens' DateOptions (Lude.Maybe Lude.Bool)
doFacetEnabled = Lens.lens (facetEnabled :: DateOptions -> Lude.Maybe Lude.Bool) (\s a -> s {facetEnabled = a} :: DateOptions)
{-# DEPRECATED doFacetEnabled "Use generic-lens or generic-optics with 'facetEnabled' instead." #-}

-- | Whether the contents of the field are searchable.
--
-- /Note:/ Consider using 'searchEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doSearchEnabled :: Lens.Lens' DateOptions (Lude.Maybe Lude.Bool)
doSearchEnabled = Lens.lens (searchEnabled :: DateOptions -> Lude.Maybe Lude.Bool) (\s a -> s {searchEnabled = a} :: DateOptions)
{-# DEPRECATED doSearchEnabled "Use generic-lens or generic-optics with 'searchEnabled' instead." #-}

-- | Whether the field can be used to sort the search results.
--
-- /Note:/ Consider using 'sortEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doSortEnabled :: Lens.Lens' DateOptions (Lude.Maybe Lude.Bool)
doSortEnabled = Lens.lens (sortEnabled :: DateOptions -> Lude.Maybe Lude.Bool) (\s a -> s {sortEnabled = a} :: DateOptions)
{-# DEPRECATED doSortEnabled "Use generic-lens or generic-optics with 'sortEnabled' instead." #-}

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doDefaultValue :: Lens.Lens' DateOptions (Lude.Maybe Lude.Text)
doDefaultValue = Lens.lens (defaultValue :: DateOptions -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: DateOptions)
{-# DEPRECATED doDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

instance Lude.FromXML DateOptions where
  parseXML x =
    DateOptions'
      Lude.<$> (x Lude..@? "SourceField")
      Lude.<*> (x Lude..@? "ReturnEnabled")
      Lude.<*> (x Lude..@? "FacetEnabled")
      Lude.<*> (x Lude..@? "SearchEnabled")
      Lude.<*> (x Lude..@? "SortEnabled")
      Lude.<*> (x Lude..@? "DefaultValue")

instance Lude.ToQuery DateOptions where
  toQuery DateOptions' {..} =
    Lude.mconcat
      [ "SourceField" Lude.=: sourceField,
        "ReturnEnabled" Lude.=: returnEnabled,
        "FacetEnabled" Lude.=: facetEnabled,
        "SearchEnabled" Lude.=: searchEnabled,
        "SortEnabled" Lude.=: sortEnabled,
        "DefaultValue" Lude.=: defaultValue
      ]
