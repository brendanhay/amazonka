{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.LatLonOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.LatLonOptions
  ( LatLonOptions (..),

    -- * Smart constructor
    mkLatLonOptions,

    -- * Lenses
    lloSourceField,
    lloReturnEnabled,
    lloFacetEnabled,
    lloSearchEnabled,
    lloSortEnabled,
    lloDefaultValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Options for a latlon field. A latlon field contains a location stored as a latitude and longitude value pair. Present if @IndexFieldType@ specifies the field is of type @latlon@ . All options are enabled by default.
--
-- /See:/ 'mkLatLonOptions' smart constructor.
data LatLonOptions = LatLonOptions'
  { sourceField ::
      Lude.Maybe Lude.Text,
    returnEnabled :: Lude.Maybe Lude.Bool,
    facetEnabled :: Lude.Maybe Lude.Bool,
    searchEnabled :: Lude.Maybe Lude.Bool,
    sortEnabled :: Lude.Maybe Lude.Bool,
    defaultValue :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LatLonOptions' with the minimum fields required to make a request.
--
-- * 'defaultValue' - A value to use for the field if the field isn't specified for a document.
-- * 'facetEnabled' - Whether facet information can be returned for the field.
-- * 'returnEnabled' - Whether the contents of the field can be returned in the search results.
-- * 'searchEnabled' - Whether the contents of the field are searchable.
-- * 'sortEnabled' - Whether the field can be used to sort the search results.
-- * 'sourceField' - Undocumented field.
mkLatLonOptions ::
  LatLonOptions
mkLatLonOptions =
  LatLonOptions'
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
lloSourceField :: Lens.Lens' LatLonOptions (Lude.Maybe Lude.Text)
lloSourceField = Lens.lens (sourceField :: LatLonOptions -> Lude.Maybe Lude.Text) (\s a -> s {sourceField = a} :: LatLonOptions)
{-# DEPRECATED lloSourceField "Use generic-lens or generic-optics with 'sourceField' instead." #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lloReturnEnabled :: Lens.Lens' LatLonOptions (Lude.Maybe Lude.Bool)
lloReturnEnabled = Lens.lens (returnEnabled :: LatLonOptions -> Lude.Maybe Lude.Bool) (\s a -> s {returnEnabled = a} :: LatLonOptions)
{-# DEPRECATED lloReturnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead." #-}

-- | Whether facet information can be returned for the field.
--
-- /Note:/ Consider using 'facetEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lloFacetEnabled :: Lens.Lens' LatLonOptions (Lude.Maybe Lude.Bool)
lloFacetEnabled = Lens.lens (facetEnabled :: LatLonOptions -> Lude.Maybe Lude.Bool) (\s a -> s {facetEnabled = a} :: LatLonOptions)
{-# DEPRECATED lloFacetEnabled "Use generic-lens or generic-optics with 'facetEnabled' instead." #-}

-- | Whether the contents of the field are searchable.
--
-- /Note:/ Consider using 'searchEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lloSearchEnabled :: Lens.Lens' LatLonOptions (Lude.Maybe Lude.Bool)
lloSearchEnabled = Lens.lens (searchEnabled :: LatLonOptions -> Lude.Maybe Lude.Bool) (\s a -> s {searchEnabled = a} :: LatLonOptions)
{-# DEPRECATED lloSearchEnabled "Use generic-lens or generic-optics with 'searchEnabled' instead." #-}

-- | Whether the field can be used to sort the search results.
--
-- /Note:/ Consider using 'sortEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lloSortEnabled :: Lens.Lens' LatLonOptions (Lude.Maybe Lude.Bool)
lloSortEnabled = Lens.lens (sortEnabled :: LatLonOptions -> Lude.Maybe Lude.Bool) (\s a -> s {sortEnabled = a} :: LatLonOptions)
{-# DEPRECATED lloSortEnabled "Use generic-lens or generic-optics with 'sortEnabled' instead." #-}

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lloDefaultValue :: Lens.Lens' LatLonOptions (Lude.Maybe Lude.Text)
lloDefaultValue = Lens.lens (defaultValue :: LatLonOptions -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: LatLonOptions)
{-# DEPRECATED lloDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

instance Lude.FromXML LatLonOptions where
  parseXML x =
    LatLonOptions'
      Lude.<$> (x Lude..@? "SourceField")
      Lude.<*> (x Lude..@? "ReturnEnabled")
      Lude.<*> (x Lude..@? "FacetEnabled")
      Lude.<*> (x Lude..@? "SearchEnabled")
      Lude.<*> (x Lude..@? "SortEnabled")
      Lude.<*> (x Lude..@? "DefaultValue")

instance Lude.ToQuery LatLonOptions where
  toQuery LatLonOptions' {..} =
    Lude.mconcat
      [ "SourceField" Lude.=: sourceField,
        "ReturnEnabled" Lude.=: returnEnabled,
        "FacetEnabled" Lude.=: facetEnabled,
        "SearchEnabled" Lude.=: searchEnabled,
        "SortEnabled" Lude.=: sortEnabled,
        "DefaultValue" Lude.=: defaultValue
      ]
