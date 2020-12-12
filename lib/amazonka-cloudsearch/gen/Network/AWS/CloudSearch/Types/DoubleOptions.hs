{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DoubleOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DoubleOptions
  ( DoubleOptions (..),

    -- * Smart constructor
    mkDoubleOptions,

    -- * Lenses
    dSourceField,
    dReturnEnabled,
    dFacetEnabled,
    dSearchEnabled,
    dSortEnabled,
    dDefaultValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Options for a double-precision 64-bit floating point field. Present if @IndexFieldType@ specifies the field is of type @double@ . All options are enabled by default.
--
-- /See:/ 'mkDoubleOptions' smart constructor.
data DoubleOptions = DoubleOptions'
  { sourceField ::
      Lude.Maybe Lude.Text,
    returnEnabled :: Lude.Maybe Lude.Bool,
    facetEnabled :: Lude.Maybe Lude.Bool,
    searchEnabled :: Lude.Maybe Lude.Bool,
    sortEnabled :: Lude.Maybe Lude.Bool,
    defaultValue :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DoubleOptions' with the minimum fields required to make a request.
--
-- * 'defaultValue' - A value to use for the field if the field isn't specified for a document. This can be important if you are using the field in an expression and that field is not present in every document.
-- * 'facetEnabled' - Whether facet information can be returned for the field.
-- * 'returnEnabled' - Whether the contents of the field can be returned in the search results.
-- * 'searchEnabled' - Whether the contents of the field are searchable.
-- * 'sortEnabled' - Whether the field can be used to sort the search results.
-- * 'sourceField' - The name of the source field to map to the field.
mkDoubleOptions ::
  DoubleOptions
mkDoubleOptions =
  DoubleOptions'
    { sourceField = Lude.Nothing,
      returnEnabled = Lude.Nothing,
      facetEnabled = Lude.Nothing,
      searchEnabled = Lude.Nothing,
      sortEnabled = Lude.Nothing,
      defaultValue = Lude.Nothing
    }

-- | The name of the source field to map to the field.
--
-- /Note:/ Consider using 'sourceField' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSourceField :: Lens.Lens' DoubleOptions (Lude.Maybe Lude.Text)
dSourceField = Lens.lens (sourceField :: DoubleOptions -> Lude.Maybe Lude.Text) (\s a -> s {sourceField = a} :: DoubleOptions)
{-# DEPRECATED dSourceField "Use generic-lens or generic-optics with 'sourceField' instead." #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dReturnEnabled :: Lens.Lens' DoubleOptions (Lude.Maybe Lude.Bool)
dReturnEnabled = Lens.lens (returnEnabled :: DoubleOptions -> Lude.Maybe Lude.Bool) (\s a -> s {returnEnabled = a} :: DoubleOptions)
{-# DEPRECATED dReturnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead." #-}

-- | Whether facet information can be returned for the field.
--
-- /Note:/ Consider using 'facetEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dFacetEnabled :: Lens.Lens' DoubleOptions (Lude.Maybe Lude.Bool)
dFacetEnabled = Lens.lens (facetEnabled :: DoubleOptions -> Lude.Maybe Lude.Bool) (\s a -> s {facetEnabled = a} :: DoubleOptions)
{-# DEPRECATED dFacetEnabled "Use generic-lens or generic-optics with 'facetEnabled' instead." #-}

-- | Whether the contents of the field are searchable.
--
-- /Note:/ Consider using 'searchEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSearchEnabled :: Lens.Lens' DoubleOptions (Lude.Maybe Lude.Bool)
dSearchEnabled = Lens.lens (searchEnabled :: DoubleOptions -> Lude.Maybe Lude.Bool) (\s a -> s {searchEnabled = a} :: DoubleOptions)
{-# DEPRECATED dSearchEnabled "Use generic-lens or generic-optics with 'searchEnabled' instead." #-}

-- | Whether the field can be used to sort the search results.
--
-- /Note:/ Consider using 'sortEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dSortEnabled :: Lens.Lens' DoubleOptions (Lude.Maybe Lude.Bool)
dSortEnabled = Lens.lens (sortEnabled :: DoubleOptions -> Lude.Maybe Lude.Bool) (\s a -> s {sortEnabled = a} :: DoubleOptions)
{-# DEPRECATED dSortEnabled "Use generic-lens or generic-optics with 'sortEnabled' instead." #-}

-- | A value to use for the field if the field isn't specified for a document. This can be important if you are using the field in an expression and that field is not present in every document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDefaultValue :: Lens.Lens' DoubleOptions (Lude.Maybe Lude.Double)
dDefaultValue = Lens.lens (defaultValue :: DoubleOptions -> Lude.Maybe Lude.Double) (\s a -> s {defaultValue = a} :: DoubleOptions)
{-# DEPRECATED dDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

instance Lude.FromXML DoubleOptions where
  parseXML x =
    DoubleOptions'
      Lude.<$> (x Lude..@? "SourceField")
      Lude.<*> (x Lude..@? "ReturnEnabled")
      Lude.<*> (x Lude..@? "FacetEnabled")
      Lude.<*> (x Lude..@? "SearchEnabled")
      Lude.<*> (x Lude..@? "SortEnabled")
      Lude.<*> (x Lude..@? "DefaultValue")

instance Lude.ToQuery DoubleOptions where
  toQuery DoubleOptions' {..} =
    Lude.mconcat
      [ "SourceField" Lude.=: sourceField,
        "ReturnEnabled" Lude.=: returnEnabled,
        "FacetEnabled" Lude.=: facetEnabled,
        "SearchEnabled" Lude.=: searchEnabled,
        "SortEnabled" Lude.=: sortEnabled,
        "DefaultValue" Lude.=: defaultValue
      ]
