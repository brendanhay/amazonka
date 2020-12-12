{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.IntOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.IntOptions
  ( IntOptions (..),

    -- * Smart constructor
    mkIntOptions,

    -- * Lenses
    ioSourceField,
    ioReturnEnabled,
    ioFacetEnabled,
    ioSearchEnabled,
    ioSortEnabled,
    ioDefaultValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Options for a 64-bit signed integer field. Present if @IndexFieldType@ specifies the field is of type @int@ . All options are enabled by default.
--
-- /See:/ 'mkIntOptions' smart constructor.
data IntOptions = IntOptions'
  { sourceField :: Lude.Maybe Lude.Text,
    returnEnabled :: Lude.Maybe Lude.Bool,
    facetEnabled :: Lude.Maybe Lude.Bool,
    searchEnabled :: Lude.Maybe Lude.Bool,
    sortEnabled :: Lude.Maybe Lude.Bool,
    defaultValue :: Lude.Maybe Lude.Integer
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IntOptions' with the minimum fields required to make a request.
--
-- * 'defaultValue' - A value to use for the field if the field isn't specified for a document. This can be important if you are using the field in an expression and that field is not present in every document.
-- * 'facetEnabled' - Whether facet information can be returned for the field.
-- * 'returnEnabled' - Whether the contents of the field can be returned in the search results.
-- * 'searchEnabled' - Whether the contents of the field are searchable.
-- * 'sortEnabled' - Whether the field can be used to sort the search results.
-- * 'sourceField' - The name of the source field to map to the field.
mkIntOptions ::
  IntOptions
mkIntOptions =
  IntOptions'
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
ioSourceField :: Lens.Lens' IntOptions (Lude.Maybe Lude.Text)
ioSourceField = Lens.lens (sourceField :: IntOptions -> Lude.Maybe Lude.Text) (\s a -> s {sourceField = a} :: IntOptions)
{-# DEPRECATED ioSourceField "Use generic-lens or generic-optics with 'sourceField' instead." #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ioReturnEnabled :: Lens.Lens' IntOptions (Lude.Maybe Lude.Bool)
ioReturnEnabled = Lens.lens (returnEnabled :: IntOptions -> Lude.Maybe Lude.Bool) (\s a -> s {returnEnabled = a} :: IntOptions)
{-# DEPRECATED ioReturnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead." #-}

-- | Whether facet information can be returned for the field.
--
-- /Note:/ Consider using 'facetEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ioFacetEnabled :: Lens.Lens' IntOptions (Lude.Maybe Lude.Bool)
ioFacetEnabled = Lens.lens (facetEnabled :: IntOptions -> Lude.Maybe Lude.Bool) (\s a -> s {facetEnabled = a} :: IntOptions)
{-# DEPRECATED ioFacetEnabled "Use generic-lens or generic-optics with 'facetEnabled' instead." #-}

-- | Whether the contents of the field are searchable.
--
-- /Note:/ Consider using 'searchEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ioSearchEnabled :: Lens.Lens' IntOptions (Lude.Maybe Lude.Bool)
ioSearchEnabled = Lens.lens (searchEnabled :: IntOptions -> Lude.Maybe Lude.Bool) (\s a -> s {searchEnabled = a} :: IntOptions)
{-# DEPRECATED ioSearchEnabled "Use generic-lens or generic-optics with 'searchEnabled' instead." #-}

-- | Whether the field can be used to sort the search results.
--
-- /Note:/ Consider using 'sortEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ioSortEnabled :: Lens.Lens' IntOptions (Lude.Maybe Lude.Bool)
ioSortEnabled = Lens.lens (sortEnabled :: IntOptions -> Lude.Maybe Lude.Bool) (\s a -> s {sortEnabled = a} :: IntOptions)
{-# DEPRECATED ioSortEnabled "Use generic-lens or generic-optics with 'sortEnabled' instead." #-}

-- | A value to use for the field if the field isn't specified for a document. This can be important if you are using the field in an expression and that field is not present in every document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ioDefaultValue :: Lens.Lens' IntOptions (Lude.Maybe Lude.Integer)
ioDefaultValue = Lens.lens (defaultValue :: IntOptions -> Lude.Maybe Lude.Integer) (\s a -> s {defaultValue = a} :: IntOptions)
{-# DEPRECATED ioDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

instance Lude.FromXML IntOptions where
  parseXML x =
    IntOptions'
      Lude.<$> (x Lude..@? "SourceField")
      Lude.<*> (x Lude..@? "ReturnEnabled")
      Lude.<*> (x Lude..@? "FacetEnabled")
      Lude.<*> (x Lude..@? "SearchEnabled")
      Lude.<*> (x Lude..@? "SortEnabled")
      Lude.<*> (x Lude..@? "DefaultValue")

instance Lude.ToQuery IntOptions where
  toQuery IntOptions' {..} =
    Lude.mconcat
      [ "SourceField" Lude.=: sourceField,
        "ReturnEnabled" Lude.=: returnEnabled,
        "FacetEnabled" Lude.=: facetEnabled,
        "SearchEnabled" Lude.=: searchEnabled,
        "SortEnabled" Lude.=: sortEnabled,
        "DefaultValue" Lude.=: defaultValue
      ]
