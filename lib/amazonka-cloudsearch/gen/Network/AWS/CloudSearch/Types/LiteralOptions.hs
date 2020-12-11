-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.LiteralOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.LiteralOptions
  ( LiteralOptions (..),

    -- * Smart constructor
    mkLiteralOptions,

    -- * Lenses
    loSourceField,
    loReturnEnabled,
    loFacetEnabled,
    loSearchEnabled,
    loSortEnabled,
    loDefaultValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Options for literal field. Present if @IndexFieldType@ specifies the field is of type @literal@ . All options are enabled by default.
--
-- /See:/ 'mkLiteralOptions' smart constructor.
data LiteralOptions = LiteralOptions'
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

-- | Creates a value of 'LiteralOptions' with the minimum fields required to make a request.
--
-- * 'defaultValue' - A value to use for the field if the field isn't specified for a document.
-- * 'facetEnabled' - Whether facet information can be returned for the field.
-- * 'returnEnabled' - Whether the contents of the field can be returned in the search results.
-- * 'searchEnabled' - Whether the contents of the field are searchable.
-- * 'sortEnabled' - Whether the field can be used to sort the search results.
-- * 'sourceField' - Undocumented field.
mkLiteralOptions ::
  LiteralOptions
mkLiteralOptions =
  LiteralOptions'
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
loSourceField :: Lens.Lens' LiteralOptions (Lude.Maybe Lude.Text)
loSourceField = Lens.lens (sourceField :: LiteralOptions -> Lude.Maybe Lude.Text) (\s a -> s {sourceField = a} :: LiteralOptions)
{-# DEPRECATED loSourceField "Use generic-lens or generic-optics with 'sourceField' instead." #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loReturnEnabled :: Lens.Lens' LiteralOptions (Lude.Maybe Lude.Bool)
loReturnEnabled = Lens.lens (returnEnabled :: LiteralOptions -> Lude.Maybe Lude.Bool) (\s a -> s {returnEnabled = a} :: LiteralOptions)
{-# DEPRECATED loReturnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead." #-}

-- | Whether facet information can be returned for the field.
--
-- /Note:/ Consider using 'facetEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loFacetEnabled :: Lens.Lens' LiteralOptions (Lude.Maybe Lude.Bool)
loFacetEnabled = Lens.lens (facetEnabled :: LiteralOptions -> Lude.Maybe Lude.Bool) (\s a -> s {facetEnabled = a} :: LiteralOptions)
{-# DEPRECATED loFacetEnabled "Use generic-lens or generic-optics with 'facetEnabled' instead." #-}

-- | Whether the contents of the field are searchable.
--
-- /Note:/ Consider using 'searchEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loSearchEnabled :: Lens.Lens' LiteralOptions (Lude.Maybe Lude.Bool)
loSearchEnabled = Lens.lens (searchEnabled :: LiteralOptions -> Lude.Maybe Lude.Bool) (\s a -> s {searchEnabled = a} :: LiteralOptions)
{-# DEPRECATED loSearchEnabled "Use generic-lens or generic-optics with 'searchEnabled' instead." #-}

-- | Whether the field can be used to sort the search results.
--
-- /Note:/ Consider using 'sortEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loSortEnabled :: Lens.Lens' LiteralOptions (Lude.Maybe Lude.Bool)
loSortEnabled = Lens.lens (sortEnabled :: LiteralOptions -> Lude.Maybe Lude.Bool) (\s a -> s {sortEnabled = a} :: LiteralOptions)
{-# DEPRECATED loSortEnabled "Use generic-lens or generic-optics with 'sortEnabled' instead." #-}

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loDefaultValue :: Lens.Lens' LiteralOptions (Lude.Maybe Lude.Text)
loDefaultValue = Lens.lens (defaultValue :: LiteralOptions -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: LiteralOptions)
{-# DEPRECATED loDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

instance Lude.FromXML LiteralOptions where
  parseXML x =
    LiteralOptions'
      Lude.<$> (x Lude..@? "SourceField")
      Lude.<*> (x Lude..@? "ReturnEnabled")
      Lude.<*> (x Lude..@? "FacetEnabled")
      Lude.<*> (x Lude..@? "SearchEnabled")
      Lude.<*> (x Lude..@? "SortEnabled")
      Lude.<*> (x Lude..@? "DefaultValue")

instance Lude.ToQuery LiteralOptions where
  toQuery LiteralOptions' {..} =
    Lude.mconcat
      [ "SourceField" Lude.=: sourceField,
        "ReturnEnabled" Lude.=: returnEnabled,
        "FacetEnabled" Lude.=: facetEnabled,
        "SearchEnabled" Lude.=: searchEnabled,
        "SortEnabled" Lude.=: sortEnabled,
        "DefaultValue" Lude.=: defaultValue
      ]
