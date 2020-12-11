-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.LiteralArrayOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.LiteralArrayOptions
  ( LiteralArrayOptions (..),

    -- * Smart constructor
    mkLiteralArrayOptions,

    -- * Lenses
    laoSourceFields,
    laoReturnEnabled,
    laoFacetEnabled,
    laoSearchEnabled,
    laoDefaultValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Options for a field that contains an array of literal strings. Present if @IndexFieldType@ specifies the field is of type @literal-array@ . All options are enabled by default.
--
-- /See:/ 'mkLiteralArrayOptions' smart constructor.
data LiteralArrayOptions = LiteralArrayOptions'
  { sourceFields ::
      Lude.Maybe Lude.Text,
    returnEnabled :: Lude.Maybe Lude.Bool,
    facetEnabled :: Lude.Maybe Lude.Bool,
    searchEnabled :: Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'LiteralArrayOptions' with the minimum fields required to make a request.
--
-- * 'defaultValue' - A value to use for the field if the field isn't specified for a document.
-- * 'facetEnabled' - Whether facet information can be returned for the field.
-- * 'returnEnabled' - Whether the contents of the field can be returned in the search results.
-- * 'searchEnabled' - Whether the contents of the field are searchable.
-- * 'sourceFields' - A list of source fields to map to the field.
mkLiteralArrayOptions ::
  LiteralArrayOptions
mkLiteralArrayOptions =
  LiteralArrayOptions'
    { sourceFields = Lude.Nothing,
      returnEnabled = Lude.Nothing,
      facetEnabled = Lude.Nothing,
      searchEnabled = Lude.Nothing,
      defaultValue = Lude.Nothing
    }

-- | A list of source fields to map to the field.
--
-- /Note:/ Consider using 'sourceFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laoSourceFields :: Lens.Lens' LiteralArrayOptions (Lude.Maybe Lude.Text)
laoSourceFields = Lens.lens (sourceFields :: LiteralArrayOptions -> Lude.Maybe Lude.Text) (\s a -> s {sourceFields = a} :: LiteralArrayOptions)
{-# DEPRECATED laoSourceFields "Use generic-lens or generic-optics with 'sourceFields' instead." #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laoReturnEnabled :: Lens.Lens' LiteralArrayOptions (Lude.Maybe Lude.Bool)
laoReturnEnabled = Lens.lens (returnEnabled :: LiteralArrayOptions -> Lude.Maybe Lude.Bool) (\s a -> s {returnEnabled = a} :: LiteralArrayOptions)
{-# DEPRECATED laoReturnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead." #-}

-- | Whether facet information can be returned for the field.
--
-- /Note:/ Consider using 'facetEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laoFacetEnabled :: Lens.Lens' LiteralArrayOptions (Lude.Maybe Lude.Bool)
laoFacetEnabled = Lens.lens (facetEnabled :: LiteralArrayOptions -> Lude.Maybe Lude.Bool) (\s a -> s {facetEnabled = a} :: LiteralArrayOptions)
{-# DEPRECATED laoFacetEnabled "Use generic-lens or generic-optics with 'facetEnabled' instead." #-}

-- | Whether the contents of the field are searchable.
--
-- /Note:/ Consider using 'searchEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laoSearchEnabled :: Lens.Lens' LiteralArrayOptions (Lude.Maybe Lude.Bool)
laoSearchEnabled = Lens.lens (searchEnabled :: LiteralArrayOptions -> Lude.Maybe Lude.Bool) (\s a -> s {searchEnabled = a} :: LiteralArrayOptions)
{-# DEPRECATED laoSearchEnabled "Use generic-lens or generic-optics with 'searchEnabled' instead." #-}

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laoDefaultValue :: Lens.Lens' LiteralArrayOptions (Lude.Maybe Lude.Text)
laoDefaultValue = Lens.lens (defaultValue :: LiteralArrayOptions -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: LiteralArrayOptions)
{-# DEPRECATED laoDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

instance Lude.FromXML LiteralArrayOptions where
  parseXML x =
    LiteralArrayOptions'
      Lude.<$> (x Lude..@? "SourceFields")
      Lude.<*> (x Lude..@? "ReturnEnabled")
      Lude.<*> (x Lude..@? "FacetEnabled")
      Lude.<*> (x Lude..@? "SearchEnabled")
      Lude.<*> (x Lude..@? "DefaultValue")

instance Lude.ToQuery LiteralArrayOptions where
  toQuery LiteralArrayOptions' {..} =
    Lude.mconcat
      [ "SourceFields" Lude.=: sourceFields,
        "ReturnEnabled" Lude.=: returnEnabled,
        "FacetEnabled" Lude.=: facetEnabled,
        "SearchEnabled" Lude.=: searchEnabled,
        "DefaultValue" Lude.=: defaultValue
      ]
