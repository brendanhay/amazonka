{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.IntArrayOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.IntArrayOptions
  ( IntArrayOptions (..),

    -- * Smart constructor
    mkIntArrayOptions,

    -- * Lenses
    iaoSourceFields,
    iaoReturnEnabled,
    iaoFacetEnabled,
    iaoSearchEnabled,
    iaoDefaultValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Options for a field that contains an array of 64-bit signed integers. Present if @IndexFieldType@ specifies the field is of type @int-array@ . All options are enabled by default.
--
-- /See:/ 'mkIntArrayOptions' smart constructor.
data IntArrayOptions = IntArrayOptions'
  { -- | A list of source fields to map to the field.
    sourceFields :: Lude.Maybe Lude.Text,
    -- | Whether the contents of the field can be returned in the search results.
    returnEnabled :: Lude.Maybe Lude.Bool,
    -- | Whether facet information can be returned for the field.
    facetEnabled :: Lude.Maybe Lude.Bool,
    -- | Whether the contents of the field are searchable.
    searchEnabled :: Lude.Maybe Lude.Bool,
    -- | A value to use for the field if the field isn't specified for a document.
    defaultValue :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'IntArrayOptions' with the minimum fields required to make a request.
--
-- * 'sourceFields' - A list of source fields to map to the field.
-- * 'returnEnabled' - Whether the contents of the field can be returned in the search results.
-- * 'facetEnabled' - Whether facet information can be returned for the field.
-- * 'searchEnabled' - Whether the contents of the field are searchable.
-- * 'defaultValue' - A value to use for the field if the field isn't specified for a document.
mkIntArrayOptions ::
  IntArrayOptions
mkIntArrayOptions =
  IntArrayOptions'
    { sourceFields = Lude.Nothing,
      returnEnabled = Lude.Nothing,
      facetEnabled = Lude.Nothing,
      searchEnabled = Lude.Nothing,
      defaultValue = Lude.Nothing
    }

-- | A list of source fields to map to the field.
--
-- /Note:/ Consider using 'sourceFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaoSourceFields :: Lens.Lens' IntArrayOptions (Lude.Maybe Lude.Text)
iaoSourceFields = Lens.lens (sourceFields :: IntArrayOptions -> Lude.Maybe Lude.Text) (\s a -> s {sourceFields = a} :: IntArrayOptions)
{-# DEPRECATED iaoSourceFields "Use generic-lens or generic-optics with 'sourceFields' instead." #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaoReturnEnabled :: Lens.Lens' IntArrayOptions (Lude.Maybe Lude.Bool)
iaoReturnEnabled = Lens.lens (returnEnabled :: IntArrayOptions -> Lude.Maybe Lude.Bool) (\s a -> s {returnEnabled = a} :: IntArrayOptions)
{-# DEPRECATED iaoReturnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead." #-}

-- | Whether facet information can be returned for the field.
--
-- /Note:/ Consider using 'facetEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaoFacetEnabled :: Lens.Lens' IntArrayOptions (Lude.Maybe Lude.Bool)
iaoFacetEnabled = Lens.lens (facetEnabled :: IntArrayOptions -> Lude.Maybe Lude.Bool) (\s a -> s {facetEnabled = a} :: IntArrayOptions)
{-# DEPRECATED iaoFacetEnabled "Use generic-lens or generic-optics with 'facetEnabled' instead." #-}

-- | Whether the contents of the field are searchable.
--
-- /Note:/ Consider using 'searchEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaoSearchEnabled :: Lens.Lens' IntArrayOptions (Lude.Maybe Lude.Bool)
iaoSearchEnabled = Lens.lens (searchEnabled :: IntArrayOptions -> Lude.Maybe Lude.Bool) (\s a -> s {searchEnabled = a} :: IntArrayOptions)
{-# DEPRECATED iaoSearchEnabled "Use generic-lens or generic-optics with 'searchEnabled' instead." #-}

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iaoDefaultValue :: Lens.Lens' IntArrayOptions (Lude.Maybe Lude.Integer)
iaoDefaultValue = Lens.lens (defaultValue :: IntArrayOptions -> Lude.Maybe Lude.Integer) (\s a -> s {defaultValue = a} :: IntArrayOptions)
{-# DEPRECATED iaoDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

instance Lude.FromXML IntArrayOptions where
  parseXML x =
    IntArrayOptions'
      Lude.<$> (x Lude..@? "SourceFields")
      Lude.<*> (x Lude..@? "ReturnEnabled")
      Lude.<*> (x Lude..@? "FacetEnabled")
      Lude.<*> (x Lude..@? "SearchEnabled")
      Lude.<*> (x Lude..@? "DefaultValue")

instance Lude.ToQuery IntArrayOptions where
  toQuery IntArrayOptions' {..} =
    Lude.mconcat
      [ "SourceFields" Lude.=: sourceFields,
        "ReturnEnabled" Lude.=: returnEnabled,
        "FacetEnabled" Lude.=: facetEnabled,
        "SearchEnabled" Lude.=: searchEnabled,
        "DefaultValue" Lude.=: defaultValue
      ]
