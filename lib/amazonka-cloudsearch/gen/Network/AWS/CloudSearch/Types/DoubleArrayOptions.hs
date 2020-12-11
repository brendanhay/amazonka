-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DoubleArrayOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DoubleArrayOptions
  ( DoubleArrayOptions (..),

    -- * Smart constructor
    mkDoubleArrayOptions,

    -- * Lenses
    daoSourceFields,
    daoReturnEnabled,
    daoFacetEnabled,
    daoSearchEnabled,
    daoDefaultValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Options for a field that contains an array of double-precision 64-bit floating point values. Present if @IndexFieldType@ specifies the field is of type @double-array@ . All options are enabled by default.
--
-- /See:/ 'mkDoubleArrayOptions' smart constructor.
data DoubleArrayOptions = DoubleArrayOptions'
  { sourceFields ::
      Lude.Maybe Lude.Text,
    returnEnabled :: Lude.Maybe Lude.Bool,
    facetEnabled :: Lude.Maybe Lude.Bool,
    searchEnabled :: Lude.Maybe Lude.Bool,
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

-- | Creates a value of 'DoubleArrayOptions' with the minimum fields required to make a request.
--
-- * 'defaultValue' - A value to use for the field if the field isn't specified for a document.
-- * 'facetEnabled' - Whether facet information can be returned for the field.
-- * 'returnEnabled' - Whether the contents of the field can be returned in the search results.
-- * 'searchEnabled' - Whether the contents of the field are searchable.
-- * 'sourceFields' - A list of source fields to map to the field.
mkDoubleArrayOptions ::
  DoubleArrayOptions
mkDoubleArrayOptions =
  DoubleArrayOptions'
    { sourceFields = Lude.Nothing,
      returnEnabled = Lude.Nothing,
      facetEnabled = Lude.Nothing,
      searchEnabled = Lude.Nothing,
      defaultValue = Lude.Nothing
    }

-- | A list of source fields to map to the field.
--
-- /Note:/ Consider using 'sourceFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoSourceFields :: Lens.Lens' DoubleArrayOptions (Lude.Maybe Lude.Text)
daoSourceFields = Lens.lens (sourceFields :: DoubleArrayOptions -> Lude.Maybe Lude.Text) (\s a -> s {sourceFields = a} :: DoubleArrayOptions)
{-# DEPRECATED daoSourceFields "Use generic-lens or generic-optics with 'sourceFields' instead." #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoReturnEnabled :: Lens.Lens' DoubleArrayOptions (Lude.Maybe Lude.Bool)
daoReturnEnabled = Lens.lens (returnEnabled :: DoubleArrayOptions -> Lude.Maybe Lude.Bool) (\s a -> s {returnEnabled = a} :: DoubleArrayOptions)
{-# DEPRECATED daoReturnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead." #-}

-- | Whether facet information can be returned for the field.
--
-- /Note:/ Consider using 'facetEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoFacetEnabled :: Lens.Lens' DoubleArrayOptions (Lude.Maybe Lude.Bool)
daoFacetEnabled = Lens.lens (facetEnabled :: DoubleArrayOptions -> Lude.Maybe Lude.Bool) (\s a -> s {facetEnabled = a} :: DoubleArrayOptions)
{-# DEPRECATED daoFacetEnabled "Use generic-lens or generic-optics with 'facetEnabled' instead." #-}

-- | Whether the contents of the field are searchable.
--
-- /Note:/ Consider using 'searchEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoSearchEnabled :: Lens.Lens' DoubleArrayOptions (Lude.Maybe Lude.Bool)
daoSearchEnabled = Lens.lens (searchEnabled :: DoubleArrayOptions -> Lude.Maybe Lude.Bool) (\s a -> s {searchEnabled = a} :: DoubleArrayOptions)
{-# DEPRECATED daoSearchEnabled "Use generic-lens or generic-optics with 'searchEnabled' instead." #-}

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daoDefaultValue :: Lens.Lens' DoubleArrayOptions (Lude.Maybe Lude.Double)
daoDefaultValue = Lens.lens (defaultValue :: DoubleArrayOptions -> Lude.Maybe Lude.Double) (\s a -> s {defaultValue = a} :: DoubleArrayOptions)
{-# DEPRECATED daoDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

instance Lude.FromXML DoubleArrayOptions where
  parseXML x =
    DoubleArrayOptions'
      Lude.<$> (x Lude..@? "SourceFields")
      Lude.<*> (x Lude..@? "ReturnEnabled")
      Lude.<*> (x Lude..@? "FacetEnabled")
      Lude.<*> (x Lude..@? "SearchEnabled")
      Lude.<*> (x Lude..@? "DefaultValue")

instance Lude.ToQuery DoubleArrayOptions where
  toQuery DoubleArrayOptions' {..} =
    Lude.mconcat
      [ "SourceFields" Lude.=: sourceFields,
        "ReturnEnabled" Lude.=: returnEnabled,
        "FacetEnabled" Lude.=: facetEnabled,
        "SearchEnabled" Lude.=: searchEnabled,
        "DefaultValue" Lude.=: defaultValue
      ]
