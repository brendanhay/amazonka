-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DateArrayOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DateArrayOptions
  ( DateArrayOptions (..),

    -- * Smart constructor
    mkDateArrayOptions,

    -- * Lenses
    daosSourceFields,
    daosReturnEnabled,
    daosFacetEnabled,
    daosSearchEnabled,
    daosDefaultValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Options for a field that contains an array of dates. Present if @IndexFieldType@ specifies the field is of type @date-array@ . All options are enabled by default.
--
-- /See:/ 'mkDateArrayOptions' smart constructor.
data DateArrayOptions = DateArrayOptions'
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

-- | Creates a value of 'DateArrayOptions' with the minimum fields required to make a request.
--
-- * 'defaultValue' - A value to use for the field if the field isn't specified for a document.
-- * 'facetEnabled' - Whether facet information can be returned for the field.
-- * 'returnEnabled' - Whether the contents of the field can be returned in the search results.
-- * 'searchEnabled' - Whether the contents of the field are searchable.
-- * 'sourceFields' - A list of source fields to map to the field.
mkDateArrayOptions ::
  DateArrayOptions
mkDateArrayOptions =
  DateArrayOptions'
    { sourceFields = Lude.Nothing,
      returnEnabled = Lude.Nothing,
      facetEnabled = Lude.Nothing,
      searchEnabled = Lude.Nothing,
      defaultValue = Lude.Nothing
    }

-- | A list of source fields to map to the field.
--
-- /Note:/ Consider using 'sourceFields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daosSourceFields :: Lens.Lens' DateArrayOptions (Lude.Maybe Lude.Text)
daosSourceFields = Lens.lens (sourceFields :: DateArrayOptions -> Lude.Maybe Lude.Text) (\s a -> s {sourceFields = a} :: DateArrayOptions)
{-# DEPRECATED daosSourceFields "Use generic-lens or generic-optics with 'sourceFields' instead." #-}

-- | Whether the contents of the field can be returned in the search results.
--
-- /Note:/ Consider using 'returnEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daosReturnEnabled :: Lens.Lens' DateArrayOptions (Lude.Maybe Lude.Bool)
daosReturnEnabled = Lens.lens (returnEnabled :: DateArrayOptions -> Lude.Maybe Lude.Bool) (\s a -> s {returnEnabled = a} :: DateArrayOptions)
{-# DEPRECATED daosReturnEnabled "Use generic-lens or generic-optics with 'returnEnabled' instead." #-}

-- | Whether facet information can be returned for the field.
--
-- /Note:/ Consider using 'facetEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daosFacetEnabled :: Lens.Lens' DateArrayOptions (Lude.Maybe Lude.Bool)
daosFacetEnabled = Lens.lens (facetEnabled :: DateArrayOptions -> Lude.Maybe Lude.Bool) (\s a -> s {facetEnabled = a} :: DateArrayOptions)
{-# DEPRECATED daosFacetEnabled "Use generic-lens or generic-optics with 'facetEnabled' instead." #-}

-- | Whether the contents of the field are searchable.
--
-- /Note:/ Consider using 'searchEnabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daosSearchEnabled :: Lens.Lens' DateArrayOptions (Lude.Maybe Lude.Bool)
daosSearchEnabled = Lens.lens (searchEnabled :: DateArrayOptions -> Lude.Maybe Lude.Bool) (\s a -> s {searchEnabled = a} :: DateArrayOptions)
{-# DEPRECATED daosSearchEnabled "Use generic-lens or generic-optics with 'searchEnabled' instead." #-}

-- | A value to use for the field if the field isn't specified for a document.
--
-- /Note:/ Consider using 'defaultValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
daosDefaultValue :: Lens.Lens' DateArrayOptions (Lude.Maybe Lude.Text)
daosDefaultValue = Lens.lens (defaultValue :: DateArrayOptions -> Lude.Maybe Lude.Text) (\s a -> s {defaultValue = a} :: DateArrayOptions)
{-# DEPRECATED daosDefaultValue "Use generic-lens or generic-optics with 'defaultValue' instead." #-}

instance Lude.FromXML DateArrayOptions where
  parseXML x =
    DateArrayOptions'
      Lude.<$> (x Lude..@? "SourceFields")
      Lude.<*> (x Lude..@? "ReturnEnabled")
      Lude.<*> (x Lude..@? "FacetEnabled")
      Lude.<*> (x Lude..@? "SearchEnabled")
      Lude.<*> (x Lude..@? "DefaultValue")

instance Lude.ToQuery DateArrayOptions where
  toQuery DateArrayOptions' {..} =
    Lude.mconcat
      [ "SourceFields" Lude.=: sourceFields,
        "ReturnEnabled" Lude.=: returnEnabled,
        "FacetEnabled" Lude.=: facetEnabled,
        "SearchEnabled" Lude.=: searchEnabled,
        "DefaultValue" Lude.=: defaultValue
      ]
