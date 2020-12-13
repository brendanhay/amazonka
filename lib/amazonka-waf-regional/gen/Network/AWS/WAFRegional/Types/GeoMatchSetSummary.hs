{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.GeoMatchSetSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.GeoMatchSetSummary
  ( GeoMatchSetSummary (..),

    -- * Smart constructor
    mkGeoMatchSetSummary,

    -- * Lenses
    gmssGeoMatchSetId,
    gmssName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the identifier and the name of the @GeoMatchSet@ .
--
-- /See:/ 'mkGeoMatchSetSummary' smart constructor.
data GeoMatchSetSummary = GeoMatchSetSummary'
  { -- | The @GeoMatchSetId@ for an 'GeoMatchSet' . You can use @GeoMatchSetId@ in a 'GetGeoMatchSet' request to get detailed information about an 'GeoMatchSet' .
    geoMatchSetId :: Lude.Text,
    -- | A friendly name or description of the 'GeoMatchSet' . You can't change the name of an @GeoMatchSet@ after you create it.
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GeoMatchSetSummary' with the minimum fields required to make a request.
--
-- * 'geoMatchSetId' - The @GeoMatchSetId@ for an 'GeoMatchSet' . You can use @GeoMatchSetId@ in a 'GetGeoMatchSet' request to get detailed information about an 'GeoMatchSet' .
-- * 'name' - A friendly name or description of the 'GeoMatchSet' . You can't change the name of an @GeoMatchSet@ after you create it.
mkGeoMatchSetSummary ::
  -- | 'geoMatchSetId'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  GeoMatchSetSummary
mkGeoMatchSetSummary pGeoMatchSetId_ pName_ =
  GeoMatchSetSummary'
    { geoMatchSetId = pGeoMatchSetId_,
      name = pName_
    }

-- | The @GeoMatchSetId@ for an 'GeoMatchSet' . You can use @GeoMatchSetId@ in a 'GetGeoMatchSet' request to get detailed information about an 'GeoMatchSet' .
--
-- /Note:/ Consider using 'geoMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmssGeoMatchSetId :: Lens.Lens' GeoMatchSetSummary Lude.Text
gmssGeoMatchSetId = Lens.lens (geoMatchSetId :: GeoMatchSetSummary -> Lude.Text) (\s a -> s {geoMatchSetId = a} :: GeoMatchSetSummary)
{-# DEPRECATED gmssGeoMatchSetId "Use generic-lens or generic-optics with 'geoMatchSetId' instead." #-}

-- | A friendly name or description of the 'GeoMatchSet' . You can't change the name of an @GeoMatchSet@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmssName :: Lens.Lens' GeoMatchSetSummary Lude.Text
gmssName = Lens.lens (name :: GeoMatchSetSummary -> Lude.Text) (\s a -> s {name = a} :: GeoMatchSetSummary)
{-# DEPRECATED gmssName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON GeoMatchSetSummary where
  parseJSON =
    Lude.withObject
      "GeoMatchSetSummary"
      ( \x ->
          GeoMatchSetSummary'
            Lude.<$> (x Lude..: "GeoMatchSetId") Lude.<*> (x Lude..: "Name")
      )
