{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAFRegional.Types.GeoMatchSet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAFRegional.Types.GeoMatchSet
  ( GeoMatchSet (..),

    -- * Smart constructor
    mkGeoMatchSet,

    -- * Lenses
    gmsName,
    gmsGeoMatchSetId,
    gmsGeoMatchConstraints,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAFRegional.Types.GeoMatchConstraint

-- | Contains one or more countries that AWS WAF will search for.
--
-- /See:/ 'mkGeoMatchSet' smart constructor.
data GeoMatchSet = GeoMatchSet'
  { name :: Lude.Maybe Lude.Text,
    geoMatchSetId :: Lude.Text,
    geoMatchConstraints :: [GeoMatchConstraint]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GeoMatchSet' with the minimum fields required to make a request.
--
-- * 'geoMatchConstraints' - An array of 'GeoMatchConstraint' objects, which contain the country that you want AWS WAF to search for.
-- * 'geoMatchSetId' - The @GeoMatchSetId@ for an @GeoMatchSet@ . You use @GeoMatchSetId@ to get information about a @GeoMatchSet@ (see 'GeoMatchSet' ), update a @GeoMatchSet@ (see 'UpdateGeoMatchSet' ), insert a @GeoMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @GeoMatchSet@ from AWS WAF (see 'DeleteGeoMatchSet' ).
--
-- @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
-- * 'name' - A friendly name or description of the 'GeoMatchSet' . You can't change the name of an @GeoMatchSet@ after you create it.
mkGeoMatchSet ::
  -- | 'geoMatchSetId'
  Lude.Text ->
  GeoMatchSet
mkGeoMatchSet pGeoMatchSetId_ =
  GeoMatchSet'
    { name = Lude.Nothing,
      geoMatchSetId = pGeoMatchSetId_,
      geoMatchConstraints = Lude.mempty
    }

-- | A friendly name or description of the 'GeoMatchSet' . You can't change the name of an @GeoMatchSet@ after you create it.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsName :: Lens.Lens' GeoMatchSet (Lude.Maybe Lude.Text)
gmsName = Lens.lens (name :: GeoMatchSet -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GeoMatchSet)
{-# DEPRECATED gmsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The @GeoMatchSetId@ for an @GeoMatchSet@ . You use @GeoMatchSetId@ to get information about a @GeoMatchSet@ (see 'GeoMatchSet' ), update a @GeoMatchSet@ (see 'UpdateGeoMatchSet' ), insert a @GeoMatchSet@ into a @Rule@ or delete one from a @Rule@ (see 'UpdateRule' ), and delete a @GeoMatchSet@ from AWS WAF (see 'DeleteGeoMatchSet' ).
--
-- @GeoMatchSetId@ is returned by 'CreateGeoMatchSet' and by 'ListGeoMatchSets' .
--
-- /Note:/ Consider using 'geoMatchSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsGeoMatchSetId :: Lens.Lens' GeoMatchSet Lude.Text
gmsGeoMatchSetId = Lens.lens (geoMatchSetId :: GeoMatchSet -> Lude.Text) (\s a -> s {geoMatchSetId = a} :: GeoMatchSet)
{-# DEPRECATED gmsGeoMatchSetId "Use generic-lens or generic-optics with 'geoMatchSetId' instead." #-}

-- | An array of 'GeoMatchConstraint' objects, which contain the country that you want AWS WAF to search for.
--
-- /Note:/ Consider using 'geoMatchConstraints' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsGeoMatchConstraints :: Lens.Lens' GeoMatchSet [GeoMatchConstraint]
gmsGeoMatchConstraints = Lens.lens (geoMatchConstraints :: GeoMatchSet -> [GeoMatchConstraint]) (\s a -> s {geoMatchConstraints = a} :: GeoMatchSet)
{-# DEPRECATED gmsGeoMatchConstraints "Use generic-lens or generic-optics with 'geoMatchConstraints' instead." #-}

instance Lude.FromJSON GeoMatchSet where
  parseJSON =
    Lude.withObject
      "GeoMatchSet"
      ( \x ->
          GeoMatchSet'
            Lude.<$> (x Lude..:? "Name")
            Lude.<*> (x Lude..: "GeoMatchSetId")
            Lude.<*> (x Lude..:? "GeoMatchConstraints" Lude..!= Lude.mempty)
      )
