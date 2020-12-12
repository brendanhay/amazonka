{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.GeoMatchSetUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.GeoMatchSetUpdate
  ( GeoMatchSetUpdate (..),

    -- * Smart constructor
    mkGeoMatchSetUpdate,

    -- * Lenses
    gmsuAction,
    gmsuGeoMatchConstraint,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAF.Types.ChangeAction
import Network.AWS.WAF.Types.GeoMatchConstraint

-- | Specifies the type of update to perform to an 'GeoMatchSet' with 'UpdateGeoMatchSet' .
--
-- /See:/ 'mkGeoMatchSetUpdate' smart constructor.
data GeoMatchSetUpdate = GeoMatchSetUpdate'
  { action :: ChangeAction,
    geoMatchConstraint :: GeoMatchConstraint
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GeoMatchSetUpdate' with the minimum fields required to make a request.
--
-- * 'action' - Specifies whether to insert or delete a country with 'UpdateGeoMatchSet' .
-- * 'geoMatchConstraint' - The country from which web requests originate that you want AWS WAF to search for.
mkGeoMatchSetUpdate ::
  -- | 'action'
  ChangeAction ->
  -- | 'geoMatchConstraint'
  GeoMatchConstraint ->
  GeoMatchSetUpdate
mkGeoMatchSetUpdate pAction_ pGeoMatchConstraint_ =
  GeoMatchSetUpdate'
    { action = pAction_,
      geoMatchConstraint = pGeoMatchConstraint_
    }

-- | Specifies whether to insert or delete a country with 'UpdateGeoMatchSet' .
--
-- /Note:/ Consider using 'action' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsuAction :: Lens.Lens' GeoMatchSetUpdate ChangeAction
gmsuAction = Lens.lens (action :: GeoMatchSetUpdate -> ChangeAction) (\s a -> s {action = a} :: GeoMatchSetUpdate)
{-# DEPRECATED gmsuAction "Use generic-lens or generic-optics with 'action' instead." #-}

-- | The country from which web requests originate that you want AWS WAF to search for.
--
-- /Note:/ Consider using 'geoMatchConstraint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmsuGeoMatchConstraint :: Lens.Lens' GeoMatchSetUpdate GeoMatchConstraint
gmsuGeoMatchConstraint = Lens.lens (geoMatchConstraint :: GeoMatchSetUpdate -> GeoMatchConstraint) (\s a -> s {geoMatchConstraint = a} :: GeoMatchSetUpdate)
{-# DEPRECATED gmsuGeoMatchConstraint "Use generic-lens or generic-optics with 'geoMatchConstraint' instead." #-}

instance Lude.ToJSON GeoMatchSetUpdate where
  toJSON GeoMatchSetUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Action" Lude..= action),
            Lude.Just ("GeoMatchConstraint" Lude..= geoMatchConstraint)
          ]
      )
