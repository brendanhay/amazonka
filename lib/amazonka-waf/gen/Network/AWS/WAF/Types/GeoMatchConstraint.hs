{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WAF.Types.GeoMatchConstraint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WAF.Types.GeoMatchConstraint
  ( GeoMatchConstraint (..),

    -- * Smart constructor
    mkGeoMatchConstraint,

    -- * Lenses
    gmcValue,
    gmcType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.WAF.Types.GeoMatchConstraintType
import Network.AWS.WAF.Types.GeoMatchConstraintValue

-- | The country from which web requests originate that you want AWS WAF to search for.
--
-- /See:/ 'mkGeoMatchConstraint' smart constructor.
data GeoMatchConstraint = GeoMatchConstraint'
  { -- | The country that you want AWS WAF to search for.
    value :: GeoMatchConstraintValue,
    -- | The type of geographical area you want AWS WAF to search for. Currently @Country@ is the only valid value.
    type' :: GeoMatchConstraintType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GeoMatchConstraint' with the minimum fields required to make a request.
--
-- * 'value' - The country that you want AWS WAF to search for.
-- * 'type'' - The type of geographical area you want AWS WAF to search for. Currently @Country@ is the only valid value.
mkGeoMatchConstraint ::
  -- | 'value'
  GeoMatchConstraintValue ->
  -- | 'type''
  GeoMatchConstraintType ->
  GeoMatchConstraint
mkGeoMatchConstraint pValue_ pType_ =
  GeoMatchConstraint' {value = pValue_, type' = pType_}

-- | The country that you want AWS WAF to search for.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcValue :: Lens.Lens' GeoMatchConstraint GeoMatchConstraintValue
gmcValue = Lens.lens (value :: GeoMatchConstraint -> GeoMatchConstraintValue) (\s a -> s {value = a} :: GeoMatchConstraint)
{-# DEPRECATED gmcValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The type of geographical area you want AWS WAF to search for. Currently @Country@ is the only valid value.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmcType :: Lens.Lens' GeoMatchConstraint GeoMatchConstraintType
gmcType = Lens.lens (type' :: GeoMatchConstraint -> GeoMatchConstraintType) (\s a -> s {type' = a} :: GeoMatchConstraint)
{-# DEPRECATED gmcType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromJSON GeoMatchConstraint where
  parseJSON =
    Lude.withObject
      "GeoMatchConstraint"
      ( \x ->
          GeoMatchConstraint'
            Lude.<$> (x Lude..: "Value") Lude.<*> (x Lude..: "Type")
      )

instance Lude.ToJSON GeoMatchConstraint where
  toJSON GeoMatchConstraint' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Value" Lude..= value),
            Lude.Just ("Type" Lude..= type')
          ]
      )
