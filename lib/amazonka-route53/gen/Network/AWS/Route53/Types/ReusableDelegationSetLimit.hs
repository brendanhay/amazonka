{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.ReusableDelegationSetLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ReusableDelegationSetLimit
  ( ReusableDelegationSetLimit (..),

    -- * Smart constructor
    mkReusableDelegationSetLimit,

    -- * Lenses
    rdslValue,
    rdslType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.ReusableDelegationSetLimitType

-- | A complex type that contains the type of limit that you specified in the request and the current value for that limit.
--
-- /See:/ 'mkReusableDelegationSetLimit' smart constructor.
data ReusableDelegationSetLimit = ReusableDelegationSetLimit'
  { -- | The current value for the @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ limit.
    value :: Lude.Natural,
    -- | The limit that you requested: @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ , the maximum number of hosted zones that you can associate with the specified reusable delegation set.
    type' :: ReusableDelegationSetLimitType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReusableDelegationSetLimit' with the minimum fields required to make a request.
--
-- * 'value' - The current value for the @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ limit.
-- * 'type'' - The limit that you requested: @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ , the maximum number of hosted zones that you can associate with the specified reusable delegation set.
mkReusableDelegationSetLimit ::
  -- | 'value'
  Lude.Natural ->
  -- | 'type''
  ReusableDelegationSetLimitType ->
  ReusableDelegationSetLimit
mkReusableDelegationSetLimit pValue_ pType_ =
  ReusableDelegationSetLimit' {value = pValue_, type' = pType_}

-- | The current value for the @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ limit.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdslValue :: Lens.Lens' ReusableDelegationSetLimit Lude.Natural
rdslValue = Lens.lens (value :: ReusableDelegationSetLimit -> Lude.Natural) (\s a -> s {value = a} :: ReusableDelegationSetLimit)
{-# DEPRECATED rdslValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The limit that you requested: @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ , the maximum number of hosted zones that you can associate with the specified reusable delegation set.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdslType :: Lens.Lens' ReusableDelegationSetLimit ReusableDelegationSetLimitType
rdslType = Lens.lens (type' :: ReusableDelegationSetLimit -> ReusableDelegationSetLimitType) (\s a -> s {type' = a} :: ReusableDelegationSetLimit)
{-# DEPRECATED rdslType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromXML ReusableDelegationSetLimit where
  parseXML x =
    ReusableDelegationSetLimit'
      Lude.<$> (x Lude..@ "Value") Lude.<*> (x Lude..@ "Type")
