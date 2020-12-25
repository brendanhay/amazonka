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
    rdslType,
    rdslValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.ReusableDelegationSetLimitType as Types

-- | A complex type that contains the type of limit that you specified in the request and the current value for that limit.
--
-- /See:/ 'mkReusableDelegationSetLimit' smart constructor.
data ReusableDelegationSetLimit = ReusableDelegationSetLimit'
  { -- | The limit that you requested: @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ , the maximum number of hosted zones that you can associate with the specified reusable delegation set.
    type' :: Types.ReusableDelegationSetLimitType,
    -- | The current value for the @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ limit.
    value :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ReusableDelegationSetLimit' value with any optional fields omitted.
mkReusableDelegationSetLimit ::
  -- | 'type\''
  Types.ReusableDelegationSetLimitType ->
  -- | 'value'
  Core.Natural ->
  ReusableDelegationSetLimit
mkReusableDelegationSetLimit type' value =
  ReusableDelegationSetLimit' {type', value}

-- | The limit that you requested: @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ , the maximum number of hosted zones that you can associate with the specified reusable delegation set.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdslType :: Lens.Lens' ReusableDelegationSetLimit Types.ReusableDelegationSetLimitType
rdslType = Lens.field @"type'"
{-# DEPRECATED rdslType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The current value for the @MAX_ZONES_BY_REUSABLE_DELEGATION_SET@ limit.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rdslValue :: Lens.Lens' ReusableDelegationSetLimit Core.Natural
rdslValue = Lens.field @"value"
{-# DEPRECATED rdslValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromXML ReusableDelegationSetLimit where
  parseXML x =
    ReusableDelegationSetLimit'
      Core.<$> (x Core..@ "Type") Core.<*> (x Core..@ "Value")
