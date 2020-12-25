{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HostedZoneLimit
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HostedZoneLimit
  ( HostedZoneLimit (..),

    -- * Smart constructor
    mkHostedZoneLimit,

    -- * Lenses
    hzlType,
    hzlValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.HostedZoneLimitType as Types

-- | A complex type that contains the type of limit that you specified in the request and the current value for that limit.
--
-- /See:/ 'mkHostedZoneLimit' smart constructor.
data HostedZoneLimit = HostedZoneLimit'
  { -- | The limit that you requested. Valid values include the following:
    --
    --
    --     * __MAX_RRSETS_BY_ZONE__ : The maximum number of records that you can create in the specified hosted zone.
    --
    --
    --     * __MAX_VPCS_ASSOCIATED_BY_ZONE__ : The maximum number of Amazon VPCs that you can associate with the specified private hosted zone.
    type' :: Types.HostedZoneLimitType,
    -- | The current value for the limit that is specified by @Type@ .
    value :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HostedZoneLimit' value with any optional fields omitted.
mkHostedZoneLimit ::
  -- | 'type\''
  Types.HostedZoneLimitType ->
  -- | 'value'
  Core.Natural ->
  HostedZoneLimit
mkHostedZoneLimit type' value = HostedZoneLimit' {type', value}

-- | The limit that you requested. Valid values include the following:
--
--
--     * __MAX_RRSETS_BY_ZONE__ : The maximum number of records that you can create in the specified hosted zone.
--
--
--     * __MAX_VPCS_ASSOCIATED_BY_ZONE__ : The maximum number of Amazon VPCs that you can associate with the specified private hosted zone.
--
--
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzlType :: Lens.Lens' HostedZoneLimit Types.HostedZoneLimitType
hzlType = Lens.field @"type'"
{-# DEPRECATED hzlType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The current value for the limit that is specified by @Type@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzlValue :: Lens.Lens' HostedZoneLimit Core.Natural
hzlValue = Lens.field @"value"
{-# DEPRECATED hzlValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromXML HostedZoneLimit where
  parseXML x =
    HostedZoneLimit'
      Core.<$> (x Core..@ "Type") Core.<*> (x Core..@ "Value")
