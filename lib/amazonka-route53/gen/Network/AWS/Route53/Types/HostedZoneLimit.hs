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
    hzlValue,
    hzlType,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.HostedZoneLimitType

-- | A complex type that contains the type of limit that you specified in the request and the current value for that limit.
--
-- /See:/ 'mkHostedZoneLimit' smart constructor.
data HostedZoneLimit = HostedZoneLimit'
  { -- | The current value for the limit that is specified by @Type@ .
    value :: Lude.Natural,
    -- | The limit that you requested. Valid values include the following:
    --
    --
    --     * __MAX_RRSETS_BY_ZONE__ : The maximum number of records that you can create in the specified hosted zone.
    --
    --
    --     * __MAX_VPCS_ASSOCIATED_BY_ZONE__ : The maximum number of Amazon VPCs that you can associate with the specified private hosted zone.
    type' :: HostedZoneLimitType
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HostedZoneLimit' with the minimum fields required to make a request.
--
-- * 'value' - The current value for the limit that is specified by @Type@ .
-- * 'type'' - The limit that you requested. Valid values include the following:
--
--
--     * __MAX_RRSETS_BY_ZONE__ : The maximum number of records that you can create in the specified hosted zone.
--
--
--     * __MAX_VPCS_ASSOCIATED_BY_ZONE__ : The maximum number of Amazon VPCs that you can associate with the specified private hosted zone.
mkHostedZoneLimit ::
  -- | 'value'
  Lude.Natural ->
  -- | 'type''
  HostedZoneLimitType ->
  HostedZoneLimit
mkHostedZoneLimit pValue_ pType_ =
  HostedZoneLimit' {value = pValue_, type' = pType_}

-- | The current value for the limit that is specified by @Type@ .
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzlValue :: Lens.Lens' HostedZoneLimit Lude.Natural
hzlValue = Lens.lens (value :: HostedZoneLimit -> Lude.Natural) (\s a -> s {value = a} :: HostedZoneLimit)
{-# DEPRECATED hzlValue "Use generic-lens or generic-optics with 'value' instead." #-}

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
hzlType :: Lens.Lens' HostedZoneLimit HostedZoneLimitType
hzlType = Lens.lens (type' :: HostedZoneLimit -> HostedZoneLimitType) (\s a -> s {type' = a} :: HostedZoneLimit)
{-# DEPRECATED hzlType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromXML HostedZoneLimit where
  parseXML x =
    HostedZoneLimit'
      Lude.<$> (x Lude..@ "Value") Lude.<*> (x Lude..@ "Type")
