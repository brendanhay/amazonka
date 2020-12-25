{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HostedZoneSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HostedZoneSummary
  ( HostedZoneSummary (..),

    -- * Smart constructor
    mkHostedZoneSummary,

    -- * Lenses
    hzsHostedZoneId,
    hzsName,
    hzsOwner,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Route53.Internal as Types
import qualified Network.AWS.Route53.Types.HostedZoneOwner as Types
import qualified Network.AWS.Route53.Types.Name as Types

-- | In the response to a @ListHostedZonesByVPC@ request, the @HostedZoneSummaries@ element contains one @HostedZoneSummary@ element for each hosted zone that the specified Amazon VPC is associated with. Each @HostedZoneSummary@ element contains the hosted zone name and ID, and information about who owns the hosted zone.
--
-- /See:/ 'mkHostedZoneSummary' smart constructor.
data HostedZoneSummary = HostedZoneSummary'
  { -- | The Route 53 hosted zone ID of a private hosted zone that the specified VPC is associated with.
    hostedZoneId :: Types.ResourceId,
    -- | The name of the private hosted zone, such as @example.com@ .
    name :: Types.Name,
    -- | The owner of a private hosted zone that the specified VPC is associated with. The owner can be either an AWS account or an AWS service.
    owner :: Types.HostedZoneOwner
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'HostedZoneSummary' value with any optional fields omitted.
mkHostedZoneSummary ::
  -- | 'hostedZoneId'
  Types.ResourceId ->
  -- | 'name'
  Types.Name ->
  -- | 'owner'
  Types.HostedZoneOwner ->
  HostedZoneSummary
mkHostedZoneSummary hostedZoneId name owner =
  HostedZoneSummary' {hostedZoneId, name, owner}

-- | The Route 53 hosted zone ID of a private hosted zone that the specified VPC is associated with.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzsHostedZoneId :: Lens.Lens' HostedZoneSummary Types.ResourceId
hzsHostedZoneId = Lens.field @"hostedZoneId"
{-# DEPRECATED hzsHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | The name of the private hosted zone, such as @example.com@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzsName :: Lens.Lens' HostedZoneSummary Types.Name
hzsName = Lens.field @"name"
{-# DEPRECATED hzsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The owner of a private hosted zone that the specified VPC is associated with. The owner can be either an AWS account or an AWS service.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzsOwner :: Lens.Lens' HostedZoneSummary Types.HostedZoneOwner
hzsOwner = Lens.field @"owner"
{-# DEPRECATED hzsOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

instance Core.FromXML HostedZoneSummary where
  parseXML x =
    HostedZoneSummary'
      Core.<$> (x Core..@ "HostedZoneId")
      Core.<*> (x Core..@ "Name")
      Core.<*> (x Core..@ "Owner")
