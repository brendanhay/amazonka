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
    hzsOwner,
    hzsName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.HostedZoneOwner

-- | In the response to a @ListHostedZonesByVPC@ request, the @HostedZoneSummaries@ element contains one @HostedZoneSummary@ element for each hosted zone that the specified Amazon VPC is associated with. Each @HostedZoneSummary@ element contains the hosted zone name and ID, and information about who owns the hosted zone.
--
-- /See:/ 'mkHostedZoneSummary' smart constructor.
data HostedZoneSummary = HostedZoneSummary'
  { -- | The Route 53 hosted zone ID of a private hosted zone that the specified VPC is associated with.
    hostedZoneId :: ResourceId,
    -- | The owner of a private hosted zone that the specified VPC is associated with. The owner can be either an AWS account or an AWS service.
    owner :: HostedZoneOwner,
    -- | The name of the private hosted zone, such as @example.com@ .
    name :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HostedZoneSummary' with the minimum fields required to make a request.
--
-- * 'hostedZoneId' - The Route 53 hosted zone ID of a private hosted zone that the specified VPC is associated with.
-- * 'owner' - The owner of a private hosted zone that the specified VPC is associated with. The owner can be either an AWS account or an AWS service.
-- * 'name' - The name of the private hosted zone, such as @example.com@ .
mkHostedZoneSummary ::
  -- | 'hostedZoneId'
  ResourceId ->
  -- | 'owner'
  HostedZoneOwner ->
  -- | 'name'
  Lude.Text ->
  HostedZoneSummary
mkHostedZoneSummary pHostedZoneId_ pOwner_ pName_ =
  HostedZoneSummary'
    { hostedZoneId = pHostedZoneId_,
      owner = pOwner_,
      name = pName_
    }

-- | The Route 53 hosted zone ID of a private hosted zone that the specified VPC is associated with.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzsHostedZoneId :: Lens.Lens' HostedZoneSummary ResourceId
hzsHostedZoneId = Lens.lens (hostedZoneId :: HostedZoneSummary -> ResourceId) (\s a -> s {hostedZoneId = a} :: HostedZoneSummary)
{-# DEPRECATED hzsHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | The owner of a private hosted zone that the specified VPC is associated with. The owner can be either an AWS account or an AWS service.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzsOwner :: Lens.Lens' HostedZoneSummary HostedZoneOwner
hzsOwner = Lens.lens (owner :: HostedZoneSummary -> HostedZoneOwner) (\s a -> s {owner = a} :: HostedZoneSummary)
{-# DEPRECATED hzsOwner "Use generic-lens or generic-optics with 'owner' instead." #-}

-- | The name of the private hosted zone, such as @example.com@ .
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzsName :: Lens.Lens' HostedZoneSummary Lude.Text
hzsName = Lens.lens (name :: HostedZoneSummary -> Lude.Text) (\s a -> s {name = a} :: HostedZoneSummary)
{-# DEPRECATED hzsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromXML HostedZoneSummary where
  parseXML x =
    HostedZoneSummary'
      Lude.<$> (x Lude..@ "HostedZoneId")
      Lude.<*> (x Lude..@ "Owner")
      Lude.<*> (x Lude..@ "Name")
