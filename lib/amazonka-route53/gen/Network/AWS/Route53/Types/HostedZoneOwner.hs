-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53.Types.HostedZoneOwner
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.HostedZoneOwner
  ( HostedZoneOwner (..),

    -- * Smart constructor
    mkHostedZoneOwner,

    -- * Lenses
    hzoOwningAccount,
    hzoOwningService,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53.Internal

-- | A complex type that identifies a hosted zone that a specified Amazon VPC is associated with and the owner of the hosted zone. If there is a value for @OwningAccount@ , there is no value for @OwningService@ , and vice versa.
--
-- /See:/ 'mkHostedZoneOwner' smart constructor.
data HostedZoneOwner = HostedZoneOwner'
  { owningAccount ::
      Lude.Maybe Lude.Text,
    owningService :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HostedZoneOwner' with the minimum fields required to make a request.
--
-- * 'owningAccount' - If the hosted zone was created by an AWS account, or was created by an AWS service that creates hosted zones using the current account, @OwningAccount@ contains the account ID of that account. For example, when you use AWS Cloud Map to create a hosted zone, Cloud Map creates the hosted zone using the current AWS account.
-- * 'owningService' - If an AWS service uses its own account to create a hosted zone and associate the specified VPC with that hosted zone, @OwningService@ contains an abbreviation that identifies the service. For example, if Amazon Elastic File System (Amazon EFS) created a hosted zone and associated a VPC with the hosted zone, the value of @OwningService@ is @efs.amazonaws.com@ .
mkHostedZoneOwner ::
  HostedZoneOwner
mkHostedZoneOwner =
  HostedZoneOwner'
    { owningAccount = Lude.Nothing,
      owningService = Lude.Nothing
    }

-- | If the hosted zone was created by an AWS account, or was created by an AWS service that creates hosted zones using the current account, @OwningAccount@ contains the account ID of that account. For example, when you use AWS Cloud Map to create a hosted zone, Cloud Map creates the hosted zone using the current AWS account.
--
-- /Note:/ Consider using 'owningAccount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzoOwningAccount :: Lens.Lens' HostedZoneOwner (Lude.Maybe Lude.Text)
hzoOwningAccount = Lens.lens (owningAccount :: HostedZoneOwner -> Lude.Maybe Lude.Text) (\s a -> s {owningAccount = a} :: HostedZoneOwner)
{-# DEPRECATED hzoOwningAccount "Use generic-lens or generic-optics with 'owningAccount' instead." #-}

-- | If an AWS service uses its own account to create a hosted zone and associate the specified VPC with that hosted zone, @OwningService@ contains an abbreviation that identifies the service. For example, if Amazon Elastic File System (Amazon EFS) created a hosted zone and associated a VPC with the hosted zone, the value of @OwningService@ is @efs.amazonaws.com@ .
--
-- /Note:/ Consider using 'owningService' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
hzoOwningService :: Lens.Lens' HostedZoneOwner (Lude.Maybe Lude.Text)
hzoOwningService = Lens.lens (owningService :: HostedZoneOwner -> Lude.Maybe Lude.Text) (\s a -> s {owningService = a} :: HostedZoneOwner)
{-# DEPRECATED hzoOwningService "Use generic-lens or generic-optics with 'owningService' instead." #-}

instance Lude.FromXML HostedZoneOwner where
  parseXML x =
    HostedZoneOwner'
      Lude.<$> (x Lude..@? "OwningAccount") Lude.<*> (x Lude..@? "OwningService")
