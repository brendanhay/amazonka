{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkMail.Types.Domain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.WorkMail.Types.Domain
  ( Domain (..),

    -- * Smart constructor
    mkDomain,

    -- * Lenses
    dDomainName,
    dHostedZoneId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.WorkMail.Types.DomainName as Types
import qualified Network.AWS.WorkMail.Types.HostedZoneId as Types

-- | The domain to associate with an Amazon WorkMail organization.
--
-- When you configure a domain hosted in Amazon Route 53 (Route 53), all recommended DNS records are added to the organization when you create it. For more information, see <https://docs.aws.amazon.com/workmail/latest/adminguide/add_domain.html Adding a domain> in the /Amazon WorkMail Administrator Guide/ .
--
-- /See:/ 'mkDomain' smart constructor.
data Domain = Domain'
  { -- | The fully qualified domain name.
    domainName :: Core.Maybe Types.DomainName,
    -- | The hosted zone ID for a domain hosted in Route 53. Required when configuring a domain hosted in Route 53.
    hostedZoneId :: Core.Maybe Types.HostedZoneId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Domain' value with any optional fields omitted.
mkDomain ::
  Domain
mkDomain =
  Domain' {domainName = Core.Nothing, hostedZoneId = Core.Nothing}

-- | The fully qualified domain name.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainName :: Lens.Lens' Domain (Core.Maybe Types.DomainName)
dDomainName = Lens.field @"domainName"
{-# DEPRECATED dDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

-- | The hosted zone ID for a domain hosted in Route 53. Required when configuring a domain hosted in Route 53.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHostedZoneId :: Lens.Lens' Domain (Core.Maybe Types.HostedZoneId)
dHostedZoneId = Lens.field @"hostedZoneId"
{-# DEPRECATED dHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

instance Core.FromJSON Domain where
  toJSON Domain {..} =
    Core.object
      ( Core.catMaybes
          [ ("DomainName" Core..=) Core.<$> domainName,
            ("HostedZoneId" Core..=) Core.<$> hostedZoneId
          ]
      )
