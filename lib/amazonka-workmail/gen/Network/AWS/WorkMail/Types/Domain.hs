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
    dHostedZoneId,
    dDomainName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The domain to associate with an Amazon WorkMail organization.
--
-- When you configure a domain hosted in Amazon Route 53 (Route 53), all recommended DNS records are added to the organization when you create it. For more information, see <https://docs.aws.amazon.com/workmail/latest/adminguide/add_domain.html Adding a domain> in the /Amazon WorkMail Administrator Guide/ .
--
-- /See:/ 'mkDomain' smart constructor.
data Domain = Domain'
  { hostedZoneId :: Lude.Maybe Lude.Text,
    domainName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Domain' with the minimum fields required to make a request.
--
-- * 'domainName' - The fully qualified domain name.
-- * 'hostedZoneId' - The hosted zone ID for a domain hosted in Route 53. Required when configuring a domain hosted in Route 53.
mkDomain ::
  Domain
mkDomain =
  Domain' {hostedZoneId = Lude.Nothing, domainName = Lude.Nothing}

-- | The hosted zone ID for a domain hosted in Route 53. Required when configuring a domain hosted in Route 53.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dHostedZoneId :: Lens.Lens' Domain (Lude.Maybe Lude.Text)
dHostedZoneId = Lens.lens (hostedZoneId :: Domain -> Lude.Maybe Lude.Text) (\s a -> s {hostedZoneId = a} :: Domain)
{-# DEPRECATED dHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

-- | The fully qualified domain name.
--
-- /Note:/ Consider using 'domainName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dDomainName :: Lens.Lens' Domain (Lude.Maybe Lude.Text)
dDomainName = Lens.lens (domainName :: Domain -> Lude.Maybe Lude.Text) (\s a -> s {domainName = a} :: Domain)
{-# DEPRECATED dDomainName "Use generic-lens or generic-optics with 'domainName' instead." #-}

instance Lude.ToJSON Domain where
  toJSON Domain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("HostedZoneId" Lude..=) Lude.<$> hostedZoneId,
            ("DomainName" Lude..=) Lude.<$> domainName
          ]
      )
