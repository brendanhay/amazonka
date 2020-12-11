-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.LoadBalancerTLSCertificateSummary
  ( LoadBalancerTLSCertificateSummary (..),

    -- * Smart constructor
    mkLoadBalancerTLSCertificateSummary,

    -- * Lenses
    lbtcsIsAttached,
    lbtcsName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Provides a summary of SSL/TLS certificate metadata.
--
-- /See:/ 'mkLoadBalancerTLSCertificateSummary' smart constructor.
data LoadBalancerTLSCertificateSummary = LoadBalancerTLSCertificateSummary'
  { isAttached ::
      Lude.Maybe Lude.Bool,
    name ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LoadBalancerTLSCertificateSummary' with the minimum fields required to make a request.
--
-- * 'isAttached' - When @true@ , the SSL/TLS certificate is attached to the Lightsail load balancer.
-- * 'name' - The name of the SSL/TLS certificate.
mkLoadBalancerTLSCertificateSummary ::
  LoadBalancerTLSCertificateSummary
mkLoadBalancerTLSCertificateSummary =
  LoadBalancerTLSCertificateSummary'
    { isAttached = Lude.Nothing,
      name = Lude.Nothing
    }

-- | When @true@ , the SSL/TLS certificate is attached to the Lightsail load balancer.
--
-- /Note:/ Consider using 'isAttached' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcsIsAttached :: Lens.Lens' LoadBalancerTLSCertificateSummary (Lude.Maybe Lude.Bool)
lbtcsIsAttached = Lens.lens (isAttached :: LoadBalancerTLSCertificateSummary -> Lude.Maybe Lude.Bool) (\s a -> s {isAttached = a} :: LoadBalancerTLSCertificateSummary)
{-# DEPRECATED lbtcsIsAttached "Use generic-lens or generic-optics with 'isAttached' instead." #-}

-- | The name of the SSL/TLS certificate.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbtcsName :: Lens.Lens' LoadBalancerTLSCertificateSummary (Lude.Maybe Lude.Text)
lbtcsName = Lens.lens (name :: LoadBalancerTLSCertificateSummary -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: LoadBalancerTLSCertificateSummary)
{-# DEPRECATED lbtcsName "Use generic-lens or generic-optics with 'name' instead." #-}

instance Lude.FromJSON LoadBalancerTLSCertificateSummary where
  parseJSON =
    Lude.withObject
      "LoadBalancerTLSCertificateSummary"
      ( \x ->
          LoadBalancerTLSCertificateSummary'
            Lude.<$> (x Lude..:? "isAttached") Lude.<*> (x Lude..:? "name")
      )
