-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.DNSProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.DNSProperties
  ( DNSProperties (..),

    -- * Smart constructor
    mkDNSProperties,

    -- * Lenses
    dpHostedZoneId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A complex type that contains the ID for the Route 53 hosted zone that AWS Cloud Map creates when you create a namespace.
--
-- /See:/ 'mkDNSProperties' smart constructor.
newtype DNSProperties = DNSProperties'
  { hostedZoneId ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DNSProperties' with the minimum fields required to make a request.
--
-- * 'hostedZoneId' - The ID for the Route 53 hosted zone that AWS Cloud Map creates when you create a namespace.
mkDNSProperties ::
  DNSProperties
mkDNSProperties = DNSProperties' {hostedZoneId = Lude.Nothing}

-- | The ID for the Route 53 hosted zone that AWS Cloud Map creates when you create a namespace.
--
-- /Note:/ Consider using 'hostedZoneId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dpHostedZoneId :: Lens.Lens' DNSProperties (Lude.Maybe Lude.Text)
dpHostedZoneId = Lens.lens (hostedZoneId :: DNSProperties -> Lude.Maybe Lude.Text) (\s a -> s {hostedZoneId = a} :: DNSProperties)
{-# DEPRECATED dpHostedZoneId "Use generic-lens or generic-optics with 'hostedZoneId' instead." #-}

instance Lude.FromJSON DNSProperties where
  parseJSON =
    Lude.withObject
      "DNSProperties"
      (\x -> DNSProperties' Lude.<$> (x Lude..:? "HostedZoneId"))
