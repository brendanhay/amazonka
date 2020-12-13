{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Route53AutoNaming.Types.NamespaceProperties
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53AutoNaming.Types.NamespaceProperties
  ( NamespaceProperties (..),

    -- * Smart constructor
    mkNamespaceProperties,

    -- * Lenses
    npDNSProperties,
    npHTTPProperties,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Route53AutoNaming.Types.DNSProperties
import Network.AWS.Route53AutoNaming.Types.HTTPProperties

-- | A complex type that contains information that is specific to the namespace type.
--
-- /See:/ 'mkNamespaceProperties' smart constructor.
data NamespaceProperties = NamespaceProperties'
  { -- | A complex type that contains the ID for the Route 53 hosted zone that AWS Cloud Map creates when you create a namespace.
    dnsProperties :: Lude.Maybe DNSProperties,
    -- | A complex type that contains the name of an HTTP namespace.
    hTTPProperties :: Lude.Maybe HTTPProperties
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NamespaceProperties' with the minimum fields required to make a request.
--
-- * 'dnsProperties' - A complex type that contains the ID for the Route 53 hosted zone that AWS Cloud Map creates when you create a namespace.
-- * 'hTTPProperties' - A complex type that contains the name of an HTTP namespace.
mkNamespaceProperties ::
  NamespaceProperties
mkNamespaceProperties =
  NamespaceProperties'
    { dnsProperties = Lude.Nothing,
      hTTPProperties = Lude.Nothing
    }

-- | A complex type that contains the ID for the Route 53 hosted zone that AWS Cloud Map creates when you create a namespace.
--
-- /Note:/ Consider using 'dnsProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npDNSProperties :: Lens.Lens' NamespaceProperties (Lude.Maybe DNSProperties)
npDNSProperties = Lens.lens (dnsProperties :: NamespaceProperties -> Lude.Maybe DNSProperties) (\s a -> s {dnsProperties = a} :: NamespaceProperties)
{-# DEPRECATED npDNSProperties "Use generic-lens or generic-optics with 'dnsProperties' instead." #-}

-- | A complex type that contains the name of an HTTP namespace.
--
-- /Note:/ Consider using 'hTTPProperties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
npHTTPProperties :: Lens.Lens' NamespaceProperties (Lude.Maybe HTTPProperties)
npHTTPProperties = Lens.lens (hTTPProperties :: NamespaceProperties -> Lude.Maybe HTTPProperties) (\s a -> s {hTTPProperties = a} :: NamespaceProperties)
{-# DEPRECATED npHTTPProperties "Use generic-lens or generic-optics with 'hTTPProperties' instead." #-}

instance Lude.FromJSON NamespaceProperties where
  parseJSON =
    Lude.withObject
      "NamespaceProperties"
      ( \x ->
          NamespaceProperties'
            Lude.<$> (x Lude..:? "DnsProperties")
            Lude.<*> (x Lude..:? "HttpProperties")
      )
