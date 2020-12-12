{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudSearch.Types.DomainEndpointOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearch.Types.DomainEndpointOptions
  ( DomainEndpointOptions (..),

    -- * Smart constructor
    mkDomainEndpointOptions,

    -- * Lenses
    deoEnforceHTTPS,
    deoTLSSecurityPolicy,
  )
where

import Network.AWS.CloudSearch.Types.TLSSecurityPolicy
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The domain's endpoint options.
--
-- /See:/ 'mkDomainEndpointOptions' smart constructor.
data DomainEndpointOptions = DomainEndpointOptions'
  { enforceHTTPS ::
      Lude.Maybe Lude.Bool,
    tlsSecurityPolicy ::
      Lude.Maybe TLSSecurityPolicy
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DomainEndpointOptions' with the minimum fields required to make a request.
--
-- * 'enforceHTTPS' - Whether the domain is HTTPS only enabled.
-- * 'tlsSecurityPolicy' - The minimum required TLS version
mkDomainEndpointOptions ::
  DomainEndpointOptions
mkDomainEndpointOptions =
  DomainEndpointOptions'
    { enforceHTTPS = Lude.Nothing,
      tlsSecurityPolicy = Lude.Nothing
    }

-- | Whether the domain is HTTPS only enabled.
--
-- /Note:/ Consider using 'enforceHTTPS' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoEnforceHTTPS :: Lens.Lens' DomainEndpointOptions (Lude.Maybe Lude.Bool)
deoEnforceHTTPS = Lens.lens (enforceHTTPS :: DomainEndpointOptions -> Lude.Maybe Lude.Bool) (\s a -> s {enforceHTTPS = a} :: DomainEndpointOptions)
{-# DEPRECATED deoEnforceHTTPS "Use generic-lens or generic-optics with 'enforceHTTPS' instead." #-}

-- | The minimum required TLS version
--
-- /Note:/ Consider using 'tlsSecurityPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deoTLSSecurityPolicy :: Lens.Lens' DomainEndpointOptions (Lude.Maybe TLSSecurityPolicy)
deoTLSSecurityPolicy = Lens.lens (tlsSecurityPolicy :: DomainEndpointOptions -> Lude.Maybe TLSSecurityPolicy) (\s a -> s {tlsSecurityPolicy = a} :: DomainEndpointOptions)
{-# DEPRECATED deoTLSSecurityPolicy "Use generic-lens or generic-optics with 'tlsSecurityPolicy' instead." #-}

instance Lude.FromXML DomainEndpointOptions where
  parseXML x =
    DomainEndpointOptions'
      Lude.<$> (x Lude..@? "EnforceHTTPS")
      Lude.<*> (x Lude..@? "TLSSecurityPolicy")

instance Lude.ToQuery DomainEndpointOptions where
  toQuery DomainEndpointOptions' {..} =
    Lude.mconcat
      [ "EnforceHTTPS" Lude.=: enforceHTTPS,
        "TLSSecurityPolicy" Lude.=: tlsSecurityPolicy
      ]
