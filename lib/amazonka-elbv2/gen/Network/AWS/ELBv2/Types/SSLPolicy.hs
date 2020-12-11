-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.SSLPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.SSLPolicy
  ( SSLPolicy (..),

    -- * Smart constructor
    mkSSLPolicy,

    -- * Lenses
    spCiphers,
    spName,
    spSSLProtocols,
  )
where

import Network.AWS.ELBv2.Types.Cipher
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a policy used for SSL negotiation.
--
-- /See:/ 'mkSSLPolicy' smart constructor.
data SSLPolicy = SSLPolicy'
  { ciphers :: Lude.Maybe [Cipher],
    name :: Lude.Maybe Lude.Text,
    sslProtocols :: Lude.Maybe [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SSLPolicy' with the minimum fields required to make a request.
--
-- * 'ciphers' - The ciphers.
-- * 'name' - The name of the policy.
-- * 'sslProtocols' - The protocols.
mkSSLPolicy ::
  SSLPolicy
mkSSLPolicy =
  SSLPolicy'
    { ciphers = Lude.Nothing,
      name = Lude.Nothing,
      sslProtocols = Lude.Nothing
    }

-- | The ciphers.
--
-- /Note:/ Consider using 'ciphers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spCiphers :: Lens.Lens' SSLPolicy (Lude.Maybe [Cipher])
spCiphers = Lens.lens (ciphers :: SSLPolicy -> Lude.Maybe [Cipher]) (\s a -> s {ciphers = a} :: SSLPolicy)
{-# DEPRECATED spCiphers "Use generic-lens or generic-optics with 'ciphers' instead." #-}

-- | The name of the policy.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spName :: Lens.Lens' SSLPolicy (Lude.Maybe Lude.Text)
spName = Lens.lens (name :: SSLPolicy -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: SSLPolicy)
{-# DEPRECATED spName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The protocols.
--
-- /Note:/ Consider using 'sslProtocols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
spSSLProtocols :: Lens.Lens' SSLPolicy (Lude.Maybe [Lude.Text])
spSSLProtocols = Lens.lens (sslProtocols :: SSLPolicy -> Lude.Maybe [Lude.Text]) (\s a -> s {sslProtocols = a} :: SSLPolicy)
{-# DEPRECATED spSSLProtocols "Use generic-lens or generic-optics with 'sslProtocols' instead." #-}

instance Lude.FromXML SSLPolicy where
  parseXML x =
    SSLPolicy'
      Lude.<$> ( x Lude..@? "Ciphers" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
      Lude.<*> (x Lude..@? "Name")
      Lude.<*> ( x Lude..@? "SslProtocols" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "member")
               )
