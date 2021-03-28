{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.SslConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Types.SslConfiguration
  ( SslConfiguration (..)
  -- * Smart constructor
  , mkSslConfiguration
  -- * Lenses
  , scCertificate
  , scPrivateKey
  , scChain
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an app's SSL configuration.
--
-- /See:/ 'mkSslConfiguration' smart constructor.
data SslConfiguration = SslConfiguration'
  { certificate :: Core.Maybe Core.Text
    -- ^ The contents of the certificate's domain.crt file.
  , privateKey :: Core.Maybe Core.Text
    -- ^ The private key; the contents of the certificate's domain.kex file.
  , chain :: Core.Maybe Core.Text
    -- ^ Optional. Can be used to specify an intermediate certificate authority key or client authentication.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SslConfiguration' value with any optional fields omitted.
mkSslConfiguration
    :: SslConfiguration
mkSslConfiguration
  = SslConfiguration'{certificate = Core.Nothing,
                      privateKey = Core.Nothing, chain = Core.Nothing}

-- | The contents of the certificate's domain.crt file.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scCertificate :: Lens.Lens' SslConfiguration (Core.Maybe Core.Text)
scCertificate = Lens.field @"certificate"
{-# INLINEABLE scCertificate #-}
{-# DEPRECATED certificate "Use generic-lens or generic-optics with 'certificate' instead"  #-}

-- | The private key; the contents of the certificate's domain.kex file.
--
-- /Note:/ Consider using 'privateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scPrivateKey :: Lens.Lens' SslConfiguration (Core.Maybe Core.Text)
scPrivateKey = Lens.field @"privateKey"
{-# INLINEABLE scPrivateKey #-}
{-# DEPRECATED privateKey "Use generic-lens or generic-optics with 'privateKey' instead"  #-}

-- | Optional. Can be used to specify an intermediate certificate authority key or client authentication.
--
-- /Note:/ Consider using 'chain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scChain :: Lens.Lens' SslConfiguration (Core.Maybe Core.Text)
scChain = Lens.field @"chain"
{-# INLINEABLE scChain #-}
{-# DEPRECATED chain "Use generic-lens or generic-optics with 'chain' instead"  #-}

instance Core.FromJSON SslConfiguration where
        toJSON SslConfiguration{..}
          = Core.object
              (Core.catMaybes
                 [("Certificate" Core..=) Core.<$> certificate,
                  ("PrivateKey" Core..=) Core.<$> privateKey,
                  ("Chain" Core..=) Core.<$> chain])

instance Core.FromJSON SslConfiguration where
        parseJSON
          = Core.withObject "SslConfiguration" Core.$
              \ x ->
                SslConfiguration' Core.<$>
                  (x Core..:? "Certificate") Core.<*> x Core..:? "PrivateKey"
                    Core.<*> x Core..:? "Chain"
