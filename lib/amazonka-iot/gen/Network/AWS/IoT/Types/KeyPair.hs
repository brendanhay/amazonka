{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.KeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.IoT.Types.KeyPair
  ( KeyPair (..)
  -- * Smart constructor
  , mkKeyPair
  -- * Lenses
  , kpPrivateKey
  , kpPublicKey
  ) where

import qualified Network.AWS.IoT.Types.PrivateKey as Types
import qualified Network.AWS.IoT.Types.PublicKey as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a key pair.
--
-- /See:/ 'mkKeyPair' smart constructor.
data KeyPair = KeyPair'
  { privateKey :: Core.Maybe Types.PrivateKey
    -- ^ The private key.
  , publicKey :: Core.Maybe Types.PublicKey
    -- ^ The public key.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KeyPair' value with any optional fields omitted.
mkKeyPair
    :: KeyPair
mkKeyPair
  = KeyPair'{privateKey = Core.Nothing, publicKey = Core.Nothing}

-- | The private key.
--
-- /Note:/ Consider using 'privateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpPrivateKey :: Lens.Lens' KeyPair (Core.Maybe Types.PrivateKey)
kpPrivateKey = Lens.field @"privateKey"
{-# INLINEABLE kpPrivateKey #-}
{-# DEPRECATED privateKey "Use generic-lens or generic-optics with 'privateKey' instead"  #-}

-- | The public key.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpPublicKey :: Lens.Lens' KeyPair (Core.Maybe Types.PublicKey)
kpPublicKey = Lens.field @"publicKey"
{-# INLINEABLE kpPublicKey #-}
{-# DEPRECATED publicKey "Use generic-lens or generic-optics with 'publicKey' instead"  #-}

instance Core.FromJSON KeyPair where
        parseJSON
          = Core.withObject "KeyPair" Core.$
              \ x ->
                KeyPair' Core.<$>
                  (x Core..:? "PrivateKey") Core.<*> x Core..:? "PublicKey"
