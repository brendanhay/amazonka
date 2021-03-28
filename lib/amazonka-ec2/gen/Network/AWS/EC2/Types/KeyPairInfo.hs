{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.KeyPairInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.KeyPairInfo
  ( KeyPairInfo (..)
  -- * Smart constructor
  , mkKeyPairInfo
  -- * Lenses
  , kpiKeyFingerprint
  , kpiKeyName
  , kpiKeyPairId
  , kpiTags
  ) where

import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a key pair.
--
-- /See:/ 'mkKeyPairInfo' smart constructor.
data KeyPairInfo = KeyPairInfo'
  { keyFingerprint :: Core.Maybe Core.Text
    -- ^ If you used 'CreateKeyPair' to create the key pair, this is the SHA-1 digest of the DER encoded private key. If you used 'ImportKeyPair' to provide AWS the public key, this is the MD5 public key fingerprint as specified in section 4 of RFC4716.
  , keyName :: Core.Maybe Core.Text
    -- ^ The name of the key pair.
  , keyPairId :: Core.Maybe Core.Text
    -- ^ The ID of the key pair.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags applied to the key pair.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KeyPairInfo' value with any optional fields omitted.
mkKeyPairInfo
    :: KeyPairInfo
mkKeyPairInfo
  = KeyPairInfo'{keyFingerprint = Core.Nothing,
                 keyName = Core.Nothing, keyPairId = Core.Nothing,
                 tags = Core.Nothing}

-- | If you used 'CreateKeyPair' to create the key pair, this is the SHA-1 digest of the DER encoded private key. If you used 'ImportKeyPair' to provide AWS the public key, this is the MD5 public key fingerprint as specified in section 4 of RFC4716.
--
-- /Note:/ Consider using 'keyFingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpiKeyFingerprint :: Lens.Lens' KeyPairInfo (Core.Maybe Core.Text)
kpiKeyFingerprint = Lens.field @"keyFingerprint"
{-# INLINEABLE kpiKeyFingerprint #-}
{-# DEPRECATED keyFingerprint "Use generic-lens or generic-optics with 'keyFingerprint' instead"  #-}

-- | The name of the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpiKeyName :: Lens.Lens' KeyPairInfo (Core.Maybe Core.Text)
kpiKeyName = Lens.field @"keyName"
{-# INLINEABLE kpiKeyName #-}
{-# DEPRECATED keyName "Use generic-lens or generic-optics with 'keyName' instead"  #-}

-- | The ID of the key pair.
--
-- /Note:/ Consider using 'keyPairId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpiKeyPairId :: Lens.Lens' KeyPairInfo (Core.Maybe Core.Text)
kpiKeyPairId = Lens.field @"keyPairId"
{-# INLINEABLE kpiKeyPairId #-}
{-# DEPRECATED keyPairId "Use generic-lens or generic-optics with 'keyPairId' instead"  #-}

-- | Any tags applied to the key pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpiTags :: Lens.Lens' KeyPairInfo (Core.Maybe [Types.Tag])
kpiTags = Lens.field @"tags"
{-# INLINEABLE kpiTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML KeyPairInfo where
        parseXML x
          = KeyPairInfo' Core.<$>
              (x Core..@? "keyFingerprint") Core.<*> x Core..@? "keyName"
                Core.<*> x Core..@? "keyPairId"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
