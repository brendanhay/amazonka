-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.KeyPairInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.KeyPairInfo
  ( KeyPairInfo (..),

    -- * Smart constructor
    mkKeyPairInfo,

    -- * Lenses
    kpiKeyFingerprint,
    kpiKeyName,
    kpiKeyPairId,
    kpiTags,
  )
where

import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a key pair.
--
-- /See:/ 'mkKeyPairInfo' smart constructor.
data KeyPairInfo = KeyPairInfo'
  { keyFingerprint ::
      Lude.Maybe Lude.Text,
    keyName :: Lude.Maybe Lude.Text,
    keyPairId :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KeyPairInfo' with the minimum fields required to make a request.
--
-- * 'keyFingerprint' - If you used 'CreateKeyPair' to create the key pair, this is the SHA-1 digest of the DER encoded private key. If you used 'ImportKeyPair' to provide AWS the public key, this is the MD5 public key fingerprint as specified in section 4 of RFC4716.
-- * 'keyName' - The name of the key pair.
-- * 'keyPairId' - The ID of the key pair.
-- * 'tags' - Any tags applied to the key pair.
mkKeyPairInfo ::
  KeyPairInfo
mkKeyPairInfo =
  KeyPairInfo'
    { keyFingerprint = Lude.Nothing,
      keyName = Lude.Nothing,
      keyPairId = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | If you used 'CreateKeyPair' to create the key pair, this is the SHA-1 digest of the DER encoded private key. If you used 'ImportKeyPair' to provide AWS the public key, this is the MD5 public key fingerprint as specified in section 4 of RFC4716.
--
-- /Note:/ Consider using 'keyFingerprint' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpiKeyFingerprint :: Lens.Lens' KeyPairInfo (Lude.Maybe Lude.Text)
kpiKeyFingerprint = Lens.lens (keyFingerprint :: KeyPairInfo -> Lude.Maybe Lude.Text) (\s a -> s {keyFingerprint = a} :: KeyPairInfo)
{-# DEPRECATED kpiKeyFingerprint "Use generic-lens or generic-optics with 'keyFingerprint' instead." #-}

-- | The name of the key pair.
--
-- /Note:/ Consider using 'keyName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpiKeyName :: Lens.Lens' KeyPairInfo (Lude.Maybe Lude.Text)
kpiKeyName = Lens.lens (keyName :: KeyPairInfo -> Lude.Maybe Lude.Text) (\s a -> s {keyName = a} :: KeyPairInfo)
{-# DEPRECATED kpiKeyName "Use generic-lens or generic-optics with 'keyName' instead." #-}

-- | The ID of the key pair.
--
-- /Note:/ Consider using 'keyPairId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpiKeyPairId :: Lens.Lens' KeyPairInfo (Lude.Maybe Lude.Text)
kpiKeyPairId = Lens.lens (keyPairId :: KeyPairInfo -> Lude.Maybe Lude.Text) (\s a -> s {keyPairId = a} :: KeyPairInfo)
{-# DEPRECATED kpiKeyPairId "Use generic-lens or generic-optics with 'keyPairId' instead." #-}

-- | Any tags applied to the key pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpiTags :: Lens.Lens' KeyPairInfo (Lude.Maybe [Tag])
kpiTags = Lens.lens (tags :: KeyPairInfo -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: KeyPairInfo)
{-# DEPRECATED kpiTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.FromXML KeyPairInfo where
  parseXML x =
    KeyPairInfo'
      Lude.<$> (x Lude..@? "keyFingerprint")
      Lude.<*> (x Lude..@? "keyName")
      Lude.<*> (x Lude..@? "keyPairId")
      Lude.<*> ( x Lude..@? "tagSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
