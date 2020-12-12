{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.EncryptionEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFront.Types.EncryptionEntity
  ( EncryptionEntity (..),

    -- * Smart constructor
    mkEncryptionEntity,

    -- * Lenses
    eePublicKeyId,
    eeProviderId,
    eeFieldPatterns,
  )
where

import Network.AWS.CloudFront.Types.FieldPatterns
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Complex data type for field-level encryption profiles that includes the encryption key and field pattern specifications.
--
-- /See:/ 'mkEncryptionEntity' smart constructor.
data EncryptionEntity = EncryptionEntity'
  { publicKeyId :: Lude.Text,
    providerId :: Lude.Text,
    fieldPatterns :: FieldPatterns
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'EncryptionEntity' with the minimum fields required to make a request.
--
-- * 'fieldPatterns' - Field patterns in a field-level encryption content type profile specify the fields that you want to be encrypted. You can provide the full field name, or any beginning characters followed by a wildcard (*). You can't overlap field patterns. For example, you can't have both ABC* and AB*. Note that field patterns are case-sensitive.
-- * 'providerId' - The provider associated with the public key being used for encryption. This value must also be provided with the private key for applications to be able to decrypt data.
-- * 'publicKeyId' - The public key associated with a set of field-level encryption patterns, to be used when encrypting the fields that match the patterns.
mkEncryptionEntity ::
  -- | 'publicKeyId'
  Lude.Text ->
  -- | 'providerId'
  Lude.Text ->
  -- | 'fieldPatterns'
  FieldPatterns ->
  EncryptionEntity
mkEncryptionEntity pPublicKeyId_ pProviderId_ pFieldPatterns_ =
  EncryptionEntity'
    { publicKeyId = pPublicKeyId_,
      providerId = pProviderId_,
      fieldPatterns = pFieldPatterns_
    }

-- | The public key associated with a set of field-level encryption patterns, to be used when encrypting the fields that match the patterns.
--
-- /Note:/ Consider using 'publicKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eePublicKeyId :: Lens.Lens' EncryptionEntity Lude.Text
eePublicKeyId = Lens.lens (publicKeyId :: EncryptionEntity -> Lude.Text) (\s a -> s {publicKeyId = a} :: EncryptionEntity)
{-# DEPRECATED eePublicKeyId "Use generic-lens or generic-optics with 'publicKeyId' instead." #-}

-- | The provider associated with the public key being used for encryption. This value must also be provided with the private key for applications to be able to decrypt data.
--
-- /Note:/ Consider using 'providerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeProviderId :: Lens.Lens' EncryptionEntity Lude.Text
eeProviderId = Lens.lens (providerId :: EncryptionEntity -> Lude.Text) (\s a -> s {providerId = a} :: EncryptionEntity)
{-# DEPRECATED eeProviderId "Use generic-lens or generic-optics with 'providerId' instead." #-}

-- | Field patterns in a field-level encryption content type profile specify the fields that you want to be encrypted. You can provide the full field name, or any beginning characters followed by a wildcard (*). You can't overlap field patterns. For example, you can't have both ABC* and AB*. Note that field patterns are case-sensitive.
--
-- /Note:/ Consider using 'fieldPatterns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeFieldPatterns :: Lens.Lens' EncryptionEntity FieldPatterns
eeFieldPatterns = Lens.lens (fieldPatterns :: EncryptionEntity -> FieldPatterns) (\s a -> s {fieldPatterns = a} :: EncryptionEntity)
{-# DEPRECATED eeFieldPatterns "Use generic-lens or generic-optics with 'fieldPatterns' instead." #-}

instance Lude.FromXML EncryptionEntity where
  parseXML x =
    EncryptionEntity'
      Lude.<$> (x Lude..@ "PublicKeyId")
      Lude.<*> (x Lude..@ "ProviderId")
      Lude.<*> (x Lude..@ "FieldPatterns")

instance Lude.ToXML EncryptionEntity where
  toXML EncryptionEntity' {..} =
    Lude.mconcat
      [ "PublicKeyId" Lude.@= publicKeyId,
        "ProviderId" Lude.@= providerId,
        "FieldPatterns" Lude.@= fieldPatterns
      ]
