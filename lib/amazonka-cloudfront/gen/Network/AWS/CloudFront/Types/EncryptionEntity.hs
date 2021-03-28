{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFront.Types.EncryptionEntity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFront.Types.EncryptionEntity
  ( EncryptionEntity (..)
  -- * Smart constructor
  , mkEncryptionEntity
  -- * Lenses
  , eePublicKeyId
  , eeProviderId
  , eeFieldPatterns
  ) where

import qualified Network.AWS.CloudFront.Types.FieldPatterns as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Complex data type for field-level encryption profiles that includes the encryption key and field pattern specifications. 
--
-- /See:/ 'mkEncryptionEntity' smart constructor.
data EncryptionEntity = EncryptionEntity'
  { publicKeyId :: Core.Text
    -- ^ The public key associated with a set of field-level encryption patterns, to be used when encrypting the fields that match the patterns. 
  , providerId :: Core.Text
    -- ^ The provider associated with the public key being used for encryption. This value must also be provided with the private key for applications to be able to decrypt data.
  , fieldPatterns :: Types.FieldPatterns
    -- ^ Field patterns in a field-level encryption content type profile specify the fields that you want to be encrypted. You can provide the full field name, or any beginning characters followed by a wildcard (*). You can't overlap field patterns. For example, you can't have both ABC* and AB*. Note that field patterns are case-sensitive. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EncryptionEntity' value with any optional fields omitted.
mkEncryptionEntity
    :: Core.Text -- ^ 'publicKeyId'
    -> Core.Text -- ^ 'providerId'
    -> Types.FieldPatterns -- ^ 'fieldPatterns'
    -> EncryptionEntity
mkEncryptionEntity publicKeyId providerId fieldPatterns
  = EncryptionEntity'{publicKeyId, providerId, fieldPatterns}

-- | The public key associated with a set of field-level encryption patterns, to be used when encrypting the fields that match the patterns. 
--
-- /Note:/ Consider using 'publicKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eePublicKeyId :: Lens.Lens' EncryptionEntity Core.Text
eePublicKeyId = Lens.field @"publicKeyId"
{-# INLINEABLE eePublicKeyId #-}
{-# DEPRECATED publicKeyId "Use generic-lens or generic-optics with 'publicKeyId' instead"  #-}

-- | The provider associated with the public key being used for encryption. This value must also be provided with the private key for applications to be able to decrypt data.
--
-- /Note:/ Consider using 'providerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeProviderId :: Lens.Lens' EncryptionEntity Core.Text
eeProviderId = Lens.field @"providerId"
{-# INLINEABLE eeProviderId #-}
{-# DEPRECATED providerId "Use generic-lens or generic-optics with 'providerId' instead"  #-}

-- | Field patterns in a field-level encryption content type profile specify the fields that you want to be encrypted. You can provide the full field name, or any beginning characters followed by a wildcard (*). You can't overlap field patterns. For example, you can't have both ABC* and AB*. Note that field patterns are case-sensitive. 
--
-- /Note:/ Consider using 'fieldPatterns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
eeFieldPatterns :: Lens.Lens' EncryptionEntity Types.FieldPatterns
eeFieldPatterns = Lens.field @"fieldPatterns"
{-# INLINEABLE eeFieldPatterns #-}
{-# DEPRECATED fieldPatterns "Use generic-lens or generic-optics with 'fieldPatterns' instead"  #-}

instance Core.ToXML EncryptionEntity where
        toXML EncryptionEntity{..}
          = Core.toXMLElement "PublicKeyId" publicKeyId Core.<>
              Core.toXMLElement "ProviderId" providerId
              Core.<> Core.toXMLElement "FieldPatterns" fieldPatterns

instance Core.FromXML EncryptionEntity where
        parseXML x
          = EncryptionEntity' Core.<$>
              (x Core..@ "PublicKeyId") Core.<*> x Core..@ "ProviderId" Core.<*>
                x Core..@ "FieldPatterns"
