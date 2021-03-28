{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ConnectionPasswordEncryption
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.ConnectionPasswordEncryption
  ( ConnectionPasswordEncryption (..)
  -- * Smart constructor
  , mkConnectionPasswordEncryption
  -- * Lenses
  , cpeReturnConnectionPasswordEncrypted
  , cpeAwsKmsKeyId
  ) where

import qualified Network.AWS.Glue.Types.AwsKmsKeyId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The data structure used by the Data Catalog to encrypt the password as part of @CreateConnection@ or @UpdateConnection@ and store it in the @ENCRYPTED_PASSWORD@ field in the connection properties. You can enable catalog encryption or only password encryption.
--
-- When a @CreationConnection@ request arrives containing a password, the Data Catalog first encrypts the password using your AWS KMS key. It then encrypts the whole connection object again if catalog encryption is also enabled.
-- This encryption requires that you set AWS KMS key permissions to enable or restrict access on the password key according to your security requirements. For example, you might want only administrators to have decrypt permission on the password key.
--
-- /See:/ 'mkConnectionPasswordEncryption' smart constructor.
data ConnectionPasswordEncryption = ConnectionPasswordEncryption'
  { returnConnectionPasswordEncrypted :: Core.Bool
    -- ^ When the @ReturnConnectionPasswordEncrypted@ flag is set to "true", passwords remain encrypted in the responses of @GetConnection@ and @GetConnections@ . This encryption takes effect independently from catalog encryption. 
  , awsKmsKeyId :: Core.Maybe Types.AwsKmsKeyId
    -- ^ An AWS KMS key that is used to encrypt the connection password. 
--
-- If connection password protection is enabled, the caller of @CreateConnection@ and @UpdateConnection@ needs at least @kms:Encrypt@ permission on the specified AWS KMS key, to encrypt passwords before storing them in the Data Catalog. 
-- You can set the decrypt permission to enable or restrict access on the password key according to your security requirements.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ConnectionPasswordEncryption' value with any optional fields omitted.
mkConnectionPasswordEncryption
    :: Core.Bool -- ^ 'returnConnectionPasswordEncrypted'
    -> ConnectionPasswordEncryption
mkConnectionPasswordEncryption returnConnectionPasswordEncrypted
  = ConnectionPasswordEncryption'{returnConnectionPasswordEncrypted,
                                  awsKmsKeyId = Core.Nothing}

-- | When the @ReturnConnectionPasswordEncrypted@ flag is set to "true", passwords remain encrypted in the responses of @GetConnection@ and @GetConnections@ . This encryption takes effect independently from catalog encryption. 
--
-- /Note:/ Consider using 'returnConnectionPasswordEncrypted' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpeReturnConnectionPasswordEncrypted :: Lens.Lens' ConnectionPasswordEncryption Core.Bool
cpeReturnConnectionPasswordEncrypted = Lens.field @"returnConnectionPasswordEncrypted"
{-# INLINEABLE cpeReturnConnectionPasswordEncrypted #-}
{-# DEPRECATED returnConnectionPasswordEncrypted "Use generic-lens or generic-optics with 'returnConnectionPasswordEncrypted' instead"  #-}

-- | An AWS KMS key that is used to encrypt the connection password. 
--
-- If connection password protection is enabled, the caller of @CreateConnection@ and @UpdateConnection@ needs at least @kms:Encrypt@ permission on the specified AWS KMS key, to encrypt passwords before storing them in the Data Catalog. 
-- You can set the decrypt permission to enable or restrict access on the password key according to your security requirements.
--
-- /Note:/ Consider using 'awsKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpeAwsKmsKeyId :: Lens.Lens' ConnectionPasswordEncryption (Core.Maybe Types.AwsKmsKeyId)
cpeAwsKmsKeyId = Lens.field @"awsKmsKeyId"
{-# INLINEABLE cpeAwsKmsKeyId #-}
{-# DEPRECATED awsKmsKeyId "Use generic-lens or generic-optics with 'awsKmsKeyId' instead"  #-}

instance Core.FromJSON ConnectionPasswordEncryption where
        toJSON ConnectionPasswordEncryption{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just
                    ("ReturnConnectionPasswordEncrypted" Core..=
                       returnConnectionPasswordEncrypted),
                  ("AwsKmsKeyId" Core..=) Core.<$> awsKmsKeyId])

instance Core.FromJSON ConnectionPasswordEncryption where
        parseJSON
          = Core.withObject "ConnectionPasswordEncryption" Core.$
              \ x ->
                ConnectionPasswordEncryption' Core.<$>
                  (x Core..: "ReturnConnectionPasswordEncrypted") Core.<*>
                    x Core..:? "AwsKmsKeyId"
