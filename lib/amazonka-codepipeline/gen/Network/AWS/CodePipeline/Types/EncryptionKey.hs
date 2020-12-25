{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CodePipeline.Types.EncryptionKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.EncryptionKey
  ( EncryptionKey (..),

    -- * Smart constructor
    mkEncryptionKey,

    -- * Lenses
    ekId,
    ekType,
  )
where

import qualified Network.AWS.CodePipeline.Types.EncryptionKeyId as Types
import qualified Network.AWS.CodePipeline.Types.EncryptionKeyType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents information about the key used to encrypt data in the artifact store, such as an AWS Key Management Service (AWS KMS) key.
--
-- /See:/ 'mkEncryptionKey' smart constructor.
data EncryptionKey = EncryptionKey'
  { -- | The ID used to identify the key. For an AWS KMS key, you can use the key ID, the key ARN, or the alias ARN.
    id :: Types.EncryptionKeyId,
    -- | The type of encryption key, such as an AWS Key Management Service (AWS KMS) key. When creating or updating a pipeline, the value must be set to 'KMS'.
    type' :: Types.EncryptionKeyType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EncryptionKey' value with any optional fields omitted.
mkEncryptionKey ::
  -- | 'id'
  Types.EncryptionKeyId ->
  -- | 'type\''
  Types.EncryptionKeyType ->
  EncryptionKey
mkEncryptionKey id type' = EncryptionKey' {id, type'}

-- | The ID used to identify the key. For an AWS KMS key, you can use the key ID, the key ARN, or the alias ARN.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ekId :: Lens.Lens' EncryptionKey Types.EncryptionKeyId
ekId = Lens.field @"id"
{-# DEPRECATED ekId "Use generic-lens or generic-optics with 'id' instead." #-}

-- | The type of encryption key, such as an AWS Key Management Service (AWS KMS) key. When creating or updating a pipeline, the value must be set to 'KMS'.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ekType :: Lens.Lens' EncryptionKey Types.EncryptionKeyType
ekType = Lens.field @"type'"
{-# DEPRECATED ekType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Core.FromJSON EncryptionKey where
  toJSON EncryptionKey {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("id" Core..= id), Core.Just ("type" Core..= type')]
      )

instance Core.FromJSON EncryptionKey where
  parseJSON =
    Core.withObject "EncryptionKey" Core.$
      \x ->
        EncryptionKey'
          Core.<$> (x Core..: "id") Core.<*> (x Core..: "type")
