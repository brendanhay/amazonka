{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Translate.Types.EncryptionKey
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Translate.Types.EncryptionKey
  ( EncryptionKey (..),

    -- * Smart constructor
    mkEncryptionKey,

    -- * Lenses
    ekType,
    ekId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Translate.Types.EncryptionKeyID as Types
import qualified Network.AWS.Translate.Types.EncryptionKeyType as Types

-- | The encryption key used to encrypt this object.
--
-- /See:/ 'mkEncryptionKey' smart constructor.
data EncryptionKey = EncryptionKey'
  { -- | The type of encryption key used by Amazon Translate to encrypt custom terminologies.
    type' :: Types.EncryptionKeyType,
    -- | The Amazon Resource Name (ARN) of the encryption key being used to encrypt the custom terminology.
    id :: Types.EncryptionKeyID
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EncryptionKey' value with any optional fields omitted.
mkEncryptionKey ::
  -- | 'type\''
  Types.EncryptionKeyType ->
  -- | 'id'
  Types.EncryptionKeyID ->
  EncryptionKey
mkEncryptionKey type' id = EncryptionKey' {type', id}

-- | The type of encryption key used by Amazon Translate to encrypt custom terminologies.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ekType :: Lens.Lens' EncryptionKey Types.EncryptionKeyType
ekType = Lens.field @"type'"
{-# DEPRECATED ekType "Use generic-lens or generic-optics with 'type'' instead." #-}

-- | The Amazon Resource Name (ARN) of the encryption key being used to encrypt the custom terminology.
--
-- /Note:/ Consider using 'id' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ekId :: Lens.Lens' EncryptionKey Types.EncryptionKeyID
ekId = Lens.field @"id"
{-# DEPRECATED ekId "Use generic-lens or generic-optics with 'id' instead." #-}

instance Core.FromJSON EncryptionKey where
  toJSON EncryptionKey {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("Type" Core..= type'), Core.Just ("Id" Core..= id)]
      )

instance Core.FromJSON EncryptionKey where
  parseJSON =
    Core.withObject "EncryptionKey" Core.$
      \x ->
        EncryptionKey'
          Core.<$> (x Core..: "Type") Core.<*> (x Core..: "Id")
