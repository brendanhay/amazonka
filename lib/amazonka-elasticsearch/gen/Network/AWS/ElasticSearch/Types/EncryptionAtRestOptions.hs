{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.EncryptionAtRestOptions
  ( EncryptionAtRestOptions (..),

    -- * Smart constructor
    mkEncryptionAtRestOptions,

    -- * Lenses
    earoEnabled,
    earoKmsKeyId,
  )
where

import qualified Network.AWS.ElasticSearch.Types.KmsKeyId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the Encryption At Rest Options.
--
-- /See:/ 'mkEncryptionAtRestOptions' smart constructor.
data EncryptionAtRestOptions = EncryptionAtRestOptions'
  { -- | Specifies the option to enable Encryption At Rest.
    enabled :: Core.Maybe Core.Bool,
    -- | Specifies the KMS Key ID for Encryption At Rest options.
    kmsKeyId :: Core.Maybe Types.KmsKeyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EncryptionAtRestOptions' value with any optional fields omitted.
mkEncryptionAtRestOptions ::
  EncryptionAtRestOptions
mkEncryptionAtRestOptions =
  EncryptionAtRestOptions'
    { enabled = Core.Nothing,
      kmsKeyId = Core.Nothing
    }

-- | Specifies the option to enable Encryption At Rest.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
earoEnabled :: Lens.Lens' EncryptionAtRestOptions (Core.Maybe Core.Bool)
earoEnabled = Lens.field @"enabled"
{-# DEPRECATED earoEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | Specifies the KMS Key ID for Encryption At Rest options.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
earoKmsKeyId :: Lens.Lens' EncryptionAtRestOptions (Core.Maybe Types.KmsKeyId)
earoKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED earoKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

instance Core.FromJSON EncryptionAtRestOptions where
  toJSON EncryptionAtRestOptions {..} =
    Core.object
      ( Core.catMaybes
          [ ("Enabled" Core..=) Core.<$> enabled,
            ("KmsKeyId" Core..=) Core.<$> kmsKeyId
          ]
      )

instance Core.FromJSON EncryptionAtRestOptions where
  parseJSON =
    Core.withObject "EncryptionAtRestOptions" Core.$
      \x ->
        EncryptionAtRestOptions'
          Core.<$> (x Core..:? "Enabled") Core.<*> (x Core..:? "KmsKeyId")
