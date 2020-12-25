{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.EncryptionAtRest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.EncryptionAtRest
  ( EncryptionAtRest (..),

    -- * Smart constructor
    mkEncryptionAtRest,

    -- * Lenses
    earCatalogEncryptionMode,
    earSseAwsKmsKeyId,
  )
where

import qualified Network.AWS.Glue.Types.CatalogEncryptionMode as Types
import qualified Network.AWS.Glue.Types.SseAwsKmsKeyId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Specifies the encryption-at-rest configuration for the Data Catalog.
--
-- /See:/ 'mkEncryptionAtRest' smart constructor.
data EncryptionAtRest = EncryptionAtRest'
  { -- | The encryption-at-rest mode for encrypting Data Catalog data.
    catalogEncryptionMode :: Types.CatalogEncryptionMode,
    -- | The ID of the AWS KMS key to use for encryption at rest.
    sseAwsKmsKeyId :: Core.Maybe Types.SseAwsKmsKeyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EncryptionAtRest' value with any optional fields omitted.
mkEncryptionAtRest ::
  -- | 'catalogEncryptionMode'
  Types.CatalogEncryptionMode ->
  EncryptionAtRest
mkEncryptionAtRest catalogEncryptionMode =
  EncryptionAtRest'
    { catalogEncryptionMode,
      sseAwsKmsKeyId = Core.Nothing
    }

-- | The encryption-at-rest mode for encrypting Data Catalog data.
--
-- /Note:/ Consider using 'catalogEncryptionMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
earCatalogEncryptionMode :: Lens.Lens' EncryptionAtRest Types.CatalogEncryptionMode
earCatalogEncryptionMode = Lens.field @"catalogEncryptionMode"
{-# DEPRECATED earCatalogEncryptionMode "Use generic-lens or generic-optics with 'catalogEncryptionMode' instead." #-}

-- | The ID of the AWS KMS key to use for encryption at rest.
--
-- /Note:/ Consider using 'sseAwsKmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
earSseAwsKmsKeyId :: Lens.Lens' EncryptionAtRest (Core.Maybe Types.SseAwsKmsKeyId)
earSseAwsKmsKeyId = Lens.field @"sseAwsKmsKeyId"
{-# DEPRECATED earSseAwsKmsKeyId "Use generic-lens or generic-optics with 'sseAwsKmsKeyId' instead." #-}

instance Core.FromJSON EncryptionAtRest where
  toJSON EncryptionAtRest {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("CatalogEncryptionMode" Core..= catalogEncryptionMode),
            ("SseAwsKmsKeyId" Core..=) Core.<$> sseAwsKmsKeyId
          ]
      )

instance Core.FromJSON EncryptionAtRest where
  parseJSON =
    Core.withObject "EncryptionAtRest" Core.$
      \x ->
        EncryptionAtRest'
          Core.<$> (x Core..: "CatalogEncryptionMode")
          Core.<*> (x Core..:? "SseAwsKmsKeyId")
