{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Athena.Types.EncryptionConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Athena.Types.EncryptionConfiguration
  ( EncryptionConfiguration (..),

    -- * Smart constructor
    mkEncryptionConfiguration,

    -- * Lenses
    ecEncryptionOption,
    ecKmsKey,
  )
where

import qualified Network.AWS.Athena.Types.EncryptionOption as Types
import qualified Network.AWS.Athena.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | If query results are encrypted in Amazon S3, indicates the encryption option used (for example, @SSE-KMS@ or @CSE-KMS@ ) and key information.
--
-- /See:/ 'mkEncryptionConfiguration' smart constructor.
data EncryptionConfiguration = EncryptionConfiguration'
  { -- | Indicates whether Amazon S3 server-side encryption with Amazon S3-managed keys (@SSE-S3@ ), server-side encryption with KMS-managed keys (@SSE-KMS@ ), or client-side encryption with KMS-managed keys (CSE-KMS) is used.
    --
    -- If a query runs in a workgroup and the workgroup overrides client-side settings, then the workgroup's setting for encryption is used. It specifies whether query results must be encrypted, for all queries that run in this workgroup.
    encryptionOption :: Types.EncryptionOption,
    -- | For @SSE-KMS@ and @CSE-KMS@ , this is the KMS key ARN or ID.
    kmsKey :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'EncryptionConfiguration' value with any optional fields omitted.
mkEncryptionConfiguration ::
  -- | 'encryptionOption'
  Types.EncryptionOption ->
  EncryptionConfiguration
mkEncryptionConfiguration encryptionOption =
  EncryptionConfiguration' {encryptionOption, kmsKey = Core.Nothing}

-- | Indicates whether Amazon S3 server-side encryption with Amazon S3-managed keys (@SSE-S3@ ), server-side encryption with KMS-managed keys (@SSE-KMS@ ), or client-side encryption with KMS-managed keys (CSE-KMS) is used.
--
-- If a query runs in a workgroup and the workgroup overrides client-side settings, then the workgroup's setting for encryption is used. It specifies whether query results must be encrypted, for all queries that run in this workgroup.
--
-- /Note:/ Consider using 'encryptionOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecEncryptionOption :: Lens.Lens' EncryptionConfiguration Types.EncryptionOption
ecEncryptionOption = Lens.field @"encryptionOption"
{-# DEPRECATED ecEncryptionOption "Use generic-lens or generic-optics with 'encryptionOption' instead." #-}

-- | For @SSE-KMS@ and @CSE-KMS@ , this is the KMS key ARN or ID.
--
-- /Note:/ Consider using 'kmsKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ecKmsKey :: Lens.Lens' EncryptionConfiguration (Core.Maybe Types.String)
ecKmsKey = Lens.field @"kmsKey"
{-# DEPRECATED ecKmsKey "Use generic-lens or generic-optics with 'kmsKey' instead." #-}

instance Core.FromJSON EncryptionConfiguration where
  toJSON EncryptionConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("EncryptionOption" Core..= encryptionOption),
            ("KmsKey" Core..=) Core.<$> kmsKey
          ]
      )

instance Core.FromJSON EncryptionConfiguration where
  parseJSON =
    Core.withObject "EncryptionConfiguration" Core.$
      \x ->
        EncryptionConfiguration'
          Core.<$> (x Core..: "EncryptionOption") Core.<*> (x Core..:? "KmsKey")
