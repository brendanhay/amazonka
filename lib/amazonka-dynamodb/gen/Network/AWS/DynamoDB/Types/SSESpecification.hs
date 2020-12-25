{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DynamoDB.Types.SSESpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DynamoDB.Types.SSESpecification
  ( SSESpecification (..),

    -- * Smart constructor
    mkSSESpecification,

    -- * Lenses
    ssesEnabled,
    ssesKMSMasterKeyId,
    ssesSSEType,
  )
where

import qualified Network.AWS.DynamoDB.Types.KMSMasterKeyId as Types
import qualified Network.AWS.DynamoDB.Types.SSEType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the settings used to enable server-side encryption.
--
-- /See:/ 'mkSSESpecification' smart constructor.
data SSESpecification = SSESpecification'
  { -- | Indicates whether server-side encryption is done using an AWS managed CMK or an AWS owned CMK. If enabled (true), server-side encryption type is set to @KMS@ and an AWS managed CMK is used (AWS KMS charges apply). If disabled (false) or not specified, server-side encryption is set to AWS owned CMK.
    enabled :: Core.Maybe Core.Bool,
    -- | The AWS KMS customer master key (CMK) that should be used for the AWS KMS encryption. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. Note that you should only provide this parameter if the key is different from the default DynamoDB customer master key alias/aws/dynamodb.
    kMSMasterKeyId :: Core.Maybe Types.KMSMasterKeyId,
    -- | Server-side encryption type. The only supported value is:
    --
    --
    --     * @KMS@ - Server-side encryption that uses AWS Key Management Service. The key is stored in your account and is managed by AWS KMS (AWS KMS charges apply).
    sSEType :: Core.Maybe Types.SSEType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SSESpecification' value with any optional fields omitted.
mkSSESpecification ::
  SSESpecification
mkSSESpecification =
  SSESpecification'
    { enabled = Core.Nothing,
      kMSMasterKeyId = Core.Nothing,
      sSEType = Core.Nothing
    }

-- | Indicates whether server-side encryption is done using an AWS managed CMK or an AWS owned CMK. If enabled (true), server-side encryption type is set to @KMS@ and an AWS managed CMK is used (AWS KMS charges apply). If disabled (false) or not specified, server-side encryption is set to AWS owned CMK.
--
-- /Note:/ Consider using 'enabled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssesEnabled :: Lens.Lens' SSESpecification (Core.Maybe Core.Bool)
ssesEnabled = Lens.field @"enabled"
{-# DEPRECATED ssesEnabled "Use generic-lens or generic-optics with 'enabled' instead." #-}

-- | The AWS KMS customer master key (CMK) that should be used for the AWS KMS encryption. To specify a CMK, use its key ID, Amazon Resource Name (ARN), alias name, or alias ARN. Note that you should only provide this parameter if the key is different from the default DynamoDB customer master key alias/aws/dynamodb.
--
-- /Note:/ Consider using 'kMSMasterKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssesKMSMasterKeyId :: Lens.Lens' SSESpecification (Core.Maybe Types.KMSMasterKeyId)
ssesKMSMasterKeyId = Lens.field @"kMSMasterKeyId"
{-# DEPRECATED ssesKMSMasterKeyId "Use generic-lens or generic-optics with 'kMSMasterKeyId' instead." #-}

-- | Server-side encryption type. The only supported value is:
--
--
--     * @KMS@ - Server-side encryption that uses AWS Key Management Service. The key is stored in your account and is managed by AWS KMS (AWS KMS charges apply).
--
--
--
-- /Note:/ Consider using 'sSEType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ssesSSEType :: Lens.Lens' SSESpecification (Core.Maybe Types.SSEType)
ssesSSEType = Lens.field @"sSEType"
{-# DEPRECATED ssesSSEType "Use generic-lens or generic-optics with 'sSEType' instead." #-}

instance Core.FromJSON SSESpecification where
  toJSON SSESpecification {..} =
    Core.object
      ( Core.catMaybes
          [ ("Enabled" Core..=) Core.<$> enabled,
            ("KMSMasterKeyId" Core..=) Core.<$> kMSMasterKeyId,
            ("SSEType" Core..=) Core.<$> sSEType
          ]
      )
