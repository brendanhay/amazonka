{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Firehose.Types.KMSEncryptionConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Firehose.Types.KMSEncryptionConfig
  ( KMSEncryptionConfig (..),

    -- * Smart constructor
    mkKMSEncryptionConfig,

    -- * Lenses
    kmsecAWSKMSKeyARN,
  )
where

import qualified Network.AWS.Firehose.Types.AWSKMSKeyARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an encryption key for a destination in Amazon S3.
--
-- /See:/ 'mkKMSEncryptionConfig' smart constructor.
newtype KMSEncryptionConfig = KMSEncryptionConfig'
  { -- | The Amazon Resource Name (ARN) of the encryption key. Must belong to the same AWS Region as the destination Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
    aWSKMSKeyARN :: Types.AWSKMSKeyARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'KMSEncryptionConfig' value with any optional fields omitted.
mkKMSEncryptionConfig ::
  -- | 'aWSKMSKeyARN'
  Types.AWSKMSKeyARN ->
  KMSEncryptionConfig
mkKMSEncryptionConfig aWSKMSKeyARN =
  KMSEncryptionConfig' {aWSKMSKeyARN}

-- | The Amazon Resource Name (ARN) of the encryption key. Must belong to the same AWS Region as the destination Amazon S3 bucket. For more information, see <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html Amazon Resource Names (ARNs) and AWS Service Namespaces> .
--
-- /Note:/ Consider using 'aWSKMSKeyARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kmsecAWSKMSKeyARN :: Lens.Lens' KMSEncryptionConfig Types.AWSKMSKeyARN
kmsecAWSKMSKeyARN = Lens.field @"aWSKMSKeyARN"
{-# DEPRECATED kmsecAWSKMSKeyARN "Use generic-lens or generic-optics with 'aWSKMSKeyARN' instead." #-}

instance Core.FromJSON KMSEncryptionConfig where
  toJSON KMSEncryptionConfig {..} =
    Core.object
      (Core.catMaybes [Core.Just ("AWSKMSKeyARN" Core..= aWSKMSKeyARN)])

instance Core.FromJSON KMSEncryptionConfig where
  parseJSON =
    Core.withObject "KMSEncryptionConfig" Core.$
      \x -> KMSEncryptionConfig' Core.<$> (x Core..: "AWSKMSKeyARN")
