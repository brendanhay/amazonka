{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Comprehend.Types.PiiOutputDataConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Comprehend.Types.PiiOutputDataConfig
  ( PiiOutputDataConfig (..),

    -- * Smart constructor
    mkPiiOutputDataConfig,

    -- * Lenses
    podcS3Uri,
    podcKmsKeyId,
  )
where

import qualified Network.AWS.Comprehend.Types.KmsKeyId as Types
import qualified Network.AWS.Comprehend.Types.S3Uri as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Provides configuration parameters for the output of PII entity detection jobs.
--
-- /See:/ 'mkPiiOutputDataConfig' smart constructor.
data PiiOutputDataConfig = PiiOutputDataConfig'
  { -- | When you use the @PiiOutputDataConfig@ object with asynchronous operations, you specify the Amazon S3 location where you want to write the output data.
    s3Uri :: Types.S3Uri,
    -- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt the output results from an analysis job.
    kmsKeyId :: Core.Maybe Types.KmsKeyId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PiiOutputDataConfig' value with any optional fields omitted.
mkPiiOutputDataConfig ::
  -- | 's3Uri'
  Types.S3Uri ->
  PiiOutputDataConfig
mkPiiOutputDataConfig s3Uri =
  PiiOutputDataConfig' {s3Uri, kmsKeyId = Core.Nothing}

-- | When you use the @PiiOutputDataConfig@ object with asynchronous operations, you specify the Amazon S3 location where you want to write the output data.
--
-- /Note:/ Consider using 's3Uri' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
podcS3Uri :: Lens.Lens' PiiOutputDataConfig Types.S3Uri
podcS3Uri = Lens.field @"s3Uri"
{-# DEPRECATED podcS3Uri "Use generic-lens or generic-optics with 's3Uri' instead." #-}

-- | ID for the AWS Key Management Service (KMS) key that Amazon Comprehend uses to encrypt the output results from an analysis job.
--
-- /Note:/ Consider using 'kmsKeyId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
podcKmsKeyId :: Lens.Lens' PiiOutputDataConfig (Core.Maybe Types.KmsKeyId)
podcKmsKeyId = Lens.field @"kmsKeyId"
{-# DEPRECATED podcKmsKeyId "Use generic-lens or generic-optics with 'kmsKeyId' instead." #-}

instance Core.FromJSON PiiOutputDataConfig where
  parseJSON =
    Core.withObject "PiiOutputDataConfig" Core.$
      \x ->
        PiiOutputDataConfig'
          Core.<$> (x Core..: "S3Uri") Core.<*> (x Core..:? "KmsKeyId")
