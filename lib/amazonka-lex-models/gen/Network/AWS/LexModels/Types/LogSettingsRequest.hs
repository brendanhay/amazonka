{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.LexModels.Types.LogSettingsRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.LexModels.Types.LogSettingsRequest
  ( LogSettingsRequest (..)
  -- * Smart constructor
  , mkLogSettingsRequest
  -- * Lenses
  , lLogType
  , lDestination
  , lResourceArn
  , lKmsKeyArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.LexModels.Types.Destination as Types
import qualified Network.AWS.LexModels.Types.KmsKeyArn as Types
import qualified Network.AWS.LexModels.Types.LogType as Types
import qualified Network.AWS.LexModels.Types.ResourceArn as Types
import qualified Network.AWS.Prelude as Core

-- | Settings used to configure delivery mode and destination for conversation logs.
--
-- /See:/ 'mkLogSettingsRequest' smart constructor.
data LogSettingsRequest = LogSettingsRequest'
  { logType :: Types.LogType
    -- ^ The type of logging to enable. Text logs are delivered to a CloudWatch Logs log group. Audio logs are delivered to an S3 bucket.
  , destination :: Types.Destination
    -- ^ Where the logs will be delivered. Text logs are delivered to a CloudWatch Logs log group. Audio logs are delivered to an S3 bucket.
  , resourceArn :: Types.ResourceArn
    -- ^ The Amazon Resource Name (ARN) of the CloudWatch Logs log group or S3 bucket where the logs should be delivered.
  , kmsKeyArn :: Core.Maybe Types.KmsKeyArn
    -- ^ The Amazon Resource Name (ARN) of the AWS KMS customer managed key for encrypting audio logs delivered to an S3 bucket. The key does not apply to CloudWatch Logs and is optional for S3 buckets.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LogSettingsRequest' value with any optional fields omitted.
mkLogSettingsRequest
    :: Types.LogType -- ^ 'logType'
    -> Types.Destination -- ^ 'destination'
    -> Types.ResourceArn -- ^ 'resourceArn'
    -> LogSettingsRequest
mkLogSettingsRequest logType destination resourceArn
  = LogSettingsRequest'{logType, destination, resourceArn,
                        kmsKeyArn = Core.Nothing}

-- | The type of logging to enable. Text logs are delivered to a CloudWatch Logs log group. Audio logs are delivered to an S3 bucket.
--
-- /Note:/ Consider using 'logType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lLogType :: Lens.Lens' LogSettingsRequest Types.LogType
lLogType = Lens.field @"logType"
{-# INLINEABLE lLogType #-}
{-# DEPRECATED logType "Use generic-lens or generic-optics with 'logType' instead"  #-}

-- | Where the logs will be delivered. Text logs are delivered to a CloudWatch Logs log group. Audio logs are delivered to an S3 bucket.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lDestination :: Lens.Lens' LogSettingsRequest Types.Destination
lDestination = Lens.field @"destination"
{-# INLINEABLE lDestination #-}
{-# DEPRECATED destination "Use generic-lens or generic-optics with 'destination' instead"  #-}

-- | The Amazon Resource Name (ARN) of the CloudWatch Logs log group or S3 bucket where the logs should be delivered.
--
-- /Note:/ Consider using 'resourceArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lResourceArn :: Lens.Lens' LogSettingsRequest Types.ResourceArn
lResourceArn = Lens.field @"resourceArn"
{-# INLINEABLE lResourceArn #-}
{-# DEPRECATED resourceArn "Use generic-lens or generic-optics with 'resourceArn' instead"  #-}

-- | The Amazon Resource Name (ARN) of the AWS KMS customer managed key for encrypting audio logs delivered to an S3 bucket. The key does not apply to CloudWatch Logs and is optional for S3 buckets.
--
-- /Note:/ Consider using 'kmsKeyArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lKmsKeyArn :: Lens.Lens' LogSettingsRequest (Core.Maybe Types.KmsKeyArn)
lKmsKeyArn = Lens.field @"kmsKeyArn"
{-# INLINEABLE lKmsKeyArn #-}
{-# DEPRECATED kmsKeyArn "Use generic-lens or generic-optics with 'kmsKeyArn' instead"  #-}

instance Core.FromJSON LogSettingsRequest where
        toJSON LogSettingsRequest{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("logType" Core..= logType),
                  Core.Just ("destination" Core..= destination),
                  Core.Just ("resourceArn" Core..= resourceArn),
                  ("kmsKeyArn" Core..=) Core.<$> kmsKeyArn])
