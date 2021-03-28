{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsInputDescription
  ( KinesisStreamsInputDescription (..)
  -- * Smart constructor
  , mkKinesisStreamsInputDescription
  -- * Lenses
  , ksidResourceARN
  , ksidRoleARN
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.ResourceARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Amazon Kinesis stream that is configured as the streaming source in the application input configuration. 
--
-- /See:/ 'mkKinesisStreamsInputDescription' smart constructor.
data KinesisStreamsInputDescription = KinesisStreamsInputDescription'
  { resourceARN :: Core.Maybe Types.ResourceARN
    -- ^ Amazon Resource Name (ARN) of the Amazon Kinesis stream.
  , roleARN :: Core.Maybe Types.RoleARN
    -- ^ ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisStreamsInputDescription' value with any optional fields omitted.
mkKinesisStreamsInputDescription
    :: KinesisStreamsInputDescription
mkKinesisStreamsInputDescription
  = KinesisStreamsInputDescription'{resourceARN = Core.Nothing,
                                    roleARN = Core.Nothing}

-- | Amazon Resource Name (ARN) of the Amazon Kinesis stream.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksidResourceARN :: Lens.Lens' KinesisStreamsInputDescription (Core.Maybe Types.ResourceARN)
ksidResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE ksidResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksidRoleARN :: Lens.Lens' KinesisStreamsInputDescription (Core.Maybe Types.RoleARN)
ksidRoleARN = Lens.field @"roleARN"
{-# INLINEABLE ksidRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

instance Core.FromJSON KinesisStreamsInputDescription where
        parseJSON
          = Core.withObject "KinesisStreamsInputDescription" Core.$
              \ x ->
                KinesisStreamsInputDescription' Core.<$>
                  (x Core..:? "ResourceARN") Core.<*> x Core..:? "RoleARN"
