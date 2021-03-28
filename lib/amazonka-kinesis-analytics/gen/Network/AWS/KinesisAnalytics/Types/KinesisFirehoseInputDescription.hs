{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputDescription
  ( KinesisFirehoseInputDescription (..)
  -- * Smart constructor
  , mkKinesisFirehoseInputDescription
  -- * Lenses
  , kfidResourceARN
  , kfidRoleARN
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.ResourceARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes the Amazon Kinesis Firehose delivery stream that is configured as the streaming source in the application input configuration. 
--
-- /See:/ 'mkKinesisFirehoseInputDescription' smart constructor.
data KinesisFirehoseInputDescription = KinesisFirehoseInputDescription'
  { resourceARN :: Core.Maybe Types.ResourceARN
    -- ^ Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream.
  , roleARN :: Core.Maybe Types.RoleARN
    -- ^ ARN of the IAM role that Amazon Kinesis Analytics assumes to access the stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisFirehoseInputDescription' value with any optional fields omitted.
mkKinesisFirehoseInputDescription
    :: KinesisFirehoseInputDescription
mkKinesisFirehoseInputDescription
  = KinesisFirehoseInputDescription'{resourceARN = Core.Nothing,
                                     roleARN = Core.Nothing}

-- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfidResourceARN :: Lens.Lens' KinesisFirehoseInputDescription (Core.Maybe Types.ResourceARN)
kfidResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE kfidResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics assumes to access the stream.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfidRoleARN :: Lens.Lens' KinesisFirehoseInputDescription (Core.Maybe Types.RoleARN)
kfidRoleARN = Lens.field @"roleARN"
{-# INLINEABLE kfidRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

instance Core.FromJSON KinesisFirehoseInputDescription where
        parseJSON
          = Core.withObject "KinesisFirehoseInputDescription" Core.$
              \ x ->
                KinesisFirehoseInputDescription' Core.<$>
                  (x Core..:? "ResourceARN") Core.<*> x Core..:? "RoleARN"
