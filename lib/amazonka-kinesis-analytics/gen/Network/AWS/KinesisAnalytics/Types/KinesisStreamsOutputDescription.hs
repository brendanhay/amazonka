{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsOutputDescription
  ( KinesisStreamsOutputDescription (..)
  -- * Smart constructor
  , mkKinesisStreamsOutputDescription
  -- * Lenses
  , ksodResourceARN
  , ksodRoleARN
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.ResourceARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | For an application output, describes the Amazon Kinesis stream configured as its destination. 
--
-- /See:/ 'mkKinesisStreamsOutputDescription' smart constructor.
data KinesisStreamsOutputDescription = KinesisStreamsOutputDescription'
  { resourceARN :: Core.Maybe Types.ResourceARN
    -- ^ Amazon Resource Name (ARN) of the Amazon Kinesis stream.
  , roleARN :: Core.Maybe Types.RoleARN
    -- ^ ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisStreamsOutputDescription' value with any optional fields omitted.
mkKinesisStreamsOutputDescription
    :: KinesisStreamsOutputDescription
mkKinesisStreamsOutputDescription
  = KinesisStreamsOutputDescription'{resourceARN = Core.Nothing,
                                     roleARN = Core.Nothing}

-- | Amazon Resource Name (ARN) of the Amazon Kinesis stream.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksodResourceARN :: Lens.Lens' KinesisStreamsOutputDescription (Core.Maybe Types.ResourceARN)
ksodResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE ksodResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksodRoleARN :: Lens.Lens' KinesisStreamsOutputDescription (Core.Maybe Types.RoleARN)
ksodRoleARN = Lens.field @"roleARN"
{-# INLINEABLE ksodRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

instance Core.FromJSON KinesisStreamsOutputDescription where
        parseJSON
          = Core.withObject "KinesisStreamsOutputDescription" Core.$
              \ x ->
                KinesisStreamsOutputDescription' Core.<$>
                  (x Core..:? "ResourceARN") Core.<*> x Core..:? "RoleARN"
