{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseOutputUpdate
  ( KinesisFirehoseOutputUpdate (..)
  -- * Smart constructor
  , mkKinesisFirehoseOutputUpdate
  -- * Lenses
  , kfouResourceARNUpdate
  , kfouRoleARNUpdate
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.ResourceARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARNUpdate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | When updating an output configuration using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_UpdateApplication.html UpdateApplication> operation, provides information about an Amazon Kinesis Firehose delivery stream configured as the destination. 
--
-- /See:/ 'mkKinesisFirehoseOutputUpdate' smart constructor.
data KinesisFirehoseOutputUpdate = KinesisFirehoseOutputUpdate'
  { resourceARNUpdate :: Core.Maybe Types.ResourceARN
    -- ^ Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream to write to.
  , roleARNUpdate :: Core.Maybe Types.RoleARNUpdate
    -- ^ ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisFirehoseOutputUpdate' value with any optional fields omitted.
mkKinesisFirehoseOutputUpdate
    :: KinesisFirehoseOutputUpdate
mkKinesisFirehoseOutputUpdate
  = KinesisFirehoseOutputUpdate'{resourceARNUpdate = Core.Nothing,
                                 roleARNUpdate = Core.Nothing}

-- | Amazon Resource Name (ARN) of the Amazon Kinesis Firehose delivery stream to write to.
--
-- /Note:/ Consider using 'resourceARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfouResourceARNUpdate :: Lens.Lens' KinesisFirehoseOutputUpdate (Core.Maybe Types.ResourceARN)
kfouResourceARNUpdate = Lens.field @"resourceARNUpdate"
{-# INLINEABLE kfouResourceARNUpdate #-}
{-# DEPRECATED resourceARNUpdate "Use generic-lens or generic-optics with 'resourceARNUpdate' instead"  #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
--
-- /Note:/ Consider using 'roleARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfouRoleARNUpdate :: Lens.Lens' KinesisFirehoseOutputUpdate (Core.Maybe Types.RoleARNUpdate)
kfouRoleARNUpdate = Lens.field @"roleARNUpdate"
{-# INLINEABLE kfouRoleARNUpdate #-}
{-# DEPRECATED roleARNUpdate "Use generic-lens or generic-optics with 'roleARNUpdate' instead"  #-}

instance Core.FromJSON KinesisFirehoseOutputUpdate where
        toJSON KinesisFirehoseOutputUpdate{..}
          = Core.object
              (Core.catMaybes
                 [("ResourceARNUpdate" Core..=) Core.<$> resourceARNUpdate,
                  ("RoleARNUpdate" Core..=) Core.<$> roleARNUpdate])
