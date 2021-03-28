{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInputUpdate
  ( KinesisFirehoseInputUpdate (..)
  -- * Smart constructor
  , mkKinesisFirehoseInputUpdate
  -- * Lenses
  , kfiuResourceARNUpdate
  , kfiuRoleARNUpdate
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.ResourceARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARNUpdate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | When updating application input configuration, provides information about an Amazon Kinesis Firehose delivery stream as the streaming source.
--
-- /See:/ 'mkKinesisFirehoseInputUpdate' smart constructor.
data KinesisFirehoseInputUpdate = KinesisFirehoseInputUpdate'
  { resourceARNUpdate :: Core.Maybe Types.ResourceARN
    -- ^ Amazon Resource Name (ARN) of the input Amazon Kinesis Firehose delivery stream to read.
  , roleARNUpdate :: Core.Maybe Types.RoleARNUpdate
    -- ^ ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisFirehoseInputUpdate' value with any optional fields omitted.
mkKinesisFirehoseInputUpdate
    :: KinesisFirehoseInputUpdate
mkKinesisFirehoseInputUpdate
  = KinesisFirehoseInputUpdate'{resourceARNUpdate = Core.Nothing,
                                roleARNUpdate = Core.Nothing}

-- | Amazon Resource Name (ARN) of the input Amazon Kinesis Firehose delivery stream to read.
--
-- /Note:/ Consider using 'resourceARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfiuResourceARNUpdate :: Lens.Lens' KinesisFirehoseInputUpdate (Core.Maybe Types.ResourceARN)
kfiuResourceARNUpdate = Lens.field @"resourceARNUpdate"
{-# INLINEABLE kfiuResourceARNUpdate #-}
{-# DEPRECATED resourceARNUpdate "Use generic-lens or generic-optics with 'resourceARNUpdate' instead"  #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
--
-- /Note:/ Consider using 'roleARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfiuRoleARNUpdate :: Lens.Lens' KinesisFirehoseInputUpdate (Core.Maybe Types.RoleARNUpdate)
kfiuRoleARNUpdate = Lens.field @"roleARNUpdate"
{-# INLINEABLE kfiuRoleARNUpdate #-}
{-# DEPRECATED roleARNUpdate "Use generic-lens or generic-optics with 'roleARNUpdate' instead"  #-}

instance Core.FromJSON KinesisFirehoseInputUpdate where
        toJSON KinesisFirehoseInputUpdate{..}
          = Core.object
              (Core.catMaybes
                 [("ResourceARNUpdate" Core..=) Core.<$> resourceARNUpdate,
                  ("RoleARNUpdate" Core..=) Core.<$> roleARNUpdate])
