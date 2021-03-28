{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.KinesisFirehoseInput
  ( KinesisFirehoseInput (..)
  -- * Smart constructor
  , mkKinesisFirehoseInput
  -- * Lenses
  , kfiResourceARN
  , kfiRoleARN
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.ResourceARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Identifies an Amazon Kinesis Firehose delivery stream as the streaming source. You provide the delivery stream's Amazon Resource Name (ARN) and an IAM role ARN that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- /See:/ 'mkKinesisFirehoseInput' smart constructor.
data KinesisFirehoseInput = KinesisFirehoseInput'
  { resourceARN :: Types.ResourceARN
    -- ^ ARN of the input delivery stream.
  , roleARN :: Types.RoleARN
    -- ^ ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to make sure that the role has the necessary permissions to access the stream.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisFirehoseInput' value with any optional fields omitted.
mkKinesisFirehoseInput
    :: Types.ResourceARN -- ^ 'resourceARN'
    -> Types.RoleARN -- ^ 'roleARN'
    -> KinesisFirehoseInput
mkKinesisFirehoseInput resourceARN roleARN
  = KinesisFirehoseInput'{resourceARN, roleARN}

-- | ARN of the input delivery stream.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfiResourceARN :: Lens.Lens' KinesisFirehoseInput Types.ResourceARN
kfiResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE kfiResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to make sure that the role has the necessary permissions to access the stream.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kfiRoleARN :: Lens.Lens' KinesisFirehoseInput Types.RoleARN
kfiRoleARN = Lens.field @"roleARN"
{-# INLINEABLE kfiRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

instance Core.FromJSON KinesisFirehoseInput where
        toJSON KinesisFirehoseInput{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceARN" Core..= resourceARN),
                  Core.Just ("RoleARN" Core..= roleARN)])
