{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.KinesisStreamsInput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.KinesisStreamsInput
  ( KinesisStreamsInput (..)
  -- * Smart constructor
  , mkKinesisStreamsInput
  -- * Lenses
  , ksiResourceARN
  , ksiRoleARN
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.ResourceARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Identifies an Amazon Kinesis stream as the streaming source. You provide the stream's Amazon Resource Name (ARN) and an IAM role ARN that enables Amazon Kinesis Analytics to access the stream on your behalf.
--
-- /See:/ 'mkKinesisStreamsInput' smart constructor.
data KinesisStreamsInput = KinesisStreamsInput'
  { resourceARN :: Types.ResourceARN
    -- ^ ARN of the input Amazon Kinesis stream to read.
  , roleARN :: Types.RoleARN
    -- ^ ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'KinesisStreamsInput' value with any optional fields omitted.
mkKinesisStreamsInput
    :: Types.ResourceARN -- ^ 'resourceARN'
    -> Types.RoleARN -- ^ 'roleARN'
    -> KinesisStreamsInput
mkKinesisStreamsInput resourceARN roleARN
  = KinesisStreamsInput'{resourceARN, roleARN}

-- | ARN of the input Amazon Kinesis stream to read.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksiResourceARN :: Lens.Lens' KinesisStreamsInput Types.ResourceARN
ksiResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE ksiResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to access the stream on your behalf. You need to grant the necessary permissions to this role.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ksiRoleARN :: Lens.Lens' KinesisStreamsInput Types.RoleARN
ksiRoleARN = Lens.field @"roleARN"
{-# INLINEABLE ksiRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

instance Core.FromJSON KinesisStreamsInput where
        toJSON KinesisStreamsInput{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceARN" Core..= resourceARN),
                  Core.Just ("RoleARN" Core..= roleARN)])
