{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.Types.DeadLetterConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lambda.Types.DeadLetterConfig
  ( DeadLetterConfig (..)
  -- * Smart constructor
  , mkDeadLetterConfig
  -- * Lenses
  , dlcTargetArn
  ) where

import qualified Network.AWS.Lambda.Types.TargetArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The <https://docs.aws.amazon.com/lambda/latest/dg/invocation-async.html#dlq dead-letter queue> for failed asynchronous invocations.
--
-- /See:/ 'mkDeadLetterConfig' smart constructor.
newtype DeadLetterConfig = DeadLetterConfig'
  { targetArn :: Core.Maybe Types.TargetArn
    -- ^ The Amazon Resource Name (ARN) of an Amazon SQS queue or Amazon SNS topic.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeadLetterConfig' value with any optional fields omitted.
mkDeadLetterConfig
    :: DeadLetterConfig
mkDeadLetterConfig = DeadLetterConfig'{targetArn = Core.Nothing}

-- | The Amazon Resource Name (ARN) of an Amazon SQS queue or Amazon SNS topic.
--
-- /Note:/ Consider using 'targetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcTargetArn :: Lens.Lens' DeadLetterConfig (Core.Maybe Types.TargetArn)
dlcTargetArn = Lens.field @"targetArn"
{-# INLINEABLE dlcTargetArn #-}
{-# DEPRECATED targetArn "Use generic-lens or generic-optics with 'targetArn' instead"  #-}

instance Core.FromJSON DeadLetterConfig where
        toJSON DeadLetterConfig{..}
          = Core.object
              (Core.catMaybes [("TargetArn" Core..=) Core.<$> targetArn])

instance Core.FromJSON DeadLetterConfig where
        parseJSON
          = Core.withObject "DeadLetterConfig" Core.$
              \ x -> DeadLetterConfig' Core.<$> (x Core..:? "TargetArn")
