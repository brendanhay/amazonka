{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.Types.DeadLetterConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudWatchEvents.Types.DeadLetterConfig
  ( DeadLetterConfig (..)
  -- * Smart constructor
  , mkDeadLetterConfig
  -- * Lenses
  , dlcArn
  ) where

import qualified Network.AWS.CloudWatchEvents.Types.Arn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A @DeadLetterConfig@ object that contains information about a dead-letter queue configuration.
--
-- /See:/ 'mkDeadLetterConfig' smart constructor.
newtype DeadLetterConfig = DeadLetterConfig'
  { arn :: Core.Maybe Types.Arn
    -- ^ The ARN of the SQS queue specified as the target for the dead-letter queue.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DeadLetterConfig' value with any optional fields omitted.
mkDeadLetterConfig
    :: DeadLetterConfig
mkDeadLetterConfig = DeadLetterConfig'{arn = Core.Nothing}

-- | The ARN of the SQS queue specified as the target for the dead-letter queue.
--
-- /Note:/ Consider using 'arn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dlcArn :: Lens.Lens' DeadLetterConfig (Core.Maybe Types.Arn)
dlcArn = Lens.field @"arn"
{-# INLINEABLE dlcArn #-}
{-# DEPRECATED arn "Use generic-lens or generic-optics with 'arn' instead"  #-}

instance Core.FromJSON DeadLetterConfig where
        toJSON DeadLetterConfig{..}
          = Core.object (Core.catMaybes [("Arn" Core..=) Core.<$> arn])

instance Core.FromJSON DeadLetterConfig where
        parseJSON
          = Core.withObject "DeadLetterConfig" Core.$
              \ x -> DeadLetterConfig' Core.<$> (x Core..:? "Arn")
