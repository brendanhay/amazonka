{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.Types.LambdaResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Snowball.Types.LambdaResource
  ( LambdaResource (..),

    -- * Smart constructor
    mkLambdaResource,

    -- * Lenses
    lrEventTriggers,
    lrLambdaArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Snowball.Types.EventTriggerDefinition as Types
import qualified Network.AWS.Snowball.Types.LambdaArn as Types

-- | Identifies
--
-- /See:/ 'mkLambdaResource' smart constructor.
data LambdaResource = LambdaResource'
  { -- | The array of ARNs for 'S3Resource' objects to trigger the 'LambdaResource' objects associated with this job.
    eventTriggers :: Core.Maybe [Types.EventTriggerDefinition],
    -- | An Amazon Resource Name (ARN) that represents an AWS Lambda function to be triggered by PUT object actions on the associated local Amazon S3 resource.
    lambdaArn :: Core.Maybe Types.LambdaArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaResource' value with any optional fields omitted.
mkLambdaResource ::
  LambdaResource
mkLambdaResource =
  LambdaResource'
    { eventTriggers = Core.Nothing,
      lambdaArn = Core.Nothing
    }

-- | The array of ARNs for 'S3Resource' objects to trigger the 'LambdaResource' objects associated with this job.
--
-- /Note:/ Consider using 'eventTriggers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrEventTriggers :: Lens.Lens' LambdaResource (Core.Maybe [Types.EventTriggerDefinition])
lrEventTriggers = Lens.field @"eventTriggers"
{-# DEPRECATED lrEventTriggers "Use generic-lens or generic-optics with 'eventTriggers' instead." #-}

-- | An Amazon Resource Name (ARN) that represents an AWS Lambda function to be triggered by PUT object actions on the associated local Amazon S3 resource.
--
-- /Note:/ Consider using 'lambdaArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lrLambdaArn :: Lens.Lens' LambdaResource (Core.Maybe Types.LambdaArn)
lrLambdaArn = Lens.field @"lambdaArn"
{-# DEPRECATED lrLambdaArn "Use generic-lens or generic-optics with 'lambdaArn' instead." #-}

instance Core.FromJSON LambdaResource where
  toJSON LambdaResource {..} =
    Core.object
      ( Core.catMaybes
          [ ("EventTriggers" Core..=) Core.<$> eventTriggers,
            ("LambdaArn" Core..=) Core.<$> lambdaArn
          ]
      )

instance Core.FromJSON LambdaResource where
  parseJSON =
    Core.withObject "LambdaResource" Core.$
      \x ->
        LambdaResource'
          Core.<$> (x Core..:? "EventTriggers") Core.<*> (x Core..:? "LambdaArn")
