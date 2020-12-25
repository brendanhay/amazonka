{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoTAnalytics.Types.LambdaActivity
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.LambdaActivity
  ( LambdaActivity (..),

    -- * Smart constructor
    mkLambdaActivity,

    -- * Lenses
    laName,
    laLambdaName,
    laBatchSize,
    laNext,
  )
where

import qualified Network.AWS.IoTAnalytics.Types.LambdaName as Types
import qualified Network.AWS.IoTAnalytics.Types.Name as Types
import qualified Network.AWS.IoTAnalytics.Types.Next as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An activity that runs a Lambda function to modify the message.
--
-- /See:/ 'mkLambdaActivity' smart constructor.
data LambdaActivity = LambdaActivity'
  { -- | The name of the lambda activity.
    name :: Types.Name,
    -- | The name of the Lambda function that is run on the message.
    lambdaName :: Types.LambdaName,
    -- | The number of messages passed to the Lambda function for processing.
    --
    -- The Lambda function must be able to process all of these messages within five minutes, which is the maximum timeout duration for Lambda functions.
    batchSize :: Core.Natural,
    -- | The next activity in the pipeline.
    next :: Core.Maybe Types.Next
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaActivity' value with any optional fields omitted.
mkLambdaActivity ::
  -- | 'name'
  Types.Name ->
  -- | 'lambdaName'
  Types.LambdaName ->
  -- | 'batchSize'
  Core.Natural ->
  LambdaActivity
mkLambdaActivity name lambdaName batchSize =
  LambdaActivity' {name, lambdaName, batchSize, next = Core.Nothing}

-- | The name of the lambda activity.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laName :: Lens.Lens' LambdaActivity Types.Name
laName = Lens.field @"name"
{-# DEPRECATED laName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the Lambda function that is run on the message.
--
-- /Note:/ Consider using 'lambdaName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laLambdaName :: Lens.Lens' LambdaActivity Types.LambdaName
laLambdaName = Lens.field @"lambdaName"
{-# DEPRECATED laLambdaName "Use generic-lens or generic-optics with 'lambdaName' instead." #-}

-- | The number of messages passed to the Lambda function for processing.
--
-- The Lambda function must be able to process all of these messages within five minutes, which is the maximum timeout duration for Lambda functions.
--
-- /Note:/ Consider using 'batchSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laBatchSize :: Lens.Lens' LambdaActivity Core.Natural
laBatchSize = Lens.field @"batchSize"
{-# DEPRECATED laBatchSize "Use generic-lens or generic-optics with 'batchSize' instead." #-}

-- | The next activity in the pipeline.
--
-- /Note:/ Consider using 'next' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laNext :: Lens.Lens' LambdaActivity (Core.Maybe Types.Next)
laNext = Lens.field @"next"
{-# DEPRECATED laNext "Use generic-lens or generic-optics with 'next' instead." #-}

instance Core.FromJSON LambdaActivity where
  toJSON LambdaActivity {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("name" Core..= name),
            Core.Just ("lambdaName" Core..= lambdaName),
            Core.Just ("batchSize" Core..= batchSize),
            ("next" Core..=) Core.<$> next
          ]
      )

instance Core.FromJSON LambdaActivity where
  parseJSON =
    Core.withObject "LambdaActivity" Core.$
      \x ->
        LambdaActivity'
          Core.<$> (x Core..: "name")
          Core.<*> (x Core..: "lambdaName")
          Core.<*> (x Core..: "batchSize")
          Core.<*> (x Core..:? "next")
