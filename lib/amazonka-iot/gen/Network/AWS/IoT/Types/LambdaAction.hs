{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.LambdaAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.LambdaAction
  ( LambdaAction (..),

    -- * Smart constructor
    mkLambdaAction,

    -- * Lenses
    laFunctionArn,
  )
where

import qualified Network.AWS.IoT.Types.FunctionArn as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an action to invoke a Lambda function.
--
-- /See:/ 'mkLambdaAction' smart constructor.
newtype LambdaAction = LambdaAction'
  { -- | The ARN of the Lambda function.
    functionArn :: Types.FunctionArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaAction' value with any optional fields omitted.
mkLambdaAction ::
  -- | 'functionArn'
  Types.FunctionArn ->
  LambdaAction
mkLambdaAction functionArn = LambdaAction' {functionArn}

-- | The ARN of the Lambda function.
--
-- /Note:/ Consider using 'functionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laFunctionArn :: Lens.Lens' LambdaAction Types.FunctionArn
laFunctionArn = Lens.field @"functionArn"
{-# DEPRECATED laFunctionArn "Use generic-lens or generic-optics with 'functionArn' instead." #-}

instance Core.FromJSON LambdaAction where
  toJSON LambdaAction {..} =
    Core.object
      (Core.catMaybes [Core.Just ("functionArn" Core..= functionArn)])

instance Core.FromJSON LambdaAction where
  parseJSON =
    Core.withObject "LambdaAction" Core.$
      \x -> LambdaAction' Core.<$> (x Core..: "functionArn")
