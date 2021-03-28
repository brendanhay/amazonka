{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AppSync.Types.LambdaConflictHandlerConfig
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AppSync.Types.LambdaConflictHandlerConfig
  ( LambdaConflictHandlerConfig (..)
  -- * Smart constructor
  , mkLambdaConflictHandlerConfig
  -- * Lenses
  , lchcLambdaConflictHandlerArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The @LambdaConflictHandlerConfig@ object when configuring LAMBDA as the Conflict Handler.
--
-- /See:/ 'mkLambdaConflictHandlerConfig' smart constructor.
newtype LambdaConflictHandlerConfig = LambdaConflictHandlerConfig'
  { lambdaConflictHandlerArn :: Core.Maybe Core.Text
    -- ^ The Arn for the Lambda function to use as the Conflict Handler.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaConflictHandlerConfig' value with any optional fields omitted.
mkLambdaConflictHandlerConfig
    :: LambdaConflictHandlerConfig
mkLambdaConflictHandlerConfig
  = LambdaConflictHandlerConfig'{lambdaConflictHandlerArn =
                                   Core.Nothing}

-- | The Arn for the Lambda function to use as the Conflict Handler.
--
-- /Note:/ Consider using 'lambdaConflictHandlerArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lchcLambdaConflictHandlerArn :: Lens.Lens' LambdaConflictHandlerConfig (Core.Maybe Core.Text)
lchcLambdaConflictHandlerArn = Lens.field @"lambdaConflictHandlerArn"
{-# INLINEABLE lchcLambdaConflictHandlerArn #-}
{-# DEPRECATED lambdaConflictHandlerArn "Use generic-lens or generic-optics with 'lambdaConflictHandlerArn' instead"  #-}

instance Core.FromJSON LambdaConflictHandlerConfig where
        toJSON LambdaConflictHandlerConfig{..}
          = Core.object
              (Core.catMaybes
                 [("lambdaConflictHandlerArn" Core..=) Core.<$>
                    lambdaConflictHandlerArn])

instance Core.FromJSON LambdaConflictHandlerConfig where
        parseJSON
          = Core.withObject "LambdaConflictHandlerConfig" Core.$
              \ x ->
                LambdaConflictHandlerConfig' Core.<$>
                  (x Core..:? "lambdaConflictHandlerArn")
