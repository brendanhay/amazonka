{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorUpdate
  ( InputLambdaProcessorUpdate (..)
  -- * Smart constructor
  , mkInputLambdaProcessorUpdate
  -- * Lenses
  , ilpuResourceARNUpdate
  , ilpuRoleARNUpdate
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.ResourceARNUpdate as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARNUpdate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents an update to the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> that is used to preprocess the records in the stream.
--
-- /See:/ 'mkInputLambdaProcessorUpdate' smart constructor.
data InputLambdaProcessorUpdate = InputLambdaProcessorUpdate'
  { resourceARNUpdate :: Core.Maybe Types.ResourceARNUpdate
    -- ^ The Amazon Resource Name (ARN) of the new <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used to preprocess the records in the stream.
  , roleARNUpdate :: Core.Maybe Types.RoleARNUpdate
    -- ^ The ARN of the new IAM role that is used to access the AWS Lambda function.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputLambdaProcessorUpdate' value with any optional fields omitted.
mkInputLambdaProcessorUpdate
    :: InputLambdaProcessorUpdate
mkInputLambdaProcessorUpdate
  = InputLambdaProcessorUpdate'{resourceARNUpdate = Core.Nothing,
                                roleARNUpdate = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the new <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used to preprocess the records in the stream.
--
-- /Note:/ Consider using 'resourceARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilpuResourceARNUpdate :: Lens.Lens' InputLambdaProcessorUpdate (Core.Maybe Types.ResourceARNUpdate)
ilpuResourceARNUpdate = Lens.field @"resourceARNUpdate"
{-# INLINEABLE ilpuResourceARNUpdate #-}
{-# DEPRECATED resourceARNUpdate "Use generic-lens or generic-optics with 'resourceARNUpdate' instead"  #-}

-- | The ARN of the new IAM role that is used to access the AWS Lambda function.
--
-- /Note:/ Consider using 'roleARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilpuRoleARNUpdate :: Lens.Lens' InputLambdaProcessorUpdate (Core.Maybe Types.RoleARNUpdate)
ilpuRoleARNUpdate = Lens.field @"roleARNUpdate"
{-# INLINEABLE ilpuRoleARNUpdate #-}
{-# DEPRECATED roleARNUpdate "Use generic-lens or generic-optics with 'roleARNUpdate' instead"  #-}

instance Core.FromJSON InputLambdaProcessorUpdate where
        toJSON InputLambdaProcessorUpdate{..}
          = Core.object
              (Core.catMaybes
                 [("ResourceARNUpdate" Core..=) Core.<$> resourceARNUpdate,
                  ("RoleARNUpdate" Core..=) Core.<$> roleARNUpdate])
