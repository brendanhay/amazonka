{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorDescription
  ( InputLambdaProcessorDescription (..)
  -- * Smart constructor
  , mkInputLambdaProcessorDescription
  -- * Lenses
  , ilpdResourceARN
  , ilpdRoleARN
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.ResourceARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that contains the Amazon Resource Name (ARN) of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used to preprocess records in the stream, and the ARN of the IAM role that is used to access the AWS Lambda expression.
--
-- /See:/ 'mkInputLambdaProcessorDescription' smart constructor.
data InputLambdaProcessorDescription = InputLambdaProcessorDescription'
  { resourceARN :: Core.Maybe Types.ResourceARN
    -- ^ The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used to preprocess the records in the stream.
  , roleARN :: Core.Maybe Types.RoleARN
    -- ^ The ARN of the IAM role that is used to access the AWS Lambda function.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputLambdaProcessorDescription' value with any optional fields omitted.
mkInputLambdaProcessorDescription
    :: InputLambdaProcessorDescription
mkInputLambdaProcessorDescription
  = InputLambdaProcessorDescription'{resourceARN = Core.Nothing,
                                     roleARN = Core.Nothing}

-- | The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used to preprocess the records in the stream.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilpdResourceARN :: Lens.Lens' InputLambdaProcessorDescription (Core.Maybe Types.ResourceARN)
ilpdResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE ilpdResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | The ARN of the IAM role that is used to access the AWS Lambda function.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilpdRoleARN :: Lens.Lens' InputLambdaProcessorDescription (Core.Maybe Types.RoleARN)
ilpdRoleARN = Lens.field @"roleARN"
{-# INLINEABLE ilpdRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

instance Core.FromJSON InputLambdaProcessorDescription where
        parseJSON
          = Core.withObject "InputLambdaProcessorDescription" Core.$
              \ x ->
                InputLambdaProcessorDescription' Core.<$>
                  (x Core..:? "ResourceARN") Core.<*> x Core..:? "RoleARN"
