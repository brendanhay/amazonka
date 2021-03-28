{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.LambdaOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.KinesisAnalytics.Types.LambdaOutput
  ( LambdaOutput (..)
  -- * Smart constructor
  , mkLambdaOutput
  -- * Lenses
  , loResourceARN
  , loRoleARN
  ) where

import qualified Network.AWS.KinesisAnalytics.Types.ResourceARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | When configuring application output, identifies an AWS Lambda function as the destination. You provide the function Amazon Resource Name (ARN) and also an IAM role ARN that Amazon Kinesis Analytics can use to write to the function on your behalf. 
--
-- /See:/ 'mkLambdaOutput' smart constructor.
data LambdaOutput = LambdaOutput'
  { resourceARN :: Types.ResourceARN
    -- ^ Amazon Resource Name (ARN) of the destination Lambda function to write to.
  , roleARN :: Types.RoleARN
    -- ^ ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function on your behalf. You need to grant the necessary permissions to this role. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaOutput' value with any optional fields omitted.
mkLambdaOutput
    :: Types.ResourceARN -- ^ 'resourceARN'
    -> Types.RoleARN -- ^ 'roleARN'
    -> LambdaOutput
mkLambdaOutput resourceARN roleARN
  = LambdaOutput'{resourceARN, roleARN}

-- | Amazon Resource Name (ARN) of the destination Lambda function to write to.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loResourceARN :: Lens.Lens' LambdaOutput Types.ResourceARN
loResourceARN = Lens.field @"resourceARN"
{-# INLINEABLE loResourceARN #-}
{-# DEPRECATED resourceARN "Use generic-lens or generic-optics with 'resourceARN' instead"  #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function on your behalf. You need to grant the necessary permissions to this role. 
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loRoleARN :: Lens.Lens' LambdaOutput Types.RoleARN
loRoleARN = Lens.field @"roleARN"
{-# INLINEABLE loRoleARN #-}
{-# DEPRECATED roleARN "Use generic-lens or generic-optics with 'roleARN' instead"  #-}

instance Core.FromJSON LambdaOutput where
        toJSON LambdaOutput{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceARN" Core..= resourceARN),
                  Core.Just ("RoleARN" Core..= roleARN)])
