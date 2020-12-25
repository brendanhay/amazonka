{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.LambdaOutputDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.LambdaOutputDescription
  ( LambdaOutputDescription (..),

    -- * Smart constructor
    mkLambdaOutputDescription,

    -- * Lenses
    lodResourceARN,
    lodRoleARN,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types.ResourceARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | For an application output, describes the AWS Lambda function configured as its destination.
--
-- /See:/ 'mkLambdaOutputDescription' smart constructor.
data LambdaOutputDescription = LambdaOutputDescription'
  { -- | Amazon Resource Name (ARN) of the destination Lambda function.
    resourceARN :: Core.Maybe Types.ResourceARN,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function.
    roleARN :: Core.Maybe Types.RoleARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaOutputDescription' value with any optional fields omitted.
mkLambdaOutputDescription ::
  LambdaOutputDescription
mkLambdaOutputDescription =
  LambdaOutputDescription'
    { resourceARN = Core.Nothing,
      roleARN = Core.Nothing
    }

-- | Amazon Resource Name (ARN) of the destination Lambda function.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lodResourceARN :: Lens.Lens' LambdaOutputDescription (Core.Maybe Types.ResourceARN)
lodResourceARN = Lens.field @"resourceARN"
{-# DEPRECATED lodResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lodRoleARN :: Lens.Lens' LambdaOutputDescription (Core.Maybe Types.RoleARN)
lodRoleARN = Lens.field @"roleARN"
{-# DEPRECATED lodRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Core.FromJSON LambdaOutputDescription where
  parseJSON =
    Core.withObject "LambdaOutputDescription" Core.$
      \x ->
        LambdaOutputDescription'
          Core.<$> (x Core..:? "ResourceARN") Core.<*> (x Core..:? "RoleARN")
