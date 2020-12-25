{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.LambdaOutputUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.LambdaOutputUpdate
  ( LambdaOutputUpdate (..),

    -- * Smart constructor
    mkLambdaOutputUpdate,

    -- * Lenses
    louResourceARNUpdate,
    louRoleARNUpdate,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types.ResourceARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARNUpdate as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | When updating an output configuration using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_UpdateApplication.html UpdateApplication> operation, provides information about an AWS Lambda function configured as the destination.
--
-- /See:/ 'mkLambdaOutputUpdate' smart constructor.
data LambdaOutputUpdate = LambdaOutputUpdate'
  { -- | Amazon Resource Name (ARN) of the destination Lambda function.
    resourceARNUpdate :: Core.Maybe Types.ResourceARN,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function on your behalf. You need to grant the necessary permissions to this role.
    roleARNUpdate :: Core.Maybe Types.RoleARNUpdate
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaOutputUpdate' value with any optional fields omitted.
mkLambdaOutputUpdate ::
  LambdaOutputUpdate
mkLambdaOutputUpdate =
  LambdaOutputUpdate'
    { resourceARNUpdate = Core.Nothing,
      roleARNUpdate = Core.Nothing
    }

-- | Amazon Resource Name (ARN) of the destination Lambda function.
--
-- /Note:/ Consider using 'resourceARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
louResourceARNUpdate :: Lens.Lens' LambdaOutputUpdate (Core.Maybe Types.ResourceARN)
louResourceARNUpdate = Lens.field @"resourceARNUpdate"
{-# DEPRECATED louResourceARNUpdate "Use generic-lens or generic-optics with 'resourceARNUpdate' instead." #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function on your behalf. You need to grant the necessary permissions to this role.
--
-- /Note:/ Consider using 'roleARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
louRoleARNUpdate :: Lens.Lens' LambdaOutputUpdate (Core.Maybe Types.RoleARNUpdate)
louRoleARNUpdate = Lens.field @"roleARNUpdate"
{-# DEPRECATED louRoleARNUpdate "Use generic-lens or generic-optics with 'roleARNUpdate' instead." #-}

instance Core.FromJSON LambdaOutputUpdate where
  toJSON LambdaOutputUpdate {..} =
    Core.object
      ( Core.catMaybes
          [ ("ResourceARNUpdate" Core..=) Core.<$> resourceARNUpdate,
            ("RoleARNUpdate" Core..=) Core.<$> roleARNUpdate
          ]
      )
