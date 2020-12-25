{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputLambdaProcessor
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputLambdaProcessor
  ( InputLambdaProcessor (..),

    -- * Smart constructor
    mkInputLambdaProcessor,

    -- * Lenses
    ilpResourceARN,
    ilpRoleARN,
  )
where

import qualified Network.AWS.KinesisAnalytics.Types.ResourceARN as Types
import qualified Network.AWS.KinesisAnalytics.Types.RoleARN as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | An object that contains the Amazon Resource Name (ARN) of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used to preprocess records in the stream, and the ARN of the IAM role that is used to access the AWS Lambda function.
--
-- /See:/ 'mkInputLambdaProcessor' smart constructor.
data InputLambdaProcessor = InputLambdaProcessor'
  { -- | The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that operates on records in the stream.
    resourceARN :: Types.ResourceARN,
    -- | The ARN of the IAM role that is used to access the AWS Lambda function.
    roleARN :: Types.RoleARN
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InputLambdaProcessor' value with any optional fields omitted.
mkInputLambdaProcessor ::
  -- | 'resourceARN'
  Types.ResourceARN ->
  -- | 'roleARN'
  Types.RoleARN ->
  InputLambdaProcessor
mkInputLambdaProcessor resourceARN roleARN =
  InputLambdaProcessor' {resourceARN, roleARN}

-- | The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that operates on records in the stream.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilpResourceARN :: Lens.Lens' InputLambdaProcessor Types.ResourceARN
ilpResourceARN = Lens.field @"resourceARN"
{-# DEPRECATED ilpResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The ARN of the IAM role that is used to access the AWS Lambda function.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilpRoleARN :: Lens.Lens' InputLambdaProcessor Types.RoleARN
ilpRoleARN = Lens.field @"roleARN"
{-# DEPRECATED ilpRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Core.FromJSON InputLambdaProcessor where
  toJSON InputLambdaProcessor {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ResourceARN" Core..= resourceARN),
            Core.Just ("RoleARN" Core..= roleARN)
          ]
      )
