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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that contains the Amazon Resource Name (ARN) of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used to preprocess records in the stream, and the ARN of the IAM role that is used to access the AWS Lambda function.
--
-- /See:/ 'mkInputLambdaProcessor' smart constructor.
data InputLambdaProcessor = InputLambdaProcessor'
  { -- | The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that operates on records in the stream.
    resourceARN :: Lude.Text,
    -- | The ARN of the IAM role that is used to access the AWS Lambda function.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputLambdaProcessor' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that operates on records in the stream.
-- * 'roleARN' - The ARN of the IAM role that is used to access the AWS Lambda function.
mkInputLambdaProcessor ::
  -- | 'resourceARN'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  InputLambdaProcessor
mkInputLambdaProcessor pResourceARN_ pRoleARN_ =
  InputLambdaProcessor'
    { resourceARN = pResourceARN_,
      roleARN = pRoleARN_
    }

-- | The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that operates on records in the stream.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilpResourceARN :: Lens.Lens' InputLambdaProcessor Lude.Text
ilpResourceARN = Lens.lens (resourceARN :: InputLambdaProcessor -> Lude.Text) (\s a -> s {resourceARN = a} :: InputLambdaProcessor)
{-# DEPRECATED ilpResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The ARN of the IAM role that is used to access the AWS Lambda function.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilpRoleARN :: Lens.Lens' InputLambdaProcessor Lude.Text
ilpRoleARN = Lens.lens (roleARN :: InputLambdaProcessor -> Lude.Text) (\s a -> s {roleARN = a} :: InputLambdaProcessor)
{-# DEPRECATED ilpRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.ToJSON InputLambdaProcessor where
  toJSON InputLambdaProcessor' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceARN" Lude..= resourceARN),
            Lude.Just ("RoleARN" Lude..= roleARN)
          ]
      )
