{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.LambdaOutput
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.LambdaOutput
  ( LambdaOutput (..),

    -- * Smart constructor
    mkLambdaOutput,

    -- * Lenses
    loResourceARN,
    loRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | When configuring application output, identifies an AWS Lambda function as the destination. You provide the function Amazon Resource Name (ARN) and also an IAM role ARN that Amazon Kinesis Analytics can use to write to the function on your behalf.
--
-- /See:/ 'mkLambdaOutput' smart constructor.
data LambdaOutput = LambdaOutput'
  { -- | Amazon Resource Name (ARN) of the destination Lambda function to write to.
    resourceARN :: Lude.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function on your behalf. You need to grant the necessary permissions to this role.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaOutput' with the minimum fields required to make a request.
--
-- * 'resourceARN' - Amazon Resource Name (ARN) of the destination Lambda function to write to.
-- * 'roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function on your behalf. You need to grant the necessary permissions to this role.
mkLambdaOutput ::
  -- | 'resourceARN'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  LambdaOutput
mkLambdaOutput pResourceARN_ pRoleARN_ =
  LambdaOutput' {resourceARN = pResourceARN_, roleARN = pRoleARN_}

-- | Amazon Resource Name (ARN) of the destination Lambda function to write to.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loResourceARN :: Lens.Lens' LambdaOutput Lude.Text
loResourceARN = Lens.lens (resourceARN :: LambdaOutput -> Lude.Text) (\s a -> s {resourceARN = a} :: LambdaOutput)
{-# DEPRECATED loResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function on your behalf. You need to grant the necessary permissions to this role.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
loRoleARN :: Lens.Lens' LambdaOutput Lude.Text
loRoleARN = Lens.lens (roleARN :: LambdaOutput -> Lude.Text) (\s a -> s {roleARN = a} :: LambdaOutput)
{-# DEPRECATED loRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.ToJSON LambdaOutput where
  toJSON LambdaOutput' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ResourceARN" Lude..= resourceARN),
            Lude.Just ("RoleARN" Lude..= roleARN)
          ]
      )
