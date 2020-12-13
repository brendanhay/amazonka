{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorDescription
  ( InputLambdaProcessorDescription (..),

    -- * Smart constructor
    mkInputLambdaProcessorDescription,

    -- * Lenses
    ilpdResourceARN,
    ilpdRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An object that contains the Amazon Resource Name (ARN) of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used to preprocess records in the stream, and the ARN of the IAM role that is used to access the AWS Lambda expression.
--
-- /See:/ 'mkInputLambdaProcessorDescription' smart constructor.
data InputLambdaProcessorDescription = InputLambdaProcessorDescription'
  { -- | The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used to preprocess the records in the stream.
    resourceARN :: Lude.Maybe Lude.Text,
    -- | The ARN of the IAM role that is used to access the AWS Lambda function.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputLambdaProcessorDescription' with the minimum fields required to make a request.
--
-- * 'resourceARN' - The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used to preprocess the records in the stream.
-- * 'roleARN' - The ARN of the IAM role that is used to access the AWS Lambda function.
mkInputLambdaProcessorDescription ::
  InputLambdaProcessorDescription
mkInputLambdaProcessorDescription =
  InputLambdaProcessorDescription'
    { resourceARN = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | The ARN of the <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used to preprocess the records in the stream.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilpdResourceARN :: Lens.Lens' InputLambdaProcessorDescription (Lude.Maybe Lude.Text)
ilpdResourceARN = Lens.lens (resourceARN :: InputLambdaProcessorDescription -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: InputLambdaProcessorDescription)
{-# DEPRECATED ilpdResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | The ARN of the IAM role that is used to access the AWS Lambda function.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilpdRoleARN :: Lens.Lens' InputLambdaProcessorDescription (Lude.Maybe Lude.Text)
ilpdRoleARN = Lens.lens (roleARN :: InputLambdaProcessorDescription -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: InputLambdaProcessorDescription)
{-# DEPRECATED ilpdRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON InputLambdaProcessorDescription where
  parseJSON =
    Lude.withObject
      "InputLambdaProcessorDescription"
      ( \x ->
          InputLambdaProcessorDescription'
            Lude.<$> (x Lude..:? "ResourceARN") Lude.<*> (x Lude..:? "RoleARN")
      )
