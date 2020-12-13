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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | For an application output, describes the AWS Lambda function configured as its destination.
--
-- /See:/ 'mkLambdaOutputDescription' smart constructor.
data LambdaOutputDescription = LambdaOutputDescription'
  { -- | Amazon Resource Name (ARN) of the destination Lambda function.
    resourceARN :: Lude.Maybe Lude.Text,
    -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaOutputDescription' with the minimum fields required to make a request.
--
-- * 'resourceARN' - Amazon Resource Name (ARN) of the destination Lambda function.
-- * 'roleARN' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function.
mkLambdaOutputDescription ::
  LambdaOutputDescription
mkLambdaOutputDescription =
  LambdaOutputDescription'
    { resourceARN = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Amazon Resource Name (ARN) of the destination Lambda function.
--
-- /Note:/ Consider using 'resourceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lodResourceARN :: Lens.Lens' LambdaOutputDescription (Lude.Maybe Lude.Text)
lodResourceARN = Lens.lens (resourceARN :: LambdaOutputDescription -> Lude.Maybe Lude.Text) (\s a -> s {resourceARN = a} :: LambdaOutputDescription)
{-# DEPRECATED lodResourceARN "Use generic-lens or generic-optics with 'resourceARN' instead." #-}

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lodRoleARN :: Lens.Lens' LambdaOutputDescription (Lude.Maybe Lude.Text)
lodRoleARN = Lens.lens (roleARN :: LambdaOutputDescription -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: LambdaOutputDescription)
{-# DEPRECATED lodRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON LambdaOutputDescription where
  parseJSON =
    Lude.withObject
      "LambdaOutputDescription"
      ( \x ->
          LambdaOutputDescription'
            Lude.<$> (x Lude..:? "ResourceARN") Lude.<*> (x Lude..:? "RoleARN")
      )
