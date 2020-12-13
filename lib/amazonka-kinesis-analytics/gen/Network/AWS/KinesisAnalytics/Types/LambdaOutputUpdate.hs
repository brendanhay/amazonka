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
    louRoleARNUpdate,
    louResourceARNUpdate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | When updating an output configuration using the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_UpdateApplication.html UpdateApplication> operation, provides information about an AWS Lambda function configured as the destination.
--
-- /See:/ 'mkLambdaOutputUpdate' smart constructor.
data LambdaOutputUpdate = LambdaOutputUpdate'
  { -- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function on your behalf. You need to grant the necessary permissions to this role.
    roleARNUpdate :: Lude.Maybe Lude.Text,
    -- | Amazon Resource Name (ARN) of the destination Lambda function.
    resourceARNUpdate :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaOutputUpdate' with the minimum fields required to make a request.
--
-- * 'roleARNUpdate' - ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function on your behalf. You need to grant the necessary permissions to this role.
-- * 'resourceARNUpdate' - Amazon Resource Name (ARN) of the destination Lambda function.
mkLambdaOutputUpdate ::
  LambdaOutputUpdate
mkLambdaOutputUpdate =
  LambdaOutputUpdate'
    { roleARNUpdate = Lude.Nothing,
      resourceARNUpdate = Lude.Nothing
    }

-- | ARN of the IAM role that Amazon Kinesis Analytics can assume to write to the destination function on your behalf. You need to grant the necessary permissions to this role.
--
-- /Note:/ Consider using 'roleARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
louRoleARNUpdate :: Lens.Lens' LambdaOutputUpdate (Lude.Maybe Lude.Text)
louRoleARNUpdate = Lens.lens (roleARNUpdate :: LambdaOutputUpdate -> Lude.Maybe Lude.Text) (\s a -> s {roleARNUpdate = a} :: LambdaOutputUpdate)
{-# DEPRECATED louRoleARNUpdate "Use generic-lens or generic-optics with 'roleARNUpdate' instead." #-}

-- | Amazon Resource Name (ARN) of the destination Lambda function.
--
-- /Note:/ Consider using 'resourceARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
louResourceARNUpdate :: Lens.Lens' LambdaOutputUpdate (Lude.Maybe Lude.Text)
louResourceARNUpdate = Lens.lens (resourceARNUpdate :: LambdaOutputUpdate -> Lude.Maybe Lude.Text) (\s a -> s {resourceARNUpdate = a} :: LambdaOutputUpdate)
{-# DEPRECATED louResourceARNUpdate "Use generic-lens or generic-optics with 'resourceARNUpdate' instead." #-}

instance Lude.ToJSON LambdaOutputUpdate where
  toJSON LambdaOutputUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RoleARNUpdate" Lude..=) Lude.<$> roleARNUpdate,
            ("ResourceARNUpdate" Lude..=) Lude.<$> resourceARNUpdate
          ]
      )
