-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorUpdate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.KinesisAnalytics.Types.InputLambdaProcessorUpdate
  ( InputLambdaProcessorUpdate (..),

    -- * Smart constructor
    mkInputLambdaProcessorUpdate,

    -- * Lenses
    ilpuRoleARNUpdate,
    ilpuResourceARNUpdate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents an update to the <https://docs.aws.amazon.com/kinesisanalytics/latest/dev/API_InputLambdaProcessor.html InputLambdaProcessor> that is used to preprocess the records in the stream.
--
-- /See:/ 'mkInputLambdaProcessorUpdate' smart constructor.
data InputLambdaProcessorUpdate = InputLambdaProcessorUpdate'
  { roleARNUpdate ::
      Lude.Maybe Lude.Text,
    resourceARNUpdate ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InputLambdaProcessorUpdate' with the minimum fields required to make a request.
--
-- * 'resourceARNUpdate' - The Amazon Resource Name (ARN) of the new <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used to preprocess the records in the stream.
-- * 'roleARNUpdate' - The ARN of the new IAM role that is used to access the AWS Lambda function.
mkInputLambdaProcessorUpdate ::
  InputLambdaProcessorUpdate
mkInputLambdaProcessorUpdate =
  InputLambdaProcessorUpdate'
    { roleARNUpdate = Lude.Nothing,
      resourceARNUpdate = Lude.Nothing
    }

-- | The ARN of the new IAM role that is used to access the AWS Lambda function.
--
-- /Note:/ Consider using 'roleARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilpuRoleARNUpdate :: Lens.Lens' InputLambdaProcessorUpdate (Lude.Maybe Lude.Text)
ilpuRoleARNUpdate = Lens.lens (roleARNUpdate :: InputLambdaProcessorUpdate -> Lude.Maybe Lude.Text) (\s a -> s {roleARNUpdate = a} :: InputLambdaProcessorUpdate)
{-# DEPRECATED ilpuRoleARNUpdate "Use generic-lens or generic-optics with 'roleARNUpdate' instead." #-}

-- | The Amazon Resource Name (ARN) of the new <https://docs.aws.amazon.com/lambda/ AWS Lambda> function that is used to preprocess the records in the stream.
--
-- /Note:/ Consider using 'resourceARNUpdate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ilpuResourceARNUpdate :: Lens.Lens' InputLambdaProcessorUpdate (Lude.Maybe Lude.Text)
ilpuResourceARNUpdate = Lens.lens (resourceARNUpdate :: InputLambdaProcessorUpdate -> Lude.Maybe Lude.Text) (\s a -> s {resourceARNUpdate = a} :: InputLambdaProcessorUpdate)
{-# DEPRECATED ilpuResourceARNUpdate "Use generic-lens or generic-optics with 'resourceARNUpdate' instead." #-}

instance Lude.ToJSON InputLambdaProcessorUpdate where
  toJSON InputLambdaProcessorUpdate' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RoleARNUpdate" Lude..=) Lude.<$> roleARNUpdate,
            ("ResourceARNUpdate" Lude..=) Lude.<$> resourceARNUpdate
          ]
      )
