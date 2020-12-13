{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SqsAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SqsAction
  ( SqsAction (..),

    -- * Smart constructor
    mkSqsAction,

    -- * Lenses
    saUseBase64,
    saQueueURL,
    saRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an action to publish data to an Amazon SQS queue.
--
-- /See:/ 'mkSqsAction' smart constructor.
data SqsAction = SqsAction'
  { -- | Specifies whether to use Base64 encoding.
    useBase64 :: Lude.Maybe Lude.Bool,
    -- | The URL of the Amazon SQS queue.
    queueURL :: Lude.Text,
    -- | The ARN of the IAM role that grants access.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SqsAction' with the minimum fields required to make a request.
--
-- * 'useBase64' - Specifies whether to use Base64 encoding.
-- * 'queueURL' - The URL of the Amazon SQS queue.
-- * 'roleARN' - The ARN of the IAM role that grants access.
mkSqsAction ::
  -- | 'queueURL'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  SqsAction
mkSqsAction pQueueURL_ pRoleARN_ =
  SqsAction'
    { useBase64 = Lude.Nothing,
      queueURL = pQueueURL_,
      roleARN = pRoleARN_
    }

-- | Specifies whether to use Base64 encoding.
--
-- /Note:/ Consider using 'useBase64' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saUseBase64 :: Lens.Lens' SqsAction (Lude.Maybe Lude.Bool)
saUseBase64 = Lens.lens (useBase64 :: SqsAction -> Lude.Maybe Lude.Bool) (\s a -> s {useBase64 = a} :: SqsAction)
{-# DEPRECATED saUseBase64 "Use generic-lens or generic-optics with 'useBase64' instead." #-}

-- | The URL of the Amazon SQS queue.
--
-- /Note:/ Consider using 'queueURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saQueueURL :: Lens.Lens' SqsAction Lude.Text
saQueueURL = Lens.lens (queueURL :: SqsAction -> Lude.Text) (\s a -> s {queueURL = a} :: SqsAction)
{-# DEPRECATED saQueueURL "Use generic-lens or generic-optics with 'queueURL' instead." #-}

-- | The ARN of the IAM role that grants access.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
saRoleARN :: Lens.Lens' SqsAction Lude.Text
saRoleARN = Lens.lens (roleARN :: SqsAction -> Lude.Text) (\s a -> s {roleARN = a} :: SqsAction)
{-# DEPRECATED saRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON SqsAction where
  parseJSON =
    Lude.withObject
      "SqsAction"
      ( \x ->
          SqsAction'
            Lude.<$> (x Lude..:? "useBase64")
            Lude.<*> (x Lude..: "queueUrl")
            Lude.<*> (x Lude..: "roleArn")
      )

instance Lude.ToJSON SqsAction where
  toJSON SqsAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("useBase64" Lude..=) Lude.<$> useBase64,
            Lude.Just ("queueUrl" Lude..= queueURL),
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
