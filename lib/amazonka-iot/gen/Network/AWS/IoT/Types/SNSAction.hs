{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.SNSAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.SNSAction
  ( SNSAction (..),

    -- * Smart constructor
    mkSNSAction,

    -- * Lenses
    snsaTargetARN,
    snsaMessageFormat,
    snsaRoleARN,
  )
where

import Network.AWS.IoT.Types.MessageFormat
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an action to publish to an Amazon SNS topic.
--
-- /See:/ 'mkSNSAction' smart constructor.
data SNSAction = SNSAction'
  { -- | The ARN of the SNS topic.
    targetARN :: Lude.Text,
    -- | (Optional) The message format of the message to publish. Accepted values are "JSON" and "RAW". The default value of the attribute is "RAW". SNS uses this setting to determine if the payload should be parsed and relevant platform-specific bits of the payload should be extracted. To read more about SNS message formats, see <https://docs.aws.amazon.com/sns/latest/dg/json-formats.html https://docs.aws.amazon.com/sns/latest/dg/json-formats.html> refer to their official documentation.
    messageFormat :: Lude.Maybe MessageFormat,
    -- | The ARN of the IAM role that grants access.
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SNSAction' with the minimum fields required to make a request.
--
-- * 'targetARN' - The ARN of the SNS topic.
-- * 'messageFormat' - (Optional) The message format of the message to publish. Accepted values are "JSON" and "RAW". The default value of the attribute is "RAW". SNS uses this setting to determine if the payload should be parsed and relevant platform-specific bits of the payload should be extracted. To read more about SNS message formats, see <https://docs.aws.amazon.com/sns/latest/dg/json-formats.html https://docs.aws.amazon.com/sns/latest/dg/json-formats.html> refer to their official documentation.
-- * 'roleARN' - The ARN of the IAM role that grants access.
mkSNSAction ::
  -- | 'targetARN'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  SNSAction
mkSNSAction pTargetARN_ pRoleARN_ =
  SNSAction'
    { targetARN = pTargetARN_,
      messageFormat = Lude.Nothing,
      roleARN = pRoleARN_
    }

-- | The ARN of the SNS topic.
--
-- /Note:/ Consider using 'targetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snsaTargetARN :: Lens.Lens' SNSAction Lude.Text
snsaTargetARN = Lens.lens (targetARN :: SNSAction -> Lude.Text) (\s a -> s {targetARN = a} :: SNSAction)
{-# DEPRECATED snsaTargetARN "Use generic-lens or generic-optics with 'targetARN' instead." #-}

-- | (Optional) The message format of the message to publish. Accepted values are "JSON" and "RAW". The default value of the attribute is "RAW". SNS uses this setting to determine if the payload should be parsed and relevant platform-specific bits of the payload should be extracted. To read more about SNS message formats, see <https://docs.aws.amazon.com/sns/latest/dg/json-formats.html https://docs.aws.amazon.com/sns/latest/dg/json-formats.html> refer to their official documentation.
--
-- /Note:/ Consider using 'messageFormat' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snsaMessageFormat :: Lens.Lens' SNSAction (Lude.Maybe MessageFormat)
snsaMessageFormat = Lens.lens (messageFormat :: SNSAction -> Lude.Maybe MessageFormat) (\s a -> s {messageFormat = a} :: SNSAction)
{-# DEPRECATED snsaMessageFormat "Use generic-lens or generic-optics with 'messageFormat' instead." #-}

-- | The ARN of the IAM role that grants access.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
snsaRoleARN :: Lens.Lens' SNSAction Lude.Text
snsaRoleARN = Lens.lens (roleARN :: SNSAction -> Lude.Text) (\s a -> s {roleARN = a} :: SNSAction)
{-# DEPRECATED snsaRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromJSON SNSAction where
  parseJSON =
    Lude.withObject
      "SNSAction"
      ( \x ->
          SNSAction'
            Lude.<$> (x Lude..: "targetArn")
            Lude.<*> (x Lude..:? "messageFormat")
            Lude.<*> (x Lude..: "roleArn")
      )

instance Lude.ToJSON SNSAction where
  toJSON SNSAction' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("targetArn" Lude..= targetARN),
            ("messageFormat" Lude..=) Lude.<$> messageFormat,
            Lude.Just ("roleArn" Lude..= roleARN)
          ]
      )
