{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.LambdaAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.LambdaAction
  ( LambdaAction (..),

    -- * Smart constructor
    mkLambdaAction,

    -- * Lenses
    laInvocationType,
    laTopicARN,
    laFunctionARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.SES.Types.InvocationType

-- | When included in a receipt rule, this action calls an AWS Lambda function and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
--
-- To enable Amazon SES to call your AWS Lambda function or to publish to an Amazon SNS topic of another account, Amazon SES must have permission to access those resources. For information about giving permissions, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> .
-- For information about using AWS Lambda actions in receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-lambda.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkLambdaAction' smart constructor.
data LambdaAction = LambdaAction'
  { invocationType ::
      Lude.Maybe InvocationType,
    topicARN :: Lude.Maybe Lude.Text,
    functionARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LambdaAction' with the minimum fields required to make a request.
--
-- * 'functionARN' - The Amazon Resource Name (ARN) of the AWS Lambda function. An example of an AWS Lambda function ARN is @arn:aws:lambda:us-west-2:account-id:function:MyFunction@ . For more information about AWS Lambda, see the <https://docs.aws.amazon.com/lambda/latest/dg/welcome.html AWS Lambda Developer Guide> .
-- * 'invocationType' - The invocation type of the AWS Lambda function. An invocation type of @RequestResponse@ means that the execution of the function will immediately result in a response, and a value of @Event@ means that the function will be invoked asynchronously. The default value is @Event@ . For information about AWS Lambda invocation types, see the <https://docs.aws.amazon.com/lambda/latest/dg/API_Invoke.html AWS Lambda Developer Guide> .
--
-- /Important:/ There is a 30-second timeout on @RequestResponse@ invocations. You should use @Event@ invocation in most cases. Use @RequestResponse@ only when you want to make a mail flow decision, such as whether to stop the receipt rule or the receipt rule set.
-- * 'topicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the Lambda action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
mkLambdaAction ::
  -- | 'functionARN'
  Lude.Text ->
  LambdaAction
mkLambdaAction pFunctionARN_ =
  LambdaAction'
    { invocationType = Lude.Nothing,
      topicARN = Lude.Nothing,
      functionARN = pFunctionARN_
    }

-- | The invocation type of the AWS Lambda function. An invocation type of @RequestResponse@ means that the execution of the function will immediately result in a response, and a value of @Event@ means that the function will be invoked asynchronously. The default value is @Event@ . For information about AWS Lambda invocation types, see the <https://docs.aws.amazon.com/lambda/latest/dg/API_Invoke.html AWS Lambda Developer Guide> .
--
-- /Important:/ There is a 30-second timeout on @RequestResponse@ invocations. You should use @Event@ invocation in most cases. Use @RequestResponse@ only when you want to make a mail flow decision, such as whether to stop the receipt rule or the receipt rule set.
--
-- /Note:/ Consider using 'invocationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laInvocationType :: Lens.Lens' LambdaAction (Lude.Maybe InvocationType)
laInvocationType = Lens.lens (invocationType :: LambdaAction -> Lude.Maybe InvocationType) (\s a -> s {invocationType = a} :: LambdaAction)
{-# DEPRECATED laInvocationType "Use generic-lens or generic-optics with 'invocationType' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the Lambda action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- /Note:/ Consider using 'topicARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laTopicARN :: Lens.Lens' LambdaAction (Lude.Maybe Lude.Text)
laTopicARN = Lens.lens (topicARN :: LambdaAction -> Lude.Maybe Lude.Text) (\s a -> s {topicARN = a} :: LambdaAction)
{-# DEPRECATED laTopicARN "Use generic-lens or generic-optics with 'topicARN' instead." #-}

-- | The Amazon Resource Name (ARN) of the AWS Lambda function. An example of an AWS Lambda function ARN is @arn:aws:lambda:us-west-2:account-id:function:MyFunction@ . For more information about AWS Lambda, see the <https://docs.aws.amazon.com/lambda/latest/dg/welcome.html AWS Lambda Developer Guide> .
--
-- /Note:/ Consider using 'functionARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laFunctionARN :: Lens.Lens' LambdaAction Lude.Text
laFunctionARN = Lens.lens (functionARN :: LambdaAction -> Lude.Text) (\s a -> s {functionARN = a} :: LambdaAction)
{-# DEPRECATED laFunctionARN "Use generic-lens or generic-optics with 'functionARN' instead." #-}

instance Lude.FromXML LambdaAction where
  parseXML x =
    LambdaAction'
      Lude.<$> (x Lude..@? "InvocationType")
      Lude.<*> (x Lude..@? "TopicArn")
      Lude.<*> (x Lude..@ "FunctionArn")

instance Lude.ToQuery LambdaAction where
  toQuery LambdaAction' {..} =
    Lude.mconcat
      [ "InvocationType" Lude.=: invocationType,
        "TopicArn" Lude.=: topicARN,
        "FunctionArn" Lude.=: functionARN
      ]
