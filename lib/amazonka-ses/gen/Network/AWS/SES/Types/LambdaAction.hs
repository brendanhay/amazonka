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
    laFunctionArn,
    laInvocationType,
    laTopicArn,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.FunctionArn as Types
import qualified Network.AWS.SES.Types.InvocationType as Types
import qualified Network.AWS.SES.Types.TopicArn as Types

-- | When included in a receipt rule, this action calls an AWS Lambda function and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
--
-- To enable Amazon SES to call your AWS Lambda function or to publish to an Amazon SNS topic of another account, Amazon SES must have permission to access those resources. For information about giving permissions, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> .
-- For information about using AWS Lambda actions in receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-lambda.html Amazon SES Developer Guide> .
--
-- /See:/ 'mkLambdaAction' smart constructor.
data LambdaAction = LambdaAction'
  { -- | The Amazon Resource Name (ARN) of the AWS Lambda function. An example of an AWS Lambda function ARN is @arn:aws:lambda:us-west-2:account-id:function:MyFunction@ . For more information about AWS Lambda, see the <https://docs.aws.amazon.com/lambda/latest/dg/welcome.html AWS Lambda Developer Guide> .
    functionArn :: Types.FunctionArn,
    -- | The invocation type of the AWS Lambda function. An invocation type of @RequestResponse@ means that the execution of the function will immediately result in a response, and a value of @Event@ means that the function will be invoked asynchronously. The default value is @Event@ . For information about AWS Lambda invocation types, see the <https://docs.aws.amazon.com/lambda/latest/dg/API_Invoke.html AWS Lambda Developer Guide> .
    --
    -- /Important:/ There is a 30-second timeout on @RequestResponse@ invocations. You should use @Event@ invocation in most cases. Use @RequestResponse@ only when you want to make a mail flow decision, such as whether to stop the receipt rule or the receipt rule set.
    invocationType :: Core.Maybe Types.InvocationType,
    -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the Lambda action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
    topicArn :: Core.Maybe Types.TopicArn
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LambdaAction' value with any optional fields omitted.
mkLambdaAction ::
  -- | 'functionArn'
  Types.FunctionArn ->
  LambdaAction
mkLambdaAction functionArn =
  LambdaAction'
    { functionArn,
      invocationType = Core.Nothing,
      topicArn = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the AWS Lambda function. An example of an AWS Lambda function ARN is @arn:aws:lambda:us-west-2:account-id:function:MyFunction@ . For more information about AWS Lambda, see the <https://docs.aws.amazon.com/lambda/latest/dg/welcome.html AWS Lambda Developer Guide> .
--
-- /Note:/ Consider using 'functionArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laFunctionArn :: Lens.Lens' LambdaAction Types.FunctionArn
laFunctionArn = Lens.field @"functionArn"
{-# DEPRECATED laFunctionArn "Use generic-lens or generic-optics with 'functionArn' instead." #-}

-- | The invocation type of the AWS Lambda function. An invocation type of @RequestResponse@ means that the execution of the function will immediately result in a response, and a value of @Event@ means that the function will be invoked asynchronously. The default value is @Event@ . For information about AWS Lambda invocation types, see the <https://docs.aws.amazon.com/lambda/latest/dg/API_Invoke.html AWS Lambda Developer Guide> .
--
-- /Important:/ There is a 30-second timeout on @RequestResponse@ invocations. You should use @Event@ invocation in most cases. Use @RequestResponse@ only when you want to make a mail flow decision, such as whether to stop the receipt rule or the receipt rule set.
--
-- /Note:/ Consider using 'invocationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laInvocationType :: Lens.Lens' LambdaAction (Core.Maybe Types.InvocationType)
laInvocationType = Lens.field @"invocationType"
{-# DEPRECATED laInvocationType "Use generic-lens or generic-optics with 'invocationType' instead." #-}

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the Lambda action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- /Note:/ Consider using 'topicArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
laTopicArn :: Lens.Lens' LambdaAction (Core.Maybe Types.TopicArn)
laTopicArn = Lens.field @"topicArn"
{-# DEPRECATED laTopicArn "Use generic-lens or generic-optics with 'topicArn' instead." #-}

instance Core.FromXML LambdaAction where
  parseXML x =
    LambdaAction'
      Core.<$> (x Core..@ "FunctionArn")
      Core.<*> (x Core..@? "InvocationType")
      Core.<*> (x Core..@? "TopicArn")
