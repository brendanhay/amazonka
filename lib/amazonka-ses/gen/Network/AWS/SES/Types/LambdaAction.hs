{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.LambdaAction
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.LambdaAction where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SES.Types.InvocationType

-- | When included in a receipt rule, this action calls an AWS Lambda function and, optionally, publishes a notification to Amazon Simple Notification Service (Amazon SNS).
--
--
-- To enable Amazon SES to call your AWS Lambda function or to publish to an Amazon SNS topic of another account, Amazon SES must have permission to access those resources. For information about giving permissions, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide> .
--
-- For information about using AWS Lambda actions in receipt rules, see the <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-lambda.html Amazon SES Developer Guide> .
--
--
-- /See:/ 'lambdaAction' smart constructor.
data LambdaAction = LambdaAction'
  { _laInvocationType ::
      !(Maybe InvocationType),
    _laTopicARN :: !(Maybe Text),
    _laFunctionARN :: !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'LambdaAction' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'laInvocationType' - The invocation type of the AWS Lambda function. An invocation type of @RequestResponse@ means that the execution of the function will immediately result in a response, and a value of @Event@ means that the function will be invoked asynchronously. The default value is @Event@ . For information about AWS Lambda invocation types, see the <https://docs.aws.amazon.com/lambda/latest/dg/API_Invoke.html AWS Lambda Developer Guide> . /Important:/ There is a 30-second timeout on @RequestResponse@ invocations. You should use @Event@ invocation in most cases. Use @RequestResponse@ only when you want to make a mail flow decision, such as whether to stop the receipt rule or the receipt rule set.
--
-- * 'laTopicARN' - The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the Lambda action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
--
-- * 'laFunctionARN' - The Amazon Resource Name (ARN) of the AWS Lambda function. An example of an AWS Lambda function ARN is @arn:aws:lambda:us-west-2:account-id:function:MyFunction@ . For more information about AWS Lambda, see the <https://docs.aws.amazon.com/lambda/latest/dg/welcome.html AWS Lambda Developer Guide> .
lambdaAction ::
  -- | 'laFunctionARN'
  Text ->
  LambdaAction
lambdaAction pFunctionARN_ =
  LambdaAction'
    { _laInvocationType = Nothing,
      _laTopicARN = Nothing,
      _laFunctionARN = pFunctionARN_
    }

-- | The invocation type of the AWS Lambda function. An invocation type of @RequestResponse@ means that the execution of the function will immediately result in a response, and a value of @Event@ means that the function will be invoked asynchronously. The default value is @Event@ . For information about AWS Lambda invocation types, see the <https://docs.aws.amazon.com/lambda/latest/dg/API_Invoke.html AWS Lambda Developer Guide> . /Important:/ There is a 30-second timeout on @RequestResponse@ invocations. You should use @Event@ invocation in most cases. Use @RequestResponse@ only when you want to make a mail flow decision, such as whether to stop the receipt rule or the receipt rule set.
laInvocationType :: Lens' LambdaAction (Maybe InvocationType)
laInvocationType = lens _laInvocationType (\s a -> s {_laInvocationType = a})

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when the Lambda action is taken. An example of an Amazon SNS topic ARN is @arn:aws:sns:us-west-2:123456789012:MyTopic@ . For more information about Amazon SNS topics, see the <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide> .
laTopicARN :: Lens' LambdaAction (Maybe Text)
laTopicARN = lens _laTopicARN (\s a -> s {_laTopicARN = a})

-- | The Amazon Resource Name (ARN) of the AWS Lambda function. An example of an AWS Lambda function ARN is @arn:aws:lambda:us-west-2:account-id:function:MyFunction@ . For more information about AWS Lambda, see the <https://docs.aws.amazon.com/lambda/latest/dg/welcome.html AWS Lambda Developer Guide> .
laFunctionARN :: Lens' LambdaAction Text
laFunctionARN = lens _laFunctionARN (\s a -> s {_laFunctionARN = a})

instance FromXML LambdaAction where
  parseXML x =
    LambdaAction'
      <$> (x .@? "InvocationType")
      <*> (x .@? "TopicArn")
      <*> (x .@ "FunctionArn")

instance Hashable LambdaAction

instance NFData LambdaAction

instance ToQuery LambdaAction where
  toQuery LambdaAction' {..} =
    mconcat
      [ "InvocationType" =: _laInvocationType,
        "TopicArn" =: _laTopicARN,
        "FunctionArn" =: _laFunctionARN
      ]
