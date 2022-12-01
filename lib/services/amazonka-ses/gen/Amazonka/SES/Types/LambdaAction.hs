{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SES.Types.LambdaAction
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.LambdaAction where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SES.Types.InvocationType

-- | When included in a receipt rule, this action calls an AWS Lambda
-- function and, optionally, publishes a notification to Amazon Simple
-- Notification Service (Amazon SNS).
--
-- To enable Amazon SES to call your AWS Lambda function or to publish to
-- an Amazon SNS topic of another account, Amazon SES must have permission
-- to access those resources. For information about giving permissions, see
-- the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-permissions.html Amazon SES Developer Guide>.
--
-- For information about using AWS Lambda actions in receipt rules, see the
-- <https://docs.aws.amazon.com/ses/latest/DeveloperGuide/receiving-email-action-lambda.html Amazon SES Developer Guide>.
--
-- /See:/ 'newLambdaAction' smart constructor.
data LambdaAction = LambdaAction'
  { -- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when
    -- the Lambda action is taken. An example of an Amazon SNS topic ARN is
    -- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
    -- Amazon SNS topics, see the
    -- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
    topicArn :: Prelude.Maybe Prelude.Text,
    -- | The invocation type of the AWS Lambda function. An invocation type of
    -- @RequestResponse@ means that the execution of the function will
    -- immediately result in a response, and a value of @Event@ means that the
    -- function will be invoked asynchronously. The default value is @Event@.
    -- For information about AWS Lambda invocation types, see the
    -- <https://docs.aws.amazon.com/lambda/latest/dg/API_Invoke.html AWS Lambda Developer Guide>.
    --
    -- There is a 30-second timeout on @RequestResponse@ invocations. You
    -- should use @Event@ invocation in most cases. Use @RequestResponse@ only
    -- when you want to make a mail flow decision, such as whether to stop the
    -- receipt rule or the receipt rule set.
    invocationType :: Prelude.Maybe InvocationType,
    -- | The Amazon Resource Name (ARN) of the AWS Lambda function. An example of
    -- an AWS Lambda function ARN is
    -- @arn:aws:lambda:us-west-2:account-id:function:MyFunction@. For more
    -- information about AWS Lambda, see the
    -- <https://docs.aws.amazon.com/lambda/latest/dg/welcome.html AWS Lambda Developer Guide>.
    functionArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LambdaAction' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'topicArn', 'lambdaAction_topicArn' - The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when
-- the Lambda action is taken. An example of an Amazon SNS topic ARN is
-- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
-- Amazon SNS topics, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
--
-- 'invocationType', 'lambdaAction_invocationType' - The invocation type of the AWS Lambda function. An invocation type of
-- @RequestResponse@ means that the execution of the function will
-- immediately result in a response, and a value of @Event@ means that the
-- function will be invoked asynchronously. The default value is @Event@.
-- For information about AWS Lambda invocation types, see the
-- <https://docs.aws.amazon.com/lambda/latest/dg/API_Invoke.html AWS Lambda Developer Guide>.
--
-- There is a 30-second timeout on @RequestResponse@ invocations. You
-- should use @Event@ invocation in most cases. Use @RequestResponse@ only
-- when you want to make a mail flow decision, such as whether to stop the
-- receipt rule or the receipt rule set.
--
-- 'functionArn', 'lambdaAction_functionArn' - The Amazon Resource Name (ARN) of the AWS Lambda function. An example of
-- an AWS Lambda function ARN is
-- @arn:aws:lambda:us-west-2:account-id:function:MyFunction@. For more
-- information about AWS Lambda, see the
-- <https://docs.aws.amazon.com/lambda/latest/dg/welcome.html AWS Lambda Developer Guide>.
newLambdaAction ::
  -- | 'functionArn'
  Prelude.Text ->
  LambdaAction
newLambdaAction pFunctionArn_ =
  LambdaAction'
    { topicArn = Prelude.Nothing,
      invocationType = Prelude.Nothing,
      functionArn = pFunctionArn_
    }

-- | The Amazon Resource Name (ARN) of the Amazon SNS topic to notify when
-- the Lambda action is taken. An example of an Amazon SNS topic ARN is
-- @arn:aws:sns:us-west-2:123456789012:MyTopic@. For more information about
-- Amazon SNS topics, see the
-- <https://docs.aws.amazon.com/sns/latest/dg/CreateTopic.html Amazon SNS Developer Guide>.
lambdaAction_topicArn :: Lens.Lens' LambdaAction (Prelude.Maybe Prelude.Text)
lambdaAction_topicArn = Lens.lens (\LambdaAction' {topicArn} -> topicArn) (\s@LambdaAction' {} a -> s {topicArn = a} :: LambdaAction)

-- | The invocation type of the AWS Lambda function. An invocation type of
-- @RequestResponse@ means that the execution of the function will
-- immediately result in a response, and a value of @Event@ means that the
-- function will be invoked asynchronously. The default value is @Event@.
-- For information about AWS Lambda invocation types, see the
-- <https://docs.aws.amazon.com/lambda/latest/dg/API_Invoke.html AWS Lambda Developer Guide>.
--
-- There is a 30-second timeout on @RequestResponse@ invocations. You
-- should use @Event@ invocation in most cases. Use @RequestResponse@ only
-- when you want to make a mail flow decision, such as whether to stop the
-- receipt rule or the receipt rule set.
lambdaAction_invocationType :: Lens.Lens' LambdaAction (Prelude.Maybe InvocationType)
lambdaAction_invocationType = Lens.lens (\LambdaAction' {invocationType} -> invocationType) (\s@LambdaAction' {} a -> s {invocationType = a} :: LambdaAction)

-- | The Amazon Resource Name (ARN) of the AWS Lambda function. An example of
-- an AWS Lambda function ARN is
-- @arn:aws:lambda:us-west-2:account-id:function:MyFunction@. For more
-- information about AWS Lambda, see the
-- <https://docs.aws.amazon.com/lambda/latest/dg/welcome.html AWS Lambda Developer Guide>.
lambdaAction_functionArn :: Lens.Lens' LambdaAction Prelude.Text
lambdaAction_functionArn = Lens.lens (\LambdaAction' {functionArn} -> functionArn) (\s@LambdaAction' {} a -> s {functionArn = a} :: LambdaAction)

instance Core.FromXML LambdaAction where
  parseXML x =
    LambdaAction'
      Prelude.<$> (x Core..@? "TopicArn")
      Prelude.<*> (x Core..@? "InvocationType")
      Prelude.<*> (x Core..@ "FunctionArn")

instance Prelude.Hashable LambdaAction where
  hashWithSalt _salt LambdaAction' {..} =
    _salt `Prelude.hashWithSalt` topicArn
      `Prelude.hashWithSalt` invocationType
      `Prelude.hashWithSalt` functionArn

instance Prelude.NFData LambdaAction where
  rnf LambdaAction' {..} =
    Prelude.rnf topicArn
      `Prelude.seq` Prelude.rnf invocationType
      `Prelude.seq` Prelude.rnf functionArn

instance Core.ToQuery LambdaAction where
  toQuery LambdaAction' {..} =
    Prelude.mconcat
      [ "TopicArn" Core.=: topicArn,
        "InvocationType" Core.=: invocationType,
        "FunctionArn" Core.=: functionArn
      ]
