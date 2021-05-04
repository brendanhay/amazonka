{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchEvents.PutTargets
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds the specified targets to the specified rule, or updates the targets
-- if they are already associated with the rule.
--
-- Targets are the resources that are invoked when a rule is triggered.
--
-- You can configure the following as targets for Events:
--
-- -   EC2 instances
--
-- -   SSM Run Command
--
-- -   SSM Automation
--
-- -   AWS Lambda functions
--
-- -   Data streams in Amazon Kinesis Data Streams
--
-- -   Data delivery streams in Amazon Kinesis Data Firehose
--
-- -   Amazon ECS tasks
--
-- -   AWS Step Functions state machines
--
-- -   AWS Batch jobs
--
-- -   AWS CodeBuild projects
--
-- -   Pipelines in AWS CodePipeline
--
-- -   Amazon Inspector assessment templates
--
-- -   Amazon SNS topics
--
-- -   Amazon SQS queues, including FIFO queues
--
-- -   The default event bus of another AWS account
--
-- -   Amazon API Gateway REST APIs
--
-- -   Redshift Clusters to invoke Data API ExecuteStatement on
--
-- -   Custom\/SaaS HTTPS APIs via EventBridge API Destinations
--
-- Creating rules with built-in targets is supported only in the AWS
-- Management Console. The built-in targets are
-- @EC2 CreateSnapshot API call@, @EC2 RebootInstances API call@,
-- @EC2 StopInstances API call@, and @EC2 TerminateInstances API call@.
--
-- For some target types, @PutTargets@ provides target-specific parameters.
-- If the target is a Kinesis data stream, you can optionally specify which
-- shard the event goes to by using the @KinesisParameters@ argument. To
-- invoke a command on multiple EC2 instances with one rule, you can use
-- the @RunCommandParameters@ field.
--
-- To be able to make API calls against the resources that you own, Amazon
-- EventBridge (CloudWatch Events) needs the appropriate permissions. For
-- AWS Lambda and Amazon SNS resources, EventBridge relies on
-- resource-based policies. For EC2 instances, Kinesis data streams, AWS
-- Step Functions state machines and API Gateway REST APIs, EventBridge
-- relies on IAM roles that you specify in the @RoleARN@ argument in
-- @PutTargets@. For more information, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/auth-and-access-control-eventbridge.html Authentication and Access Control>
-- in the /Amazon EventBridge User Guide/.
--
-- If another AWS account is in the same region and has granted you
-- permission (using @PutPermission@), you can send events to that account.
-- Set that account\'s event bus as a target of the rules in your account.
-- To send the matched events to the other account, specify that account\'s
-- event bus as the @Arn@ value when you run @PutTargets@. If your account
-- sends events to another account, your account is charged for each sent
-- event. Each event sent to another account is charged as a custom event.
-- The account receiving the event is not charged. For more information,
-- see
-- <https://aws.amazon.com/eventbridge/pricing/ Amazon EventBridge (CloudWatch Events) Pricing>.
--
-- @Input@, @InputPath@, and @InputTransformer@ are not available with
-- @PutTarget@ if the target is an event bus of a different AWS account.
--
-- If you are setting the event bus of another account as the target, and
-- that account granted permission to your account through an organization
-- instead of directly by the account ID, then you must specify a @RoleArn@
-- with proper permissions in the @Target@ structure. For more information,
-- see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eventbridge-cross-account-event-delivery.html Sending and Receiving Events Between AWS Accounts>
-- in the /Amazon EventBridge User Guide/.
--
-- For more information about enabling cross-account events, see
-- PutPermission.
--
-- __Input__, __InputPath__, and __InputTransformer__ are mutually
-- exclusive and optional parameters of a target. When a rule is triggered
-- due to a matched event:
--
-- -   If none of the following arguments are specified for a target, then
--     the entire event is passed to the target in JSON format (unless the
--     target is Amazon EC2 Run Command or Amazon ECS task, in which case
--     nothing from the event is passed to the target).
--
-- -   If __Input__ is specified in the form of valid JSON, then the
--     matched event is overridden with this constant.
--
-- -   If __InputPath__ is specified in the form of JSONPath (for example,
--     @$.detail@), then only the part of the event specified in the path
--     is passed to the target (for example, only the detail part of the
--     event is passed).
--
-- -   If __InputTransformer__ is specified, then one or more specified
--     JSONPaths are extracted from the event and used as values in a
--     template that you specify as the input to the target.
--
-- When you specify @InputPath@ or @InputTransformer@, you must use JSON
-- dot notation, not bracket notation.
--
-- When you add targets to a rule and the associated rule triggers soon
-- after, new or updated targets might not be immediately invoked. Allow a
-- short period of time for changes to take effect.
--
-- This action can partially fail if too many requests are made at the same
-- time. If that happens, @FailedEntryCount@ is non-zero in the response
-- and each entry in @FailedEntries@ provides the ID of the failed target
-- and the error code.
module Network.AWS.CloudWatchEvents.PutTargets
  ( -- * Creating a Request
    PutTargets (..),
    newPutTargets,

    -- * Request Lenses
    putTargets_eventBusName,
    putTargets_rule,
    putTargets_targets,

    -- * Destructuring the Response
    PutTargetsResponse (..),
    newPutTargetsResponse,

    -- * Response Lenses
    putTargetsResponse_failedEntryCount,
    putTargetsResponse_failedEntries,
    putTargetsResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newPutTargets' smart constructor.
data PutTargets = PutTargets'
  { -- | The name or ARN of the event bus associated with the rule. If you omit
    -- this, the default event bus is used.
    eventBusName :: Prelude.Maybe Prelude.Text,
    -- | The name of the rule.
    rule :: Prelude.Text,
    -- | The targets to update or add to the rule.
    targets :: Prelude.NonEmpty Target
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutTargets' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eventBusName', 'putTargets_eventBusName' - The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
--
-- 'rule', 'putTargets_rule' - The name of the rule.
--
-- 'targets', 'putTargets_targets' - The targets to update or add to the rule.
newPutTargets ::
  -- | 'rule'
  Prelude.Text ->
  -- | 'targets'
  Prelude.NonEmpty Target ->
  PutTargets
newPutTargets pRule_ pTargets_ =
  PutTargets'
    { eventBusName = Prelude.Nothing,
      rule = pRule_,
      targets = Prelude._Coerce Lens.# pTargets_
    }

-- | The name or ARN of the event bus associated with the rule. If you omit
-- this, the default event bus is used.
putTargets_eventBusName :: Lens.Lens' PutTargets (Prelude.Maybe Prelude.Text)
putTargets_eventBusName = Lens.lens (\PutTargets' {eventBusName} -> eventBusName) (\s@PutTargets' {} a -> s {eventBusName = a} :: PutTargets)

-- | The name of the rule.
putTargets_rule :: Lens.Lens' PutTargets Prelude.Text
putTargets_rule = Lens.lens (\PutTargets' {rule} -> rule) (\s@PutTargets' {} a -> s {rule = a} :: PutTargets)

-- | The targets to update or add to the rule.
putTargets_targets :: Lens.Lens' PutTargets (Prelude.NonEmpty Target)
putTargets_targets = Lens.lens (\PutTargets' {targets} -> targets) (\s@PutTargets' {} a -> s {targets = a} :: PutTargets) Prelude.. Prelude._Coerce

instance Prelude.AWSRequest PutTargets where
  type Rs PutTargets = PutTargetsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          PutTargetsResponse'
            Prelude.<$> (x Prelude..?> "FailedEntryCount")
            Prelude.<*> ( x Prelude..?> "FailedEntries"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutTargets

instance Prelude.NFData PutTargets

instance Prelude.ToHeaders PutTargets where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSEvents.PutTargets" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutTargets where
  toJSON PutTargets' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EventBusName" Prelude..=)
              Prelude.<$> eventBusName,
            Prelude.Just ("Rule" Prelude..= rule),
            Prelude.Just ("Targets" Prelude..= targets)
          ]
      )

instance Prelude.ToPath PutTargets where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutTargets where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutTargetsResponse' smart constructor.
data PutTargetsResponse = PutTargetsResponse'
  { -- | The number of failed entries.
    failedEntryCount :: Prelude.Maybe Prelude.Int,
    -- | The failed target entries.
    failedEntries :: Prelude.Maybe [PutTargetsResultEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutTargetsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failedEntryCount', 'putTargetsResponse_failedEntryCount' - The number of failed entries.
--
-- 'failedEntries', 'putTargetsResponse_failedEntries' - The failed target entries.
--
-- 'httpStatus', 'putTargetsResponse_httpStatus' - The response's http status code.
newPutTargetsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutTargetsResponse
newPutTargetsResponse pHttpStatus_ =
  PutTargetsResponse'
    { failedEntryCount =
        Prelude.Nothing,
      failedEntries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The number of failed entries.
putTargetsResponse_failedEntryCount :: Lens.Lens' PutTargetsResponse (Prelude.Maybe Prelude.Int)
putTargetsResponse_failedEntryCount = Lens.lens (\PutTargetsResponse' {failedEntryCount} -> failedEntryCount) (\s@PutTargetsResponse' {} a -> s {failedEntryCount = a} :: PutTargetsResponse)

-- | The failed target entries.
putTargetsResponse_failedEntries :: Lens.Lens' PutTargetsResponse (Prelude.Maybe [PutTargetsResultEntry])
putTargetsResponse_failedEntries = Lens.lens (\PutTargetsResponse' {failedEntries} -> failedEntries) (\s@PutTargetsResponse' {} a -> s {failedEntries = a} :: PutTargetsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
putTargetsResponse_httpStatus :: Lens.Lens' PutTargetsResponse Prelude.Int
putTargetsResponse_httpStatus = Lens.lens (\PutTargetsResponse' {httpStatus} -> httpStatus) (\s@PutTargetsResponse' {} a -> s {httpStatus = a} :: PutTargetsResponse)

instance Prelude.NFData PutTargetsResponse
