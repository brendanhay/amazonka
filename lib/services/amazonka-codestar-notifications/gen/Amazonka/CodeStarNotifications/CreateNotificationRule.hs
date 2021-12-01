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
-- Module      : Amazonka.CodeStarNotifications.CreateNotificationRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a notification rule for a resource. The rule specifies the
-- events you want notifications about and the targets (such as SNS topics)
-- where you want to receive them.
module Amazonka.CodeStarNotifications.CreateNotificationRule
  ( -- * Creating a Request
    CreateNotificationRule (..),
    newCreateNotificationRule,

    -- * Request Lenses
    createNotificationRule_status,
    createNotificationRule_clientRequestToken,
    createNotificationRule_tags,
    createNotificationRule_name,
    createNotificationRule_eventTypeIds,
    createNotificationRule_resource,
    createNotificationRule_targets,
    createNotificationRule_detailType,

    -- * Destructuring the Response
    CreateNotificationRuleResponse (..),
    newCreateNotificationRuleResponse,

    -- * Response Lenses
    createNotificationRuleResponse_arn,
    createNotificationRuleResponse_httpStatus,
  )
where

import Amazonka.CodeStarNotifications.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateNotificationRule' smart constructor.
data CreateNotificationRule = CreateNotificationRule'
  { -- | The status of the notification rule. The default value is ENABLED. If
    -- the status is set to DISABLED, notifications aren\'t sent for the
    -- notification rule.
    status :: Prelude.Maybe NotificationRuleStatus,
    -- | A unique, client-generated idempotency token that, when provided in a
    -- request, ensures the request cannot be repeated with a changed
    -- parameter. If a request with the same parameters is received and a token
    -- is included, the request returns information about the initial request
    -- that used that token.
    --
    -- The AWS SDKs prepopulate client request tokens. If you are using an AWS
    -- SDK, an idempotency token is created for you.
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | A list of tags to apply to this notification rule. Key names cannot
    -- start with \"aws\".
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name for the notification rule. Notifictaion rule names must be
    -- unique in your AWS account.
    name :: Core.Sensitive Prelude.Text,
    -- | A list of event types associated with this notification rule. For a list
    -- of allowed events, see EventTypeSummary.
    eventTypeIds :: [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the resource to associate with the
    -- notification rule. Supported resources include pipelines in AWS
    -- CodePipeline, repositories in AWS CodeCommit, and build projects in AWS
    -- CodeBuild.
    resource :: Prelude.Text,
    -- | A list of Amazon Resource Names (ARNs) of SNS topics to associate with
    -- the notification rule.
    targets :: [Target],
    -- | The level of detail to include in the notifications for this resource.
    -- BASIC will include only the contents of the event as it would appear in
    -- AWS CloudWatch. FULL will include any supplemental information provided
    -- by AWS CodeStar Notifications and\/or the service for the resource for
    -- which the notification is created.
    detailType :: DetailType
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNotificationRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'createNotificationRule_status' - The status of the notification rule. The default value is ENABLED. If
-- the status is set to DISABLED, notifications aren\'t sent for the
-- notification rule.
--
-- 'clientRequestToken', 'createNotificationRule_clientRequestToken' - A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request with the same parameters is received and a token
-- is included, the request returns information about the initial request
-- that used that token.
--
-- The AWS SDKs prepopulate client request tokens. If you are using an AWS
-- SDK, an idempotency token is created for you.
--
-- 'tags', 'createNotificationRule_tags' - A list of tags to apply to this notification rule. Key names cannot
-- start with \"aws\".
--
-- 'name', 'createNotificationRule_name' - The name for the notification rule. Notifictaion rule names must be
-- unique in your AWS account.
--
-- 'eventTypeIds', 'createNotificationRule_eventTypeIds' - A list of event types associated with this notification rule. For a list
-- of allowed events, see EventTypeSummary.
--
-- 'resource', 'createNotificationRule_resource' - The Amazon Resource Name (ARN) of the resource to associate with the
-- notification rule. Supported resources include pipelines in AWS
-- CodePipeline, repositories in AWS CodeCommit, and build projects in AWS
-- CodeBuild.
--
-- 'targets', 'createNotificationRule_targets' - A list of Amazon Resource Names (ARNs) of SNS topics to associate with
-- the notification rule.
--
-- 'detailType', 'createNotificationRule_detailType' - The level of detail to include in the notifications for this resource.
-- BASIC will include only the contents of the event as it would appear in
-- AWS CloudWatch. FULL will include any supplemental information provided
-- by AWS CodeStar Notifications and\/or the service for the resource for
-- which the notification is created.
newCreateNotificationRule ::
  -- | 'name'
  Prelude.Text ->
  -- | 'resource'
  Prelude.Text ->
  -- | 'detailType'
  DetailType ->
  CreateNotificationRule
newCreateNotificationRule
  pName_
  pResource_
  pDetailType_ =
    CreateNotificationRule'
      { status = Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = Core._Sensitive Lens.# pName_,
        eventTypeIds = Prelude.mempty,
        resource = pResource_,
        targets = Prelude.mempty,
        detailType = pDetailType_
      }

-- | The status of the notification rule. The default value is ENABLED. If
-- the status is set to DISABLED, notifications aren\'t sent for the
-- notification rule.
createNotificationRule_status :: Lens.Lens' CreateNotificationRule (Prelude.Maybe NotificationRuleStatus)
createNotificationRule_status = Lens.lens (\CreateNotificationRule' {status} -> status) (\s@CreateNotificationRule' {} a -> s {status = a} :: CreateNotificationRule)

-- | A unique, client-generated idempotency token that, when provided in a
-- request, ensures the request cannot be repeated with a changed
-- parameter. If a request with the same parameters is received and a token
-- is included, the request returns information about the initial request
-- that used that token.
--
-- The AWS SDKs prepopulate client request tokens. If you are using an AWS
-- SDK, an idempotency token is created for you.
createNotificationRule_clientRequestToken :: Lens.Lens' CreateNotificationRule (Prelude.Maybe Prelude.Text)
createNotificationRule_clientRequestToken = Lens.lens (\CreateNotificationRule' {clientRequestToken} -> clientRequestToken) (\s@CreateNotificationRule' {} a -> s {clientRequestToken = a} :: CreateNotificationRule)

-- | A list of tags to apply to this notification rule. Key names cannot
-- start with \"aws\".
createNotificationRule_tags :: Lens.Lens' CreateNotificationRule (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createNotificationRule_tags = Lens.lens (\CreateNotificationRule' {tags} -> tags) (\s@CreateNotificationRule' {} a -> s {tags = a} :: CreateNotificationRule) Prelude.. Lens.mapping Lens.coerced

-- | The name for the notification rule. Notifictaion rule names must be
-- unique in your AWS account.
createNotificationRule_name :: Lens.Lens' CreateNotificationRule Prelude.Text
createNotificationRule_name = Lens.lens (\CreateNotificationRule' {name} -> name) (\s@CreateNotificationRule' {} a -> s {name = a} :: CreateNotificationRule) Prelude.. Core._Sensitive

-- | A list of event types associated with this notification rule. For a list
-- of allowed events, see EventTypeSummary.
createNotificationRule_eventTypeIds :: Lens.Lens' CreateNotificationRule [Prelude.Text]
createNotificationRule_eventTypeIds = Lens.lens (\CreateNotificationRule' {eventTypeIds} -> eventTypeIds) (\s@CreateNotificationRule' {} a -> s {eventTypeIds = a} :: CreateNotificationRule) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of the resource to associate with the
-- notification rule. Supported resources include pipelines in AWS
-- CodePipeline, repositories in AWS CodeCommit, and build projects in AWS
-- CodeBuild.
createNotificationRule_resource :: Lens.Lens' CreateNotificationRule Prelude.Text
createNotificationRule_resource = Lens.lens (\CreateNotificationRule' {resource} -> resource) (\s@CreateNotificationRule' {} a -> s {resource = a} :: CreateNotificationRule)

-- | A list of Amazon Resource Names (ARNs) of SNS topics to associate with
-- the notification rule.
createNotificationRule_targets :: Lens.Lens' CreateNotificationRule [Target]
createNotificationRule_targets = Lens.lens (\CreateNotificationRule' {targets} -> targets) (\s@CreateNotificationRule' {} a -> s {targets = a} :: CreateNotificationRule) Prelude.. Lens.coerced

-- | The level of detail to include in the notifications for this resource.
-- BASIC will include only the contents of the event as it would appear in
-- AWS CloudWatch. FULL will include any supplemental information provided
-- by AWS CodeStar Notifications and\/or the service for the resource for
-- which the notification is created.
createNotificationRule_detailType :: Lens.Lens' CreateNotificationRule DetailType
createNotificationRule_detailType = Lens.lens (\CreateNotificationRule' {detailType} -> detailType) (\s@CreateNotificationRule' {} a -> s {detailType = a} :: CreateNotificationRule)

instance Core.AWSRequest CreateNotificationRule where
  type
    AWSResponse CreateNotificationRule =
      CreateNotificationRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateNotificationRuleResponse'
            Prelude.<$> (x Core..?> "Arn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateNotificationRule where
  hashWithSalt salt' CreateNotificationRule' {..} =
    salt' `Prelude.hashWithSalt` detailType
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` resource
      `Prelude.hashWithSalt` eventTypeIds
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` status

instance Prelude.NFData CreateNotificationRule where
  rnf CreateNotificationRule' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf detailType
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf eventTypeIds
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientRequestToken

instance Core.ToHeaders CreateNotificationRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateNotificationRule where
  toJSON CreateNotificationRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("ClientRequestToken" Core..=)
              Prelude.<$> clientRequestToken,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("EventTypeIds" Core..= eventTypeIds),
            Prelude.Just ("Resource" Core..= resource),
            Prelude.Just ("Targets" Core..= targets),
            Prelude.Just ("DetailType" Core..= detailType)
          ]
      )

instance Core.ToPath CreateNotificationRule where
  toPath = Prelude.const "/createNotificationRule"

instance Core.ToQuery CreateNotificationRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateNotificationRuleResponse' smart constructor.
data CreateNotificationRuleResponse = CreateNotificationRuleResponse'
  { -- | The Amazon Resource Name (ARN) of the notification rule.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateNotificationRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createNotificationRuleResponse_arn' - The Amazon Resource Name (ARN) of the notification rule.
--
-- 'httpStatus', 'createNotificationRuleResponse_httpStatus' - The response's http status code.
newCreateNotificationRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateNotificationRuleResponse
newCreateNotificationRuleResponse pHttpStatus_ =
  CreateNotificationRuleResponse'
    { arn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the notification rule.
createNotificationRuleResponse_arn :: Lens.Lens' CreateNotificationRuleResponse (Prelude.Maybe Prelude.Text)
createNotificationRuleResponse_arn = Lens.lens (\CreateNotificationRuleResponse' {arn} -> arn) (\s@CreateNotificationRuleResponse' {} a -> s {arn = a} :: CreateNotificationRuleResponse)

-- | The response's http status code.
createNotificationRuleResponse_httpStatus :: Lens.Lens' CreateNotificationRuleResponse Prelude.Int
createNotificationRuleResponse_httpStatus = Lens.lens (\CreateNotificationRuleResponse' {httpStatus} -> httpStatus) (\s@CreateNotificationRuleResponse' {} a -> s {httpStatus = a} :: CreateNotificationRuleResponse)

instance
  Prelude.NFData
    CreateNotificationRuleResponse
  where
  rnf CreateNotificationRuleResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf httpStatus
