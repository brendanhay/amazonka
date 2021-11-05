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
-- Module      : Network.AWS.CodeStarNotifications.DescribeNotificationRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified notification rule.
module Network.AWS.CodeStarNotifications.DescribeNotificationRule
  ( -- * Creating a Request
    DescribeNotificationRule (..),
    newDescribeNotificationRule,

    -- * Request Lenses
    describeNotificationRule_arn,

    -- * Destructuring the Response
    DescribeNotificationRuleResponse (..),
    newDescribeNotificationRuleResponse,

    -- * Response Lenses
    describeNotificationRuleResponse_status,
    describeNotificationRuleResponse_eventTypes,
    describeNotificationRuleResponse_lastModifiedTimestamp,
    describeNotificationRuleResponse_createdBy,
    describeNotificationRuleResponse_detailType,
    describeNotificationRuleResponse_name,
    describeNotificationRuleResponse_targets,
    describeNotificationRuleResponse_resource,
    describeNotificationRuleResponse_createdTimestamp,
    describeNotificationRuleResponse_tags,
    describeNotificationRuleResponse_httpStatus,
    describeNotificationRuleResponse_arn,
  )
where

import Network.AWS.CodeStarNotifications.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeNotificationRule' smart constructor.
data DescribeNotificationRule = DescribeNotificationRule'
  { -- | The Amazon Resource Name (ARN) of the notification rule.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNotificationRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeNotificationRule_arn' - The Amazon Resource Name (ARN) of the notification rule.
newDescribeNotificationRule ::
  -- | 'arn'
  Prelude.Text ->
  DescribeNotificationRule
newDescribeNotificationRule pArn_ =
  DescribeNotificationRule' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the notification rule.
describeNotificationRule_arn :: Lens.Lens' DescribeNotificationRule Prelude.Text
describeNotificationRule_arn = Lens.lens (\DescribeNotificationRule' {arn} -> arn) (\s@DescribeNotificationRule' {} a -> s {arn = a} :: DescribeNotificationRule)

instance Core.AWSRequest DescribeNotificationRule where
  type
    AWSResponse DescribeNotificationRule =
      DescribeNotificationRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNotificationRuleResponse'
            Prelude.<$> (x Core..?> "Status")
            Prelude.<*> (x Core..?> "EventTypes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "LastModifiedTimestamp")
            Prelude.<*> (x Core..?> "CreatedBy")
            Prelude.<*> (x Core..?> "DetailType")
            Prelude.<*> (x Core..?> "Name")
            Prelude.<*> (x Core..?> "Targets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "Resource")
            Prelude.<*> (x Core..?> "CreatedTimestamp")
            Prelude.<*> (x Core..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "Arn")
      )

instance Prelude.Hashable DescribeNotificationRule

instance Prelude.NFData DescribeNotificationRule

instance Core.ToHeaders DescribeNotificationRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeNotificationRule where
  toJSON DescribeNotificationRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Arn" Core..= arn)]
      )

instance Core.ToPath DescribeNotificationRule where
  toPath = Prelude.const "/describeNotificationRule"

instance Core.ToQuery DescribeNotificationRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeNotificationRuleResponse' smart constructor.
data DescribeNotificationRuleResponse = DescribeNotificationRuleResponse'
  { -- | The status of the notification rule. Valid statuses are on (sending
    -- notifications) or off (not sending notifications).
    status :: Prelude.Maybe NotificationRuleStatus,
    -- | A list of the event types associated with the notification rule.
    eventTypes :: Prelude.Maybe [EventTypeSummary],
    -- | The date and time the notification rule was most recently updated, in
    -- timestamp format.
    lastModifiedTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The name or email alias of the person who created the notification rule.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The level of detail included in the notifications for this resource.
    -- BASIC will include only the contents of the event as it would appear in
    -- AWS CloudWatch. FULL will include any supplemental information provided
    -- by AWS CodeStar Notifications and\/or the service for the resource for
    -- which the notification is created.
    detailType :: Prelude.Maybe DetailType,
    -- | The name of the notification rule.
    name :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A list of the SNS topics associated with the notification rule.
    targets :: Prelude.Maybe [TargetSummary],
    -- | The Amazon Resource Name (ARN) of the resource associated with the
    -- notification rule.
    resource :: Prelude.Maybe Prelude.Text,
    -- | The date and time the notification rule was created, in timestamp
    -- format.
    createdTimestamp :: Prelude.Maybe Core.POSIX,
    -- | The tags associated with the notification rule.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the notification rule.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeNotificationRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'describeNotificationRuleResponse_status' - The status of the notification rule. Valid statuses are on (sending
-- notifications) or off (not sending notifications).
--
-- 'eventTypes', 'describeNotificationRuleResponse_eventTypes' - A list of the event types associated with the notification rule.
--
-- 'lastModifiedTimestamp', 'describeNotificationRuleResponse_lastModifiedTimestamp' - The date and time the notification rule was most recently updated, in
-- timestamp format.
--
-- 'createdBy', 'describeNotificationRuleResponse_createdBy' - The name or email alias of the person who created the notification rule.
--
-- 'detailType', 'describeNotificationRuleResponse_detailType' - The level of detail included in the notifications for this resource.
-- BASIC will include only the contents of the event as it would appear in
-- AWS CloudWatch. FULL will include any supplemental information provided
-- by AWS CodeStar Notifications and\/or the service for the resource for
-- which the notification is created.
--
-- 'name', 'describeNotificationRuleResponse_name' - The name of the notification rule.
--
-- 'targets', 'describeNotificationRuleResponse_targets' - A list of the SNS topics associated with the notification rule.
--
-- 'resource', 'describeNotificationRuleResponse_resource' - The Amazon Resource Name (ARN) of the resource associated with the
-- notification rule.
--
-- 'createdTimestamp', 'describeNotificationRuleResponse_createdTimestamp' - The date and time the notification rule was created, in timestamp
-- format.
--
-- 'tags', 'describeNotificationRuleResponse_tags' - The tags associated with the notification rule.
--
-- 'httpStatus', 'describeNotificationRuleResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'describeNotificationRuleResponse_arn' - The Amazon Resource Name (ARN) of the notification rule.
newDescribeNotificationRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  DescribeNotificationRuleResponse
newDescribeNotificationRuleResponse
  pHttpStatus_
  pArn_ =
    DescribeNotificationRuleResponse'
      { status =
          Prelude.Nothing,
        eventTypes = Prelude.Nothing,
        lastModifiedTimestamp = Prelude.Nothing,
        createdBy = Prelude.Nothing,
        detailType = Prelude.Nothing,
        name = Prelude.Nothing,
        targets = Prelude.Nothing,
        resource = Prelude.Nothing,
        createdTimestamp = Prelude.Nothing,
        tags = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_
      }

-- | The status of the notification rule. Valid statuses are on (sending
-- notifications) or off (not sending notifications).
describeNotificationRuleResponse_status :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe NotificationRuleStatus)
describeNotificationRuleResponse_status = Lens.lens (\DescribeNotificationRuleResponse' {status} -> status) (\s@DescribeNotificationRuleResponse' {} a -> s {status = a} :: DescribeNotificationRuleResponse)

-- | A list of the event types associated with the notification rule.
describeNotificationRuleResponse_eventTypes :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe [EventTypeSummary])
describeNotificationRuleResponse_eventTypes = Lens.lens (\DescribeNotificationRuleResponse' {eventTypes} -> eventTypes) (\s@DescribeNotificationRuleResponse' {} a -> s {eventTypes = a} :: DescribeNotificationRuleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date and time the notification rule was most recently updated, in
-- timestamp format.
describeNotificationRuleResponse_lastModifiedTimestamp :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe Prelude.UTCTime)
describeNotificationRuleResponse_lastModifiedTimestamp = Lens.lens (\DescribeNotificationRuleResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@DescribeNotificationRuleResponse' {} a -> s {lastModifiedTimestamp = a} :: DescribeNotificationRuleResponse) Prelude.. Lens.mapping Core._Time

-- | The name or email alias of the person who created the notification rule.
describeNotificationRuleResponse_createdBy :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe Prelude.Text)
describeNotificationRuleResponse_createdBy = Lens.lens (\DescribeNotificationRuleResponse' {createdBy} -> createdBy) (\s@DescribeNotificationRuleResponse' {} a -> s {createdBy = a} :: DescribeNotificationRuleResponse)

-- | The level of detail included in the notifications for this resource.
-- BASIC will include only the contents of the event as it would appear in
-- AWS CloudWatch. FULL will include any supplemental information provided
-- by AWS CodeStar Notifications and\/or the service for the resource for
-- which the notification is created.
describeNotificationRuleResponse_detailType :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe DetailType)
describeNotificationRuleResponse_detailType = Lens.lens (\DescribeNotificationRuleResponse' {detailType} -> detailType) (\s@DescribeNotificationRuleResponse' {} a -> s {detailType = a} :: DescribeNotificationRuleResponse)

-- | The name of the notification rule.
describeNotificationRuleResponse_name :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe Prelude.Text)
describeNotificationRuleResponse_name = Lens.lens (\DescribeNotificationRuleResponse' {name} -> name) (\s@DescribeNotificationRuleResponse' {} a -> s {name = a} :: DescribeNotificationRuleResponse) Prelude.. Lens.mapping Core._Sensitive

-- | A list of the SNS topics associated with the notification rule.
describeNotificationRuleResponse_targets :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe [TargetSummary])
describeNotificationRuleResponse_targets = Lens.lens (\DescribeNotificationRuleResponse' {targets} -> targets) (\s@DescribeNotificationRuleResponse' {} a -> s {targets = a} :: DescribeNotificationRuleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the resource associated with the
-- notification rule.
describeNotificationRuleResponse_resource :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe Prelude.Text)
describeNotificationRuleResponse_resource = Lens.lens (\DescribeNotificationRuleResponse' {resource} -> resource) (\s@DescribeNotificationRuleResponse' {} a -> s {resource = a} :: DescribeNotificationRuleResponse)

-- | The date and time the notification rule was created, in timestamp
-- format.
describeNotificationRuleResponse_createdTimestamp :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe Prelude.UTCTime)
describeNotificationRuleResponse_createdTimestamp = Lens.lens (\DescribeNotificationRuleResponse' {createdTimestamp} -> createdTimestamp) (\s@DescribeNotificationRuleResponse' {} a -> s {createdTimestamp = a} :: DescribeNotificationRuleResponse) Prelude.. Lens.mapping Core._Time

-- | The tags associated with the notification rule.
describeNotificationRuleResponse_tags :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeNotificationRuleResponse_tags = Lens.lens (\DescribeNotificationRuleResponse' {tags} -> tags) (\s@DescribeNotificationRuleResponse' {} a -> s {tags = a} :: DescribeNotificationRuleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeNotificationRuleResponse_httpStatus :: Lens.Lens' DescribeNotificationRuleResponse Prelude.Int
describeNotificationRuleResponse_httpStatus = Lens.lens (\DescribeNotificationRuleResponse' {httpStatus} -> httpStatus) (\s@DescribeNotificationRuleResponse' {} a -> s {httpStatus = a} :: DescribeNotificationRuleResponse)

-- | The Amazon Resource Name (ARN) of the notification rule.
describeNotificationRuleResponse_arn :: Lens.Lens' DescribeNotificationRuleResponse Prelude.Text
describeNotificationRuleResponse_arn = Lens.lens (\DescribeNotificationRuleResponse' {arn} -> arn) (\s@DescribeNotificationRuleResponse' {} a -> s {arn = a} :: DescribeNotificationRuleResponse)

instance
  Prelude.NFData
    DescribeNotificationRuleResponse
