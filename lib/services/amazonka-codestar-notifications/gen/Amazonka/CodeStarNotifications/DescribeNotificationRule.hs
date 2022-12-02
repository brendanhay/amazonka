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
-- Module      : Amazonka.CodeStarNotifications.DescribeNotificationRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a specified notification rule.
module Amazonka.CodeStarNotifications.DescribeNotificationRule
  ( -- * Creating a Request
    DescribeNotificationRule (..),
    newDescribeNotificationRule,

    -- * Request Lenses
    describeNotificationRule_arn,

    -- * Destructuring the Response
    DescribeNotificationRuleResponse (..),
    newDescribeNotificationRuleResponse,

    -- * Response Lenses
    describeNotificationRuleResponse_tags,
    describeNotificationRuleResponse_name,
    describeNotificationRuleResponse_detailType,
    describeNotificationRuleResponse_createdTimestamp,
    describeNotificationRuleResponse_lastModifiedTimestamp,
    describeNotificationRuleResponse_status,
    describeNotificationRuleResponse_targets,
    describeNotificationRuleResponse_eventTypes,
    describeNotificationRuleResponse_createdBy,
    describeNotificationRuleResponse_resource,
    describeNotificationRuleResponse_httpStatus,
    describeNotificationRuleResponse_arn,
  )
where

import Amazonka.CodeStarNotifications.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeNotificationRuleResponse'
            Prelude.<$> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "DetailType")
            Prelude.<*> (x Data..?> "CreatedTimestamp")
            Prelude.<*> (x Data..?> "LastModifiedTimestamp")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (x Data..?> "Targets" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "EventTypes" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "CreatedBy")
            Prelude.<*> (x Data..?> "Resource")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Arn")
      )

instance Prelude.Hashable DescribeNotificationRule where
  hashWithSalt _salt DescribeNotificationRule' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DescribeNotificationRule where
  rnf DescribeNotificationRule' {..} = Prelude.rnf arn

instance Data.ToHeaders DescribeNotificationRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeNotificationRule where
  toJSON DescribeNotificationRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Arn" Data..= arn)]
      )

instance Data.ToPath DescribeNotificationRule where
  toPath = Prelude.const "/describeNotificationRule"

instance Data.ToQuery DescribeNotificationRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeNotificationRuleResponse' smart constructor.
data DescribeNotificationRuleResponse = DescribeNotificationRuleResponse'
  { -- | The tags associated with the notification rule.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the notification rule.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The level of detail included in the notifications for this resource.
    -- BASIC will include only the contents of the event as it would appear in
    -- Amazon CloudWatch. FULL will include any supplemental information
    -- provided by AWS CodeStar Notifications and\/or the service for the
    -- resource for which the notification is created.
    detailType :: Prelude.Maybe DetailType,
    -- | The date and time the notification rule was created, in timestamp
    -- format.
    createdTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The date and time the notification rule was most recently updated, in
    -- timestamp format.
    lastModifiedTimestamp :: Prelude.Maybe Data.POSIX,
    -- | The status of the notification rule. Valid statuses are on (sending
    -- notifications) or off (not sending notifications).
    status :: Prelude.Maybe NotificationRuleStatus,
    -- | A list of the Chatbot topics and Chatbot clients associated with the
    -- notification rule.
    targets :: Prelude.Maybe [TargetSummary],
    -- | A list of the event types associated with the notification rule.
    eventTypes :: Prelude.Maybe [EventTypeSummary],
    -- | The name or email alias of the person who created the notification rule.
    createdBy :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the resource associated with the
    -- notification rule.
    resource :: Prelude.Maybe Prelude.Text,
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
-- 'tags', 'describeNotificationRuleResponse_tags' - The tags associated with the notification rule.
--
-- 'name', 'describeNotificationRuleResponse_name' - The name of the notification rule.
--
-- 'detailType', 'describeNotificationRuleResponse_detailType' - The level of detail included in the notifications for this resource.
-- BASIC will include only the contents of the event as it would appear in
-- Amazon CloudWatch. FULL will include any supplemental information
-- provided by AWS CodeStar Notifications and\/or the service for the
-- resource for which the notification is created.
--
-- 'createdTimestamp', 'describeNotificationRuleResponse_createdTimestamp' - The date and time the notification rule was created, in timestamp
-- format.
--
-- 'lastModifiedTimestamp', 'describeNotificationRuleResponse_lastModifiedTimestamp' - The date and time the notification rule was most recently updated, in
-- timestamp format.
--
-- 'status', 'describeNotificationRuleResponse_status' - The status of the notification rule. Valid statuses are on (sending
-- notifications) or off (not sending notifications).
--
-- 'targets', 'describeNotificationRuleResponse_targets' - A list of the Chatbot topics and Chatbot clients associated with the
-- notification rule.
--
-- 'eventTypes', 'describeNotificationRuleResponse_eventTypes' - A list of the event types associated with the notification rule.
--
-- 'createdBy', 'describeNotificationRuleResponse_createdBy' - The name or email alias of the person who created the notification rule.
--
-- 'resource', 'describeNotificationRuleResponse_resource' - The Amazon Resource Name (ARN) of the resource associated with the
-- notification rule.
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
      { tags =
          Prelude.Nothing,
        name = Prelude.Nothing,
        detailType = Prelude.Nothing,
        createdTimestamp = Prelude.Nothing,
        lastModifiedTimestamp = Prelude.Nothing,
        status = Prelude.Nothing,
        targets = Prelude.Nothing,
        eventTypes = Prelude.Nothing,
        createdBy = Prelude.Nothing,
        resource = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_
      }

-- | The tags associated with the notification rule.
describeNotificationRuleResponse_tags :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describeNotificationRuleResponse_tags = Lens.lens (\DescribeNotificationRuleResponse' {tags} -> tags) (\s@DescribeNotificationRuleResponse' {} a -> s {tags = a} :: DescribeNotificationRuleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name of the notification rule.
describeNotificationRuleResponse_name :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe Prelude.Text)
describeNotificationRuleResponse_name = Lens.lens (\DescribeNotificationRuleResponse' {name} -> name) (\s@DescribeNotificationRuleResponse' {} a -> s {name = a} :: DescribeNotificationRuleResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The level of detail included in the notifications for this resource.
-- BASIC will include only the contents of the event as it would appear in
-- Amazon CloudWatch. FULL will include any supplemental information
-- provided by AWS CodeStar Notifications and\/or the service for the
-- resource for which the notification is created.
describeNotificationRuleResponse_detailType :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe DetailType)
describeNotificationRuleResponse_detailType = Lens.lens (\DescribeNotificationRuleResponse' {detailType} -> detailType) (\s@DescribeNotificationRuleResponse' {} a -> s {detailType = a} :: DescribeNotificationRuleResponse)

-- | The date and time the notification rule was created, in timestamp
-- format.
describeNotificationRuleResponse_createdTimestamp :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe Prelude.UTCTime)
describeNotificationRuleResponse_createdTimestamp = Lens.lens (\DescribeNotificationRuleResponse' {createdTimestamp} -> createdTimestamp) (\s@DescribeNotificationRuleResponse' {} a -> s {createdTimestamp = a} :: DescribeNotificationRuleResponse) Prelude.. Lens.mapping Data._Time

-- | The date and time the notification rule was most recently updated, in
-- timestamp format.
describeNotificationRuleResponse_lastModifiedTimestamp :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe Prelude.UTCTime)
describeNotificationRuleResponse_lastModifiedTimestamp = Lens.lens (\DescribeNotificationRuleResponse' {lastModifiedTimestamp} -> lastModifiedTimestamp) (\s@DescribeNotificationRuleResponse' {} a -> s {lastModifiedTimestamp = a} :: DescribeNotificationRuleResponse) Prelude.. Lens.mapping Data._Time

-- | The status of the notification rule. Valid statuses are on (sending
-- notifications) or off (not sending notifications).
describeNotificationRuleResponse_status :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe NotificationRuleStatus)
describeNotificationRuleResponse_status = Lens.lens (\DescribeNotificationRuleResponse' {status} -> status) (\s@DescribeNotificationRuleResponse' {} a -> s {status = a} :: DescribeNotificationRuleResponse)

-- | A list of the Chatbot topics and Chatbot clients associated with the
-- notification rule.
describeNotificationRuleResponse_targets :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe [TargetSummary])
describeNotificationRuleResponse_targets = Lens.lens (\DescribeNotificationRuleResponse' {targets} -> targets) (\s@DescribeNotificationRuleResponse' {} a -> s {targets = a} :: DescribeNotificationRuleResponse) Prelude.. Lens.mapping Lens.coerced

-- | A list of the event types associated with the notification rule.
describeNotificationRuleResponse_eventTypes :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe [EventTypeSummary])
describeNotificationRuleResponse_eventTypes = Lens.lens (\DescribeNotificationRuleResponse' {eventTypes} -> eventTypes) (\s@DescribeNotificationRuleResponse' {} a -> s {eventTypes = a} :: DescribeNotificationRuleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The name or email alias of the person who created the notification rule.
describeNotificationRuleResponse_createdBy :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe Prelude.Text)
describeNotificationRuleResponse_createdBy = Lens.lens (\DescribeNotificationRuleResponse' {createdBy} -> createdBy) (\s@DescribeNotificationRuleResponse' {} a -> s {createdBy = a} :: DescribeNotificationRuleResponse)

-- | The Amazon Resource Name (ARN) of the resource associated with the
-- notification rule.
describeNotificationRuleResponse_resource :: Lens.Lens' DescribeNotificationRuleResponse (Prelude.Maybe Prelude.Text)
describeNotificationRuleResponse_resource = Lens.lens (\DescribeNotificationRuleResponse' {resource} -> resource) (\s@DescribeNotificationRuleResponse' {} a -> s {resource = a} :: DescribeNotificationRuleResponse)

-- | The response's http status code.
describeNotificationRuleResponse_httpStatus :: Lens.Lens' DescribeNotificationRuleResponse Prelude.Int
describeNotificationRuleResponse_httpStatus = Lens.lens (\DescribeNotificationRuleResponse' {httpStatus} -> httpStatus) (\s@DescribeNotificationRuleResponse' {} a -> s {httpStatus = a} :: DescribeNotificationRuleResponse)

-- | The Amazon Resource Name (ARN) of the notification rule.
describeNotificationRuleResponse_arn :: Lens.Lens' DescribeNotificationRuleResponse Prelude.Text
describeNotificationRuleResponse_arn = Lens.lens (\DescribeNotificationRuleResponse' {arn} -> arn) (\s@DescribeNotificationRuleResponse' {} a -> s {arn = a} :: DescribeNotificationRuleResponse)

instance
  Prelude.NFData
    DescribeNotificationRuleResponse
  where
  rnf DescribeNotificationRuleResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf detailType
      `Prelude.seq` Prelude.rnf createdTimestamp
      `Prelude.seq` Prelude.rnf lastModifiedTimestamp
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf eventTypes
      `Prelude.seq` Prelude.rnf createdBy
      `Prelude.seq` Prelude.rnf resource
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
