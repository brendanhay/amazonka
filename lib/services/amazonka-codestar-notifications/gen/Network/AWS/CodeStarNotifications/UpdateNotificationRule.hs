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
-- Module      : Network.AWS.CodeStarNotifications.UpdateNotificationRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a notification rule for a resource. You can change the events
-- that trigger the notification rule, the status of the rule, and the
-- targets that receive the notifications.
--
-- To add or remove tags for a notification rule, you must use TagResource
-- and UntagResource.
module Network.AWS.CodeStarNotifications.UpdateNotificationRule
  ( -- * Creating a Request
    UpdateNotificationRule (..),
    newUpdateNotificationRule,

    -- * Request Lenses
    updateNotificationRule_status,
    updateNotificationRule_eventTypeIds,
    updateNotificationRule_detailType,
    updateNotificationRule_name,
    updateNotificationRule_targets,
    updateNotificationRule_arn,

    -- * Destructuring the Response
    UpdateNotificationRuleResponse (..),
    newUpdateNotificationRuleResponse,

    -- * Response Lenses
    updateNotificationRuleResponse_httpStatus,
  )
where

import Network.AWS.CodeStarNotifications.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateNotificationRule' smart constructor.
data UpdateNotificationRule = UpdateNotificationRule'
  { -- | The status of the notification rule. Valid statuses include enabled
    -- (sending notifications) or disabled (not sending notifications).
    status :: Prelude.Maybe NotificationRuleStatus,
    -- | A list of event types associated with this notification rule.
    eventTypeIds :: Prelude.Maybe [Prelude.Text],
    -- | The level of detail to include in the notifications for this resource.
    -- BASIC will include only the contents of the event as it would appear in
    -- AWS CloudWatch. FULL will include any supplemental information provided
    -- by AWS CodeStar Notifications and\/or the service for the resource for
    -- which the notification is created.
    detailType :: Prelude.Maybe DetailType,
    -- | The name of the notification rule.
    name :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The address and type of the targets to receive notifications from this
    -- notification rule.
    targets :: Prelude.Maybe [Target],
    -- | The Amazon Resource Name (ARN) of the notification rule.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNotificationRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'updateNotificationRule_status' - The status of the notification rule. Valid statuses include enabled
-- (sending notifications) or disabled (not sending notifications).
--
-- 'eventTypeIds', 'updateNotificationRule_eventTypeIds' - A list of event types associated with this notification rule.
--
-- 'detailType', 'updateNotificationRule_detailType' - The level of detail to include in the notifications for this resource.
-- BASIC will include only the contents of the event as it would appear in
-- AWS CloudWatch. FULL will include any supplemental information provided
-- by AWS CodeStar Notifications and\/or the service for the resource for
-- which the notification is created.
--
-- 'name', 'updateNotificationRule_name' - The name of the notification rule.
--
-- 'targets', 'updateNotificationRule_targets' - The address and type of the targets to receive notifications from this
-- notification rule.
--
-- 'arn', 'updateNotificationRule_arn' - The Amazon Resource Name (ARN) of the notification rule.
newUpdateNotificationRule ::
  -- | 'arn'
  Prelude.Text ->
  UpdateNotificationRule
newUpdateNotificationRule pArn_ =
  UpdateNotificationRule'
    { status = Prelude.Nothing,
      eventTypeIds = Prelude.Nothing,
      detailType = Prelude.Nothing,
      name = Prelude.Nothing,
      targets = Prelude.Nothing,
      arn = pArn_
    }

-- | The status of the notification rule. Valid statuses include enabled
-- (sending notifications) or disabled (not sending notifications).
updateNotificationRule_status :: Lens.Lens' UpdateNotificationRule (Prelude.Maybe NotificationRuleStatus)
updateNotificationRule_status = Lens.lens (\UpdateNotificationRule' {status} -> status) (\s@UpdateNotificationRule' {} a -> s {status = a} :: UpdateNotificationRule)

-- | A list of event types associated with this notification rule.
updateNotificationRule_eventTypeIds :: Lens.Lens' UpdateNotificationRule (Prelude.Maybe [Prelude.Text])
updateNotificationRule_eventTypeIds = Lens.lens (\UpdateNotificationRule' {eventTypeIds} -> eventTypeIds) (\s@UpdateNotificationRule' {} a -> s {eventTypeIds = a} :: UpdateNotificationRule) Prelude.. Lens.mapping Lens.coerced

-- | The level of detail to include in the notifications for this resource.
-- BASIC will include only the contents of the event as it would appear in
-- AWS CloudWatch. FULL will include any supplemental information provided
-- by AWS CodeStar Notifications and\/or the service for the resource for
-- which the notification is created.
updateNotificationRule_detailType :: Lens.Lens' UpdateNotificationRule (Prelude.Maybe DetailType)
updateNotificationRule_detailType = Lens.lens (\UpdateNotificationRule' {detailType} -> detailType) (\s@UpdateNotificationRule' {} a -> s {detailType = a} :: UpdateNotificationRule)

-- | The name of the notification rule.
updateNotificationRule_name :: Lens.Lens' UpdateNotificationRule (Prelude.Maybe Prelude.Text)
updateNotificationRule_name = Lens.lens (\UpdateNotificationRule' {name} -> name) (\s@UpdateNotificationRule' {} a -> s {name = a} :: UpdateNotificationRule) Prelude.. Lens.mapping Core._Sensitive

-- | The address and type of the targets to receive notifications from this
-- notification rule.
updateNotificationRule_targets :: Lens.Lens' UpdateNotificationRule (Prelude.Maybe [Target])
updateNotificationRule_targets = Lens.lens (\UpdateNotificationRule' {targets} -> targets) (\s@UpdateNotificationRule' {} a -> s {targets = a} :: UpdateNotificationRule) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the notification rule.
updateNotificationRule_arn :: Lens.Lens' UpdateNotificationRule Prelude.Text
updateNotificationRule_arn = Lens.lens (\UpdateNotificationRule' {arn} -> arn) (\s@UpdateNotificationRule' {} a -> s {arn = a} :: UpdateNotificationRule)

instance Core.AWSRequest UpdateNotificationRule where
  type
    AWSResponse UpdateNotificationRule =
      UpdateNotificationRuleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNotificationRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateNotificationRule

instance Prelude.NFData UpdateNotificationRule

instance Core.ToHeaders UpdateNotificationRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateNotificationRule where
  toJSON UpdateNotificationRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("EventTypeIds" Core..=) Prelude.<$> eventTypeIds,
            ("DetailType" Core..=) Prelude.<$> detailType,
            ("Name" Core..=) Prelude.<$> name,
            ("Targets" Core..=) Prelude.<$> targets,
            Prelude.Just ("Arn" Core..= arn)
          ]
      )

instance Core.ToPath UpdateNotificationRule where
  toPath = Prelude.const "/updateNotificationRule"

instance Core.ToQuery UpdateNotificationRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateNotificationRuleResponse' smart constructor.
data UpdateNotificationRuleResponse = UpdateNotificationRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateNotificationRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateNotificationRuleResponse_httpStatus' - The response's http status code.
newUpdateNotificationRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateNotificationRuleResponse
newUpdateNotificationRuleResponse pHttpStatus_ =
  UpdateNotificationRuleResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateNotificationRuleResponse_httpStatus :: Lens.Lens' UpdateNotificationRuleResponse Prelude.Int
updateNotificationRuleResponse_httpStatus = Lens.lens (\UpdateNotificationRuleResponse' {httpStatus} -> httpStatus) (\s@UpdateNotificationRuleResponse' {} a -> s {httpStatus = a} :: UpdateNotificationRuleResponse)

instance
  Prelude.NFData
    UpdateNotificationRuleResponse
