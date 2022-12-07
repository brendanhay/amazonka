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
-- Module      : Amazonka.CodeStarNotifications.UpdateNotificationRule
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.CodeStarNotifications.UpdateNotificationRule
  ( -- * Creating a Request
    UpdateNotificationRule (..),
    newUpdateNotificationRule,

    -- * Request Lenses
    updateNotificationRule_name,
    updateNotificationRule_detailType,
    updateNotificationRule_status,
    updateNotificationRule_targets,
    updateNotificationRule_eventTypeIds,
    updateNotificationRule_arn,

    -- * Destructuring the Response
    UpdateNotificationRuleResponse (..),
    newUpdateNotificationRuleResponse,

    -- * Response Lenses
    updateNotificationRuleResponse_httpStatus,
  )
where

import Amazonka.CodeStarNotifications.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateNotificationRule' smart constructor.
data UpdateNotificationRule = UpdateNotificationRule'
  { -- | The name of the notification rule.
    name :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The level of detail to include in the notifications for this resource.
    -- BASIC will include only the contents of the event as it would appear in
    -- Amazon CloudWatch. FULL will include any supplemental information
    -- provided by AWS CodeStar Notifications and\/or the service for the
    -- resource for which the notification is created.
    detailType :: Prelude.Maybe DetailType,
    -- | The status of the notification rule. Valid statuses include enabled
    -- (sending notifications) or disabled (not sending notifications).
    status :: Prelude.Maybe NotificationRuleStatus,
    -- | The address and type of the targets to receive notifications from this
    -- notification rule.
    targets :: Prelude.Maybe [Target],
    -- | A list of event types associated with this notification rule. For a
    -- complete list of event types and IDs, see
    -- <https://docs.aws.amazon.com/codestar-notifications/latest/userguide/concepts.html#concepts-api Notification concepts>
    -- in the /Developer Tools Console User Guide/.
    eventTypeIds :: Prelude.Maybe [Prelude.Text],
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
-- 'name', 'updateNotificationRule_name' - The name of the notification rule.
--
-- 'detailType', 'updateNotificationRule_detailType' - The level of detail to include in the notifications for this resource.
-- BASIC will include only the contents of the event as it would appear in
-- Amazon CloudWatch. FULL will include any supplemental information
-- provided by AWS CodeStar Notifications and\/or the service for the
-- resource for which the notification is created.
--
-- 'status', 'updateNotificationRule_status' - The status of the notification rule. Valid statuses include enabled
-- (sending notifications) or disabled (not sending notifications).
--
-- 'targets', 'updateNotificationRule_targets' - The address and type of the targets to receive notifications from this
-- notification rule.
--
-- 'eventTypeIds', 'updateNotificationRule_eventTypeIds' - A list of event types associated with this notification rule. For a
-- complete list of event types and IDs, see
-- <https://docs.aws.amazon.com/codestar-notifications/latest/userguide/concepts.html#concepts-api Notification concepts>
-- in the /Developer Tools Console User Guide/.
--
-- 'arn', 'updateNotificationRule_arn' - The Amazon Resource Name (ARN) of the notification rule.
newUpdateNotificationRule ::
  -- | 'arn'
  Prelude.Text ->
  UpdateNotificationRule
newUpdateNotificationRule pArn_ =
  UpdateNotificationRule'
    { name = Prelude.Nothing,
      detailType = Prelude.Nothing,
      status = Prelude.Nothing,
      targets = Prelude.Nothing,
      eventTypeIds = Prelude.Nothing,
      arn = pArn_
    }

-- | The name of the notification rule.
updateNotificationRule_name :: Lens.Lens' UpdateNotificationRule (Prelude.Maybe Prelude.Text)
updateNotificationRule_name = Lens.lens (\UpdateNotificationRule' {name} -> name) (\s@UpdateNotificationRule' {} a -> s {name = a} :: UpdateNotificationRule) Prelude.. Lens.mapping Data._Sensitive

-- | The level of detail to include in the notifications for this resource.
-- BASIC will include only the contents of the event as it would appear in
-- Amazon CloudWatch. FULL will include any supplemental information
-- provided by AWS CodeStar Notifications and\/or the service for the
-- resource for which the notification is created.
updateNotificationRule_detailType :: Lens.Lens' UpdateNotificationRule (Prelude.Maybe DetailType)
updateNotificationRule_detailType = Lens.lens (\UpdateNotificationRule' {detailType} -> detailType) (\s@UpdateNotificationRule' {} a -> s {detailType = a} :: UpdateNotificationRule)

-- | The status of the notification rule. Valid statuses include enabled
-- (sending notifications) or disabled (not sending notifications).
updateNotificationRule_status :: Lens.Lens' UpdateNotificationRule (Prelude.Maybe NotificationRuleStatus)
updateNotificationRule_status = Lens.lens (\UpdateNotificationRule' {status} -> status) (\s@UpdateNotificationRule' {} a -> s {status = a} :: UpdateNotificationRule)

-- | The address and type of the targets to receive notifications from this
-- notification rule.
updateNotificationRule_targets :: Lens.Lens' UpdateNotificationRule (Prelude.Maybe [Target])
updateNotificationRule_targets = Lens.lens (\UpdateNotificationRule' {targets} -> targets) (\s@UpdateNotificationRule' {} a -> s {targets = a} :: UpdateNotificationRule) Prelude.. Lens.mapping Lens.coerced

-- | A list of event types associated with this notification rule. For a
-- complete list of event types and IDs, see
-- <https://docs.aws.amazon.com/codestar-notifications/latest/userguide/concepts.html#concepts-api Notification concepts>
-- in the /Developer Tools Console User Guide/.
updateNotificationRule_eventTypeIds :: Lens.Lens' UpdateNotificationRule (Prelude.Maybe [Prelude.Text])
updateNotificationRule_eventTypeIds = Lens.lens (\UpdateNotificationRule' {eventTypeIds} -> eventTypeIds) (\s@UpdateNotificationRule' {} a -> s {eventTypeIds = a} :: UpdateNotificationRule) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the notification rule.
updateNotificationRule_arn :: Lens.Lens' UpdateNotificationRule Prelude.Text
updateNotificationRule_arn = Lens.lens (\UpdateNotificationRule' {arn} -> arn) (\s@UpdateNotificationRule' {} a -> s {arn = a} :: UpdateNotificationRule)

instance Core.AWSRequest UpdateNotificationRule where
  type
    AWSResponse UpdateNotificationRule =
      UpdateNotificationRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateNotificationRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateNotificationRule where
  hashWithSalt _salt UpdateNotificationRule' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` detailType
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` targets
      `Prelude.hashWithSalt` eventTypeIds
      `Prelude.hashWithSalt` arn

instance Prelude.NFData UpdateNotificationRule where
  rnf UpdateNotificationRule' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf detailType
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf targets
      `Prelude.seq` Prelude.rnf eventTypeIds
      `Prelude.seq` Prelude.rnf arn

instance Data.ToHeaders UpdateNotificationRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateNotificationRule where
  toJSON UpdateNotificationRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("DetailType" Data..=) Prelude.<$> detailType,
            ("Status" Data..=) Prelude.<$> status,
            ("Targets" Data..=) Prelude.<$> targets,
            ("EventTypeIds" Data..=) Prelude.<$> eventTypeIds,
            Prelude.Just ("Arn" Data..= arn)
          ]
      )

instance Data.ToPath UpdateNotificationRule where
  toPath = Prelude.const "/updateNotificationRule"

instance Data.ToQuery UpdateNotificationRule where
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
  where
  rnf UpdateNotificationRuleResponse' {..} =
    Prelude.rnf httpStatus
