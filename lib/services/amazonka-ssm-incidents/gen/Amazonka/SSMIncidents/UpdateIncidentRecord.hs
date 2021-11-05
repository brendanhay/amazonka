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
-- Module      : Amazonka.SSMIncidents.UpdateIncidentRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the details of an incident record. You can use this action to
-- update an incident record from the defined chat channel. For more
-- information about using actions in chat channels, see
-- <https://docs.aws.amazon.com/incident-manager/latest/userguide/chat.html#chat-interact Interacting through chat>.
module Amazonka.SSMIncidents.UpdateIncidentRecord
  ( -- * Creating a Request
    UpdateIncidentRecord (..),
    newUpdateIncidentRecord,

    -- * Request Lenses
    updateIncidentRecord_summary,
    updateIncidentRecord_status,
    updateIncidentRecord_notificationTargets,
    updateIncidentRecord_clientToken,
    updateIncidentRecord_impact,
    updateIncidentRecord_chatChannel,
    updateIncidentRecord_title,
    updateIncidentRecord_arn,

    -- * Destructuring the Response
    UpdateIncidentRecordResponse (..),
    newUpdateIncidentRecordResponse,

    -- * Response Lenses
    updateIncidentRecordResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newUpdateIncidentRecord' smart constructor.
data UpdateIncidentRecord = UpdateIncidentRecord'
  { -- | The summary describes what has happened during the incident.
    summary :: Prelude.Maybe Prelude.Text,
    -- | The status of the incident. An incident can be @Open@ or @Resolved@.
    status :: Prelude.Maybe IncidentRecordStatus,
    -- | The SNS targets that are notified when updates are made to an incident.
    --
    -- Using multiple SNS topics creates redundancy in the case that a Region
    -- is down during the incident.
    notificationTargets :: Prelude.Maybe [NotificationTargetItem],
    -- | A token ensuring that the action is called only once with the specified
    -- details.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Defines the impact to customers and applications. Providing an impact
    -- overwrites the impact provided by the response plan.
    --
    -- __Possible impacts:__
    --
    -- -   @1@ - Critical impact, this typically relates to full application
    --     failure that impacts many to all customers.
    --
    -- -   @2@ - High impact, partial application failure with impact to many
    --     customers.
    --
    -- -   @3@ - Medium impact, the application is providing reduced service to
    --     customers.
    --
    -- -   @4@ - Low impact, customer might aren\'t impacted by the problem
    --     yet.
    --
    -- -   @5@ - No impact, customers aren\'t currently impacted but urgent
    --     action is needed to avoid impact.
    impact :: Prelude.Maybe Prelude.Natural,
    -- | The AWS Chatbot chat channel for responders to collaborate in.
    chatChannel :: Prelude.Maybe ChatChannel,
    -- | The title of the incident is a brief and easily recognizable.
    title :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the incident record you are updating.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIncidentRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'summary', 'updateIncidentRecord_summary' - The summary describes what has happened during the incident.
--
-- 'status', 'updateIncidentRecord_status' - The status of the incident. An incident can be @Open@ or @Resolved@.
--
-- 'notificationTargets', 'updateIncidentRecord_notificationTargets' - The SNS targets that are notified when updates are made to an incident.
--
-- Using multiple SNS topics creates redundancy in the case that a Region
-- is down during the incident.
--
-- 'clientToken', 'updateIncidentRecord_clientToken' - A token ensuring that the action is called only once with the specified
-- details.
--
-- 'impact', 'updateIncidentRecord_impact' - Defines the impact to customers and applications. Providing an impact
-- overwrites the impact provided by the response plan.
--
-- __Possible impacts:__
--
-- -   @1@ - Critical impact, this typically relates to full application
--     failure that impacts many to all customers.
--
-- -   @2@ - High impact, partial application failure with impact to many
--     customers.
--
-- -   @3@ - Medium impact, the application is providing reduced service to
--     customers.
--
-- -   @4@ - Low impact, customer might aren\'t impacted by the problem
--     yet.
--
-- -   @5@ - No impact, customers aren\'t currently impacted but urgent
--     action is needed to avoid impact.
--
-- 'chatChannel', 'updateIncidentRecord_chatChannel' - The AWS Chatbot chat channel for responders to collaborate in.
--
-- 'title', 'updateIncidentRecord_title' - The title of the incident is a brief and easily recognizable.
--
-- 'arn', 'updateIncidentRecord_arn' - The Amazon Resource Name (ARN) of the incident record you are updating.
newUpdateIncidentRecord ::
  -- | 'arn'
  Prelude.Text ->
  UpdateIncidentRecord
newUpdateIncidentRecord pArn_ =
  UpdateIncidentRecord'
    { summary = Prelude.Nothing,
      status = Prelude.Nothing,
      notificationTargets = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      impact = Prelude.Nothing,
      chatChannel = Prelude.Nothing,
      title = Prelude.Nothing,
      arn = pArn_
    }

-- | The summary describes what has happened during the incident.
updateIncidentRecord_summary :: Lens.Lens' UpdateIncidentRecord (Prelude.Maybe Prelude.Text)
updateIncidentRecord_summary = Lens.lens (\UpdateIncidentRecord' {summary} -> summary) (\s@UpdateIncidentRecord' {} a -> s {summary = a} :: UpdateIncidentRecord)

-- | The status of the incident. An incident can be @Open@ or @Resolved@.
updateIncidentRecord_status :: Lens.Lens' UpdateIncidentRecord (Prelude.Maybe IncidentRecordStatus)
updateIncidentRecord_status = Lens.lens (\UpdateIncidentRecord' {status} -> status) (\s@UpdateIncidentRecord' {} a -> s {status = a} :: UpdateIncidentRecord)

-- | The SNS targets that are notified when updates are made to an incident.
--
-- Using multiple SNS topics creates redundancy in the case that a Region
-- is down during the incident.
updateIncidentRecord_notificationTargets :: Lens.Lens' UpdateIncidentRecord (Prelude.Maybe [NotificationTargetItem])
updateIncidentRecord_notificationTargets = Lens.lens (\UpdateIncidentRecord' {notificationTargets} -> notificationTargets) (\s@UpdateIncidentRecord' {} a -> s {notificationTargets = a} :: UpdateIncidentRecord) Prelude.. Lens.mapping Lens.coerced

-- | A token ensuring that the action is called only once with the specified
-- details.
updateIncidentRecord_clientToken :: Lens.Lens' UpdateIncidentRecord (Prelude.Maybe Prelude.Text)
updateIncidentRecord_clientToken = Lens.lens (\UpdateIncidentRecord' {clientToken} -> clientToken) (\s@UpdateIncidentRecord' {} a -> s {clientToken = a} :: UpdateIncidentRecord)

-- | Defines the impact to customers and applications. Providing an impact
-- overwrites the impact provided by the response plan.
--
-- __Possible impacts:__
--
-- -   @1@ - Critical impact, this typically relates to full application
--     failure that impacts many to all customers.
--
-- -   @2@ - High impact, partial application failure with impact to many
--     customers.
--
-- -   @3@ - Medium impact, the application is providing reduced service to
--     customers.
--
-- -   @4@ - Low impact, customer might aren\'t impacted by the problem
--     yet.
--
-- -   @5@ - No impact, customers aren\'t currently impacted but urgent
--     action is needed to avoid impact.
updateIncidentRecord_impact :: Lens.Lens' UpdateIncidentRecord (Prelude.Maybe Prelude.Natural)
updateIncidentRecord_impact = Lens.lens (\UpdateIncidentRecord' {impact} -> impact) (\s@UpdateIncidentRecord' {} a -> s {impact = a} :: UpdateIncidentRecord)

-- | The AWS Chatbot chat channel for responders to collaborate in.
updateIncidentRecord_chatChannel :: Lens.Lens' UpdateIncidentRecord (Prelude.Maybe ChatChannel)
updateIncidentRecord_chatChannel = Lens.lens (\UpdateIncidentRecord' {chatChannel} -> chatChannel) (\s@UpdateIncidentRecord' {} a -> s {chatChannel = a} :: UpdateIncidentRecord)

-- | The title of the incident is a brief and easily recognizable.
updateIncidentRecord_title :: Lens.Lens' UpdateIncidentRecord (Prelude.Maybe Prelude.Text)
updateIncidentRecord_title = Lens.lens (\UpdateIncidentRecord' {title} -> title) (\s@UpdateIncidentRecord' {} a -> s {title = a} :: UpdateIncidentRecord)

-- | The Amazon Resource Name (ARN) of the incident record you are updating.
updateIncidentRecord_arn :: Lens.Lens' UpdateIncidentRecord Prelude.Text
updateIncidentRecord_arn = Lens.lens (\UpdateIncidentRecord' {arn} -> arn) (\s@UpdateIncidentRecord' {} a -> s {arn = a} :: UpdateIncidentRecord)

instance Core.AWSRequest UpdateIncidentRecord where
  type
    AWSResponse UpdateIncidentRecord =
      UpdateIncidentRecordResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateIncidentRecordResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateIncidentRecord

instance Prelude.NFData UpdateIncidentRecord

instance Core.ToHeaders UpdateIncidentRecord where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateIncidentRecord where
  toJSON UpdateIncidentRecord' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("summary" Core..=) Prelude.<$> summary,
            ("status" Core..=) Prelude.<$> status,
            ("notificationTargets" Core..=)
              Prelude.<$> notificationTargets,
            ("clientToken" Core..=) Prelude.<$> clientToken,
            ("impact" Core..=) Prelude.<$> impact,
            ("chatChannel" Core..=) Prelude.<$> chatChannel,
            ("title" Core..=) Prelude.<$> title,
            Prelude.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath UpdateIncidentRecord where
  toPath = Prelude.const "/updateIncidentRecord"

instance Core.ToQuery UpdateIncidentRecord where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateIncidentRecordResponse' smart constructor.
data UpdateIncidentRecordResponse = UpdateIncidentRecordResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateIncidentRecordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateIncidentRecordResponse_httpStatus' - The response's http status code.
newUpdateIncidentRecordResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateIncidentRecordResponse
newUpdateIncidentRecordResponse pHttpStatus_ =
  UpdateIncidentRecordResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateIncidentRecordResponse_httpStatus :: Lens.Lens' UpdateIncidentRecordResponse Prelude.Int
updateIncidentRecordResponse_httpStatus = Lens.lens (\UpdateIncidentRecordResponse' {httpStatus} -> httpStatus) (\s@UpdateIncidentRecordResponse' {} a -> s {httpStatus = a} :: UpdateIncidentRecordResponse)

instance Prelude.NFData UpdateIncidentRecordResponse
