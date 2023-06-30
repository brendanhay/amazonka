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
-- Module      : Amazonka.SSMIncidents.StartIncident
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to start an incident from CloudWatch alarms, EventBridge events, or
-- manually.
module Amazonka.SSMIncidents.StartIncident
  ( -- * Creating a Request
    StartIncident (..),
    newStartIncident,

    -- * Request Lenses
    startIncident_clientToken,
    startIncident_impact,
    startIncident_relatedItems,
    startIncident_title,
    startIncident_triggerDetails,
    startIncident_responsePlanArn,

    -- * Destructuring the Response
    StartIncidentResponse (..),
    newStartIncidentResponse,

    -- * Response Lenses
    startIncidentResponse_httpStatus,
    startIncidentResponse_incidentRecordArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newStartIncident' smart constructor.
data StartIncident = StartIncident'
  { -- | A token ensuring that the operation is called only once with the
    -- specified details.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Defines the impact to the customers. Providing an impact overwrites the
    -- impact provided by a response plan.
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
    -- | Add related items to the incident for other responders to use. Related
    -- items are AWS resources, external links, or files uploaded to an Amazon
    -- S3 bucket.
    relatedItems :: Prelude.Maybe [RelatedItem],
    -- | Provide a title for the incident. Providing a title overwrites the title
    -- provided by the response plan.
    title :: Prelude.Maybe Prelude.Text,
    -- | Details of what created the incident record in Incident Manager.
    triggerDetails :: Prelude.Maybe TriggerDetails,
    -- | The Amazon Resource Name (ARN) of the response plan that pre-defines
    -- summary, chat channels, Amazon SNS topics, runbooks, title, and impact
    -- of the incident.
    responsePlanArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartIncident' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'startIncident_clientToken' - A token ensuring that the operation is called only once with the
-- specified details.
--
-- 'impact', 'startIncident_impact' - Defines the impact to the customers. Providing an impact overwrites the
-- impact provided by a response plan.
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
-- 'relatedItems', 'startIncident_relatedItems' - Add related items to the incident for other responders to use. Related
-- items are AWS resources, external links, or files uploaded to an Amazon
-- S3 bucket.
--
-- 'title', 'startIncident_title' - Provide a title for the incident. Providing a title overwrites the title
-- provided by the response plan.
--
-- 'triggerDetails', 'startIncident_triggerDetails' - Details of what created the incident record in Incident Manager.
--
-- 'responsePlanArn', 'startIncident_responsePlanArn' - The Amazon Resource Name (ARN) of the response plan that pre-defines
-- summary, chat channels, Amazon SNS topics, runbooks, title, and impact
-- of the incident.
newStartIncident ::
  -- | 'responsePlanArn'
  Prelude.Text ->
  StartIncident
newStartIncident pResponsePlanArn_ =
  StartIncident'
    { clientToken = Prelude.Nothing,
      impact = Prelude.Nothing,
      relatedItems = Prelude.Nothing,
      title = Prelude.Nothing,
      triggerDetails = Prelude.Nothing,
      responsePlanArn = pResponsePlanArn_
    }

-- | A token ensuring that the operation is called only once with the
-- specified details.
startIncident_clientToken :: Lens.Lens' StartIncident (Prelude.Maybe Prelude.Text)
startIncident_clientToken = Lens.lens (\StartIncident' {clientToken} -> clientToken) (\s@StartIncident' {} a -> s {clientToken = a} :: StartIncident)

-- | Defines the impact to the customers. Providing an impact overwrites the
-- impact provided by a response plan.
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
startIncident_impact :: Lens.Lens' StartIncident (Prelude.Maybe Prelude.Natural)
startIncident_impact = Lens.lens (\StartIncident' {impact} -> impact) (\s@StartIncident' {} a -> s {impact = a} :: StartIncident)

-- | Add related items to the incident for other responders to use. Related
-- items are AWS resources, external links, or files uploaded to an Amazon
-- S3 bucket.
startIncident_relatedItems :: Lens.Lens' StartIncident (Prelude.Maybe [RelatedItem])
startIncident_relatedItems = Lens.lens (\StartIncident' {relatedItems} -> relatedItems) (\s@StartIncident' {} a -> s {relatedItems = a} :: StartIncident) Prelude.. Lens.mapping Lens.coerced

-- | Provide a title for the incident. Providing a title overwrites the title
-- provided by the response plan.
startIncident_title :: Lens.Lens' StartIncident (Prelude.Maybe Prelude.Text)
startIncident_title = Lens.lens (\StartIncident' {title} -> title) (\s@StartIncident' {} a -> s {title = a} :: StartIncident)

-- | Details of what created the incident record in Incident Manager.
startIncident_triggerDetails :: Lens.Lens' StartIncident (Prelude.Maybe TriggerDetails)
startIncident_triggerDetails = Lens.lens (\StartIncident' {triggerDetails} -> triggerDetails) (\s@StartIncident' {} a -> s {triggerDetails = a} :: StartIncident)

-- | The Amazon Resource Name (ARN) of the response plan that pre-defines
-- summary, chat channels, Amazon SNS topics, runbooks, title, and impact
-- of the incident.
startIncident_responsePlanArn :: Lens.Lens' StartIncident Prelude.Text
startIncident_responsePlanArn = Lens.lens (\StartIncident' {responsePlanArn} -> responsePlanArn) (\s@StartIncident' {} a -> s {responsePlanArn = a} :: StartIncident)

instance Core.AWSRequest StartIncident where
  type
    AWSResponse StartIncident =
      StartIncidentResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          StartIncidentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "incidentRecordArn")
      )

instance Prelude.Hashable StartIncident where
  hashWithSalt _salt StartIncident' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` impact
      `Prelude.hashWithSalt` relatedItems
      `Prelude.hashWithSalt` title
      `Prelude.hashWithSalt` triggerDetails
      `Prelude.hashWithSalt` responsePlanArn

instance Prelude.NFData StartIncident where
  rnf StartIncident' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf impact
      `Prelude.seq` Prelude.rnf relatedItems
      `Prelude.seq` Prelude.rnf title
      `Prelude.seq` Prelude.rnf triggerDetails
      `Prelude.seq` Prelude.rnf responsePlanArn

instance Data.ToHeaders StartIncident where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON StartIncident where
  toJSON StartIncident' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            ("impact" Data..=) Prelude.<$> impact,
            ("relatedItems" Data..=) Prelude.<$> relatedItems,
            ("title" Data..=) Prelude.<$> title,
            ("triggerDetails" Data..=)
              Prelude.<$> triggerDetails,
            Prelude.Just
              ("responsePlanArn" Data..= responsePlanArn)
          ]
      )

instance Data.ToPath StartIncident where
  toPath = Prelude.const "/startIncident"

instance Data.ToQuery StartIncident where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newStartIncidentResponse' smart constructor.
data StartIncidentResponse = StartIncidentResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the newly created incident record.
    incidentRecordArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'StartIncidentResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'startIncidentResponse_httpStatus' - The response's http status code.
--
-- 'incidentRecordArn', 'startIncidentResponse_incidentRecordArn' - The ARN of the newly created incident record.
newStartIncidentResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'incidentRecordArn'
  Prelude.Text ->
  StartIncidentResponse
newStartIncidentResponse
  pHttpStatus_
  pIncidentRecordArn_ =
    StartIncidentResponse'
      { httpStatus = pHttpStatus_,
        incidentRecordArn = pIncidentRecordArn_
      }

-- | The response's http status code.
startIncidentResponse_httpStatus :: Lens.Lens' StartIncidentResponse Prelude.Int
startIncidentResponse_httpStatus = Lens.lens (\StartIncidentResponse' {httpStatus} -> httpStatus) (\s@StartIncidentResponse' {} a -> s {httpStatus = a} :: StartIncidentResponse)

-- | The ARN of the newly created incident record.
startIncidentResponse_incidentRecordArn :: Lens.Lens' StartIncidentResponse Prelude.Text
startIncidentResponse_incidentRecordArn = Lens.lens (\StartIncidentResponse' {incidentRecordArn} -> incidentRecordArn) (\s@StartIncidentResponse' {} a -> s {incidentRecordArn = a} :: StartIncidentResponse)

instance Prelude.NFData StartIncidentResponse where
  rnf StartIncidentResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf incidentRecordArn
