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
-- Module      : Network.AWS.SSMIncidents.StartIncident
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used to start an incident from CloudWatch alarms, EventBridge events, or
-- manually.
module Network.AWS.SSMIncidents.StartIncident
  ( -- * Creating a Request
    StartIncident (..),
    newStartIncident,

    -- * Request Lenses
    startIncident_clientToken,
    startIncident_triggerDetails,
    startIncident_relatedItems,
    startIncident_impact,
    startIncident_title,
    startIncident_responsePlanArn,

    -- * Destructuring the Response
    StartIncidentResponse (..),
    newStartIncidentResponse,

    -- * Response Lenses
    startIncidentResponse_httpStatus,
    startIncidentResponse_incidentRecordArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSMIncidents.Types

-- | /See:/ 'newStartIncident' smart constructor.
data StartIncident = StartIncident'
  { -- | A token ensuring that the action is called only once with the specified
    -- details.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | Details of what created the incident record in Incident Manager.
    triggerDetails :: Prelude.Maybe TriggerDetails,
    -- | Add related items to the incident for other responders to use. Related
    -- items are AWS resources, external links, or files uploaded to an S3
    -- bucket.
    relatedItems :: Prelude.Maybe [RelatedItem],
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
    -- | Provide a title for the incident. Providing a title overwrites the title
    -- provided by the response plan.
    title :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the response plan that pre-defines
    -- summary, chat channels, SNS topics, runbooks, title, and impact of the
    -- incident.
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
-- 'clientToken', 'startIncident_clientToken' - A token ensuring that the action is called only once with the specified
-- details.
--
-- 'triggerDetails', 'startIncident_triggerDetails' - Details of what created the incident record in Incident Manager.
--
-- 'relatedItems', 'startIncident_relatedItems' - Add related items to the incident for other responders to use. Related
-- items are AWS resources, external links, or files uploaded to an S3
-- bucket.
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
-- 'title', 'startIncident_title' - Provide a title for the incident. Providing a title overwrites the title
-- provided by the response plan.
--
-- 'responsePlanArn', 'startIncident_responsePlanArn' - The Amazon Resource Name (ARN) of the response plan that pre-defines
-- summary, chat channels, SNS topics, runbooks, title, and impact of the
-- incident.
newStartIncident ::
  -- | 'responsePlanArn'
  Prelude.Text ->
  StartIncident
newStartIncident pResponsePlanArn_ =
  StartIncident'
    { clientToken = Prelude.Nothing,
      triggerDetails = Prelude.Nothing,
      relatedItems = Prelude.Nothing,
      impact = Prelude.Nothing,
      title = Prelude.Nothing,
      responsePlanArn = pResponsePlanArn_
    }

-- | A token ensuring that the action is called only once with the specified
-- details.
startIncident_clientToken :: Lens.Lens' StartIncident (Prelude.Maybe Prelude.Text)
startIncident_clientToken = Lens.lens (\StartIncident' {clientToken} -> clientToken) (\s@StartIncident' {} a -> s {clientToken = a} :: StartIncident)

-- | Details of what created the incident record in Incident Manager.
startIncident_triggerDetails :: Lens.Lens' StartIncident (Prelude.Maybe TriggerDetails)
startIncident_triggerDetails = Lens.lens (\StartIncident' {triggerDetails} -> triggerDetails) (\s@StartIncident' {} a -> s {triggerDetails = a} :: StartIncident)

-- | Add related items to the incident for other responders to use. Related
-- items are AWS resources, external links, or files uploaded to an S3
-- bucket.
startIncident_relatedItems :: Lens.Lens' StartIncident (Prelude.Maybe [RelatedItem])
startIncident_relatedItems = Lens.lens (\StartIncident' {relatedItems} -> relatedItems) (\s@StartIncident' {} a -> s {relatedItems = a} :: StartIncident) Prelude.. Lens.mapping Lens.coerced

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

-- | Provide a title for the incident. Providing a title overwrites the title
-- provided by the response plan.
startIncident_title :: Lens.Lens' StartIncident (Prelude.Maybe Prelude.Text)
startIncident_title = Lens.lens (\StartIncident' {title} -> title) (\s@StartIncident' {} a -> s {title = a} :: StartIncident)

-- | The Amazon Resource Name (ARN) of the response plan that pre-defines
-- summary, chat channels, SNS topics, runbooks, title, and impact of the
-- incident.
startIncident_responsePlanArn :: Lens.Lens' StartIncident Prelude.Text
startIncident_responsePlanArn = Lens.lens (\StartIncident' {responsePlanArn} -> responsePlanArn) (\s@StartIncident' {} a -> s {responsePlanArn = a} :: StartIncident)

instance Core.AWSRequest StartIncident where
  type
    AWSResponse StartIncident =
      StartIncidentResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          StartIncidentResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "incidentRecordArn")
      )

instance Prelude.Hashable StartIncident

instance Prelude.NFData StartIncident

instance Core.ToHeaders StartIncident where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON StartIncident where
  toJSON StartIncident' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("clientToken" Core..=) Prelude.<$> clientToken,
            ("triggerDetails" Core..=)
              Prelude.<$> triggerDetails,
            ("relatedItems" Core..=) Prelude.<$> relatedItems,
            ("impact" Core..=) Prelude.<$> impact,
            ("title" Core..=) Prelude.<$> title,
            Prelude.Just
              ("responsePlanArn" Core..= responsePlanArn)
          ]
      )

instance Core.ToPath StartIncident where
  toPath = Prelude.const "/startIncident"

instance Core.ToQuery StartIncident where
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

instance Prelude.NFData StartIncidentResponse
