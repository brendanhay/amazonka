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
-- Module      : Amazonka.SSMIncidents.GetResponsePlan
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of the specified response plan.
module Amazonka.SSMIncidents.GetResponsePlan
  ( -- * Creating a Request
    GetResponsePlan (..),
    newGetResponsePlan,

    -- * Request Lenses
    getResponsePlan_arn,

    -- * Destructuring the Response
    GetResponsePlanResponse (..),
    newGetResponsePlanResponse,

    -- * Response Lenses
    getResponsePlanResponse_integrations,
    getResponsePlanResponse_chatChannel,
    getResponsePlanResponse_displayName,
    getResponsePlanResponse_engagements,
    getResponsePlanResponse_actions,
    getResponsePlanResponse_httpStatus,
    getResponsePlanResponse_arn,
    getResponsePlanResponse_incidentTemplate,
    getResponsePlanResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newGetResponsePlan' smart constructor.
data GetResponsePlan = GetResponsePlan'
  { -- | The Amazon Resource Name (ARN) of the response plan.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResponsePlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getResponsePlan_arn' - The Amazon Resource Name (ARN) of the response plan.
newGetResponsePlan ::
  -- | 'arn'
  Prelude.Text ->
  GetResponsePlan
newGetResponsePlan pArn_ =
  GetResponsePlan' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the response plan.
getResponsePlan_arn :: Lens.Lens' GetResponsePlan Prelude.Text
getResponsePlan_arn = Lens.lens (\GetResponsePlan' {arn} -> arn) (\s@GetResponsePlan' {} a -> s {arn = a} :: GetResponsePlan)

instance Core.AWSRequest GetResponsePlan where
  type
    AWSResponse GetResponsePlan =
      GetResponsePlanResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResponsePlanResponse'
            Prelude.<$> (x Core..?> "integrations" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "chatChannel")
            Prelude.<*> (x Core..?> "displayName")
            Prelude.<*> (x Core..?> "engagements" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "actions" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "arn")
            Prelude.<*> (x Core..:> "incidentTemplate")
            Prelude.<*> (x Core..:> "name")
      )

instance Prelude.Hashable GetResponsePlan where
  hashWithSalt _salt GetResponsePlan' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData GetResponsePlan where
  rnf GetResponsePlan' {..} = Prelude.rnf arn

instance Core.ToHeaders GetResponsePlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetResponsePlan where
  toPath = Prelude.const "/getResponsePlan"

instance Core.ToQuery GetResponsePlan where
  toQuery GetResponsePlan' {..} =
    Prelude.mconcat ["arn" Core.=: arn]

-- | /See:/ 'newGetResponsePlanResponse' smart constructor.
data GetResponsePlanResponse = GetResponsePlanResponse'
  { -- | Information about third-party services integrated into the Incident
    -- Manager response plan.
    integrations :: Prelude.Maybe [Integration],
    -- | The Chatbot chat channel used for collaboration during an incident.
    chatChannel :: Prelude.Maybe ChatChannel,
    -- | The long format name of the response plan. Can contain spaces.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the contacts and escalation plans
    -- that the response plan engages during an incident.
    engagements :: Prelude.Maybe [Prelude.Text],
    -- | The actions that this response plan takes at the beginning of the
    -- incident.
    actions :: Prelude.Maybe [Action],
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the response plan.
    arn :: Prelude.Text,
    -- | Details used to create the incident when using this response plan.
    incidentTemplate :: IncidentTemplate,
    -- | The short format name of the response plan. The name can\'t contain
    -- spaces.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResponsePlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'integrations', 'getResponsePlanResponse_integrations' - Information about third-party services integrated into the Incident
-- Manager response plan.
--
-- 'chatChannel', 'getResponsePlanResponse_chatChannel' - The Chatbot chat channel used for collaboration during an incident.
--
-- 'displayName', 'getResponsePlanResponse_displayName' - The long format name of the response plan. Can contain spaces.
--
-- 'engagements', 'getResponsePlanResponse_engagements' - The Amazon Resource Name (ARN) for the contacts and escalation plans
-- that the response plan engages during an incident.
--
-- 'actions', 'getResponsePlanResponse_actions' - The actions that this response plan takes at the beginning of the
-- incident.
--
-- 'httpStatus', 'getResponsePlanResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getResponsePlanResponse_arn' - The ARN of the response plan.
--
-- 'incidentTemplate', 'getResponsePlanResponse_incidentTemplate' - Details used to create the incident when using this response plan.
--
-- 'name', 'getResponsePlanResponse_name' - The short format name of the response plan. The name can\'t contain
-- spaces.
newGetResponsePlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'incidentTemplate'
  IncidentTemplate ->
  -- | 'name'
  Prelude.Text ->
  GetResponsePlanResponse
newGetResponsePlanResponse
  pHttpStatus_
  pArn_
  pIncidentTemplate_
  pName_ =
    GetResponsePlanResponse'
      { integrations =
          Prelude.Nothing,
        chatChannel = Prelude.Nothing,
        displayName = Prelude.Nothing,
        engagements = Prelude.Nothing,
        actions = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        incidentTemplate = pIncidentTemplate_,
        name = pName_
      }

-- | Information about third-party services integrated into the Incident
-- Manager response plan.
getResponsePlanResponse_integrations :: Lens.Lens' GetResponsePlanResponse (Prelude.Maybe [Integration])
getResponsePlanResponse_integrations = Lens.lens (\GetResponsePlanResponse' {integrations} -> integrations) (\s@GetResponsePlanResponse' {} a -> s {integrations = a} :: GetResponsePlanResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Chatbot chat channel used for collaboration during an incident.
getResponsePlanResponse_chatChannel :: Lens.Lens' GetResponsePlanResponse (Prelude.Maybe ChatChannel)
getResponsePlanResponse_chatChannel = Lens.lens (\GetResponsePlanResponse' {chatChannel} -> chatChannel) (\s@GetResponsePlanResponse' {} a -> s {chatChannel = a} :: GetResponsePlanResponse)

-- | The long format name of the response plan. Can contain spaces.
getResponsePlanResponse_displayName :: Lens.Lens' GetResponsePlanResponse (Prelude.Maybe Prelude.Text)
getResponsePlanResponse_displayName = Lens.lens (\GetResponsePlanResponse' {displayName} -> displayName) (\s@GetResponsePlanResponse' {} a -> s {displayName = a} :: GetResponsePlanResponse)

-- | The Amazon Resource Name (ARN) for the contacts and escalation plans
-- that the response plan engages during an incident.
getResponsePlanResponse_engagements :: Lens.Lens' GetResponsePlanResponse (Prelude.Maybe [Prelude.Text])
getResponsePlanResponse_engagements = Lens.lens (\GetResponsePlanResponse' {engagements} -> engagements) (\s@GetResponsePlanResponse' {} a -> s {engagements = a} :: GetResponsePlanResponse) Prelude.. Lens.mapping Lens.coerced

-- | The actions that this response plan takes at the beginning of the
-- incident.
getResponsePlanResponse_actions :: Lens.Lens' GetResponsePlanResponse (Prelude.Maybe [Action])
getResponsePlanResponse_actions = Lens.lens (\GetResponsePlanResponse' {actions} -> actions) (\s@GetResponsePlanResponse' {} a -> s {actions = a} :: GetResponsePlanResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getResponsePlanResponse_httpStatus :: Lens.Lens' GetResponsePlanResponse Prelude.Int
getResponsePlanResponse_httpStatus = Lens.lens (\GetResponsePlanResponse' {httpStatus} -> httpStatus) (\s@GetResponsePlanResponse' {} a -> s {httpStatus = a} :: GetResponsePlanResponse)

-- | The ARN of the response plan.
getResponsePlanResponse_arn :: Lens.Lens' GetResponsePlanResponse Prelude.Text
getResponsePlanResponse_arn = Lens.lens (\GetResponsePlanResponse' {arn} -> arn) (\s@GetResponsePlanResponse' {} a -> s {arn = a} :: GetResponsePlanResponse)

-- | Details used to create the incident when using this response plan.
getResponsePlanResponse_incidentTemplate :: Lens.Lens' GetResponsePlanResponse IncidentTemplate
getResponsePlanResponse_incidentTemplate = Lens.lens (\GetResponsePlanResponse' {incidentTemplate} -> incidentTemplate) (\s@GetResponsePlanResponse' {} a -> s {incidentTemplate = a} :: GetResponsePlanResponse)

-- | The short format name of the response plan. The name can\'t contain
-- spaces.
getResponsePlanResponse_name :: Lens.Lens' GetResponsePlanResponse Prelude.Text
getResponsePlanResponse_name = Lens.lens (\GetResponsePlanResponse' {name} -> name) (\s@GetResponsePlanResponse' {} a -> s {name = a} :: GetResponsePlanResponse)

instance Prelude.NFData GetResponsePlanResponse where
  rnf GetResponsePlanResponse' {..} =
    Prelude.rnf integrations
      `Prelude.seq` Prelude.rnf chatChannel
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf engagements
      `Prelude.seq` Prelude.rnf actions
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf incidentTemplate
      `Prelude.seq` Prelude.rnf name
