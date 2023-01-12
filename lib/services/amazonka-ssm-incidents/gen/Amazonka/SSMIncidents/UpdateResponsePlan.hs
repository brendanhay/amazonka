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
-- Module      : Amazonka.SSMIncidents.UpdateResponsePlan
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified response plan.
module Amazonka.SSMIncidents.UpdateResponsePlan
  ( -- * Creating a Request
    UpdateResponsePlan (..),
    newUpdateResponsePlan,

    -- * Request Lenses
    updateResponsePlan_actions,
    updateResponsePlan_chatChannel,
    updateResponsePlan_clientToken,
    updateResponsePlan_displayName,
    updateResponsePlan_engagements,
    updateResponsePlan_incidentTemplateDedupeString,
    updateResponsePlan_incidentTemplateImpact,
    updateResponsePlan_incidentTemplateNotificationTargets,
    updateResponsePlan_incidentTemplateSummary,
    updateResponsePlan_incidentTemplateTags,
    updateResponsePlan_incidentTemplateTitle,
    updateResponsePlan_integrations,
    updateResponsePlan_arn,

    -- * Destructuring the Response
    UpdateResponsePlanResponse (..),
    newUpdateResponsePlanResponse,

    -- * Response Lenses
    updateResponsePlanResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newUpdateResponsePlan' smart constructor.
data UpdateResponsePlan = UpdateResponsePlan'
  { -- | The actions that this response plan takes at the beginning of an
    -- incident.
    actions :: Prelude.Maybe [Action],
    -- | The Chatbot chat channel used for collaboration during an incident.
    --
    -- Use the empty structure to remove the chat channel from the response
    -- plan.
    chatChannel :: Prelude.Maybe ChatChannel,
    -- | A token ensuring that the operation is called only once with the
    -- specified details.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The long format name of the response plan. The display name can\'t
    -- contain spaces.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) for the contacts and escalation plans
    -- that the response plan engages during an incident.
    engagements :: Prelude.Maybe [Prelude.Text],
    -- | The string Incident Manager uses to prevent duplicate incidents from
    -- being created by the same incident in the same account.
    incidentTemplateDedupeString :: Prelude.Maybe Prelude.Text,
    -- | Defines the impact to the customers. Providing an impact overwrites the
    -- impact provided by a response plan.
    --
    -- __Possible impacts:__
    --
    -- -   @5@ - Severe impact
    --
    -- -   @4@ - High impact
    --
    -- -   @3@ - Medium impact
    --
    -- -   @2@ - Low impact
    --
    -- -   @1@ - No impact
    incidentTemplateImpact :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon SNS targets that are notified when updates are made to an
    -- incident.
    incidentTemplateNotificationTargets :: Prelude.Maybe [NotificationTargetItem],
    -- | A brief summary of the incident. This typically contains what has
    -- happened, what\'s currently happening, and next steps.
    incidentTemplateSummary :: Prelude.Maybe Prelude.Text,
    -- | Tags to assign to the template. When the @StartIncident@ API action is
    -- called, Incident Manager assigns the tags specified in the template to
    -- the incident. To call this action, you must also have permission to call
    -- the @TagResource@ API action for the incident record resource.
    incidentTemplateTags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The short format name of the incident. The title can\'t contain spaces.
    incidentTemplateTitle :: Prelude.Maybe Prelude.Text,
    -- | Information about third-party services integrated into the response
    -- plan.
    integrations :: Prelude.Maybe [Integration],
    -- | The Amazon Resource Name (ARN) of the response plan.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResponsePlan' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'actions', 'updateResponsePlan_actions' - The actions that this response plan takes at the beginning of an
-- incident.
--
-- 'chatChannel', 'updateResponsePlan_chatChannel' - The Chatbot chat channel used for collaboration during an incident.
--
-- Use the empty structure to remove the chat channel from the response
-- plan.
--
-- 'clientToken', 'updateResponsePlan_clientToken' - A token ensuring that the operation is called only once with the
-- specified details.
--
-- 'displayName', 'updateResponsePlan_displayName' - The long format name of the response plan. The display name can\'t
-- contain spaces.
--
-- 'engagements', 'updateResponsePlan_engagements' - The Amazon Resource Name (ARN) for the contacts and escalation plans
-- that the response plan engages during an incident.
--
-- 'incidentTemplateDedupeString', 'updateResponsePlan_incidentTemplateDedupeString' - The string Incident Manager uses to prevent duplicate incidents from
-- being created by the same incident in the same account.
--
-- 'incidentTemplateImpact', 'updateResponsePlan_incidentTemplateImpact' - Defines the impact to the customers. Providing an impact overwrites the
-- impact provided by a response plan.
--
-- __Possible impacts:__
--
-- -   @5@ - Severe impact
--
-- -   @4@ - High impact
--
-- -   @3@ - Medium impact
--
-- -   @2@ - Low impact
--
-- -   @1@ - No impact
--
-- 'incidentTemplateNotificationTargets', 'updateResponsePlan_incidentTemplateNotificationTargets' - The Amazon SNS targets that are notified when updates are made to an
-- incident.
--
-- 'incidentTemplateSummary', 'updateResponsePlan_incidentTemplateSummary' - A brief summary of the incident. This typically contains what has
-- happened, what\'s currently happening, and next steps.
--
-- 'incidentTemplateTags', 'updateResponsePlan_incidentTemplateTags' - Tags to assign to the template. When the @StartIncident@ API action is
-- called, Incident Manager assigns the tags specified in the template to
-- the incident. To call this action, you must also have permission to call
-- the @TagResource@ API action for the incident record resource.
--
-- 'incidentTemplateTitle', 'updateResponsePlan_incidentTemplateTitle' - The short format name of the incident. The title can\'t contain spaces.
--
-- 'integrations', 'updateResponsePlan_integrations' - Information about third-party services integrated into the response
-- plan.
--
-- 'arn', 'updateResponsePlan_arn' - The Amazon Resource Name (ARN) of the response plan.
newUpdateResponsePlan ::
  -- | 'arn'
  Prelude.Text ->
  UpdateResponsePlan
newUpdateResponsePlan pArn_ =
  UpdateResponsePlan'
    { actions = Prelude.Nothing,
      chatChannel = Prelude.Nothing,
      clientToken = Prelude.Nothing,
      displayName = Prelude.Nothing,
      engagements = Prelude.Nothing,
      incidentTemplateDedupeString = Prelude.Nothing,
      incidentTemplateImpact = Prelude.Nothing,
      incidentTemplateNotificationTargets =
        Prelude.Nothing,
      incidentTemplateSummary = Prelude.Nothing,
      incidentTemplateTags = Prelude.Nothing,
      incidentTemplateTitle = Prelude.Nothing,
      integrations = Prelude.Nothing,
      arn = pArn_
    }

-- | The actions that this response plan takes at the beginning of an
-- incident.
updateResponsePlan_actions :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe [Action])
updateResponsePlan_actions = Lens.lens (\UpdateResponsePlan' {actions} -> actions) (\s@UpdateResponsePlan' {} a -> s {actions = a} :: UpdateResponsePlan) Prelude.. Lens.mapping Lens.coerced

-- | The Chatbot chat channel used for collaboration during an incident.
--
-- Use the empty structure to remove the chat channel from the response
-- plan.
updateResponsePlan_chatChannel :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe ChatChannel)
updateResponsePlan_chatChannel = Lens.lens (\UpdateResponsePlan' {chatChannel} -> chatChannel) (\s@UpdateResponsePlan' {} a -> s {chatChannel = a} :: UpdateResponsePlan)

-- | A token ensuring that the operation is called only once with the
-- specified details.
updateResponsePlan_clientToken :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe Prelude.Text)
updateResponsePlan_clientToken = Lens.lens (\UpdateResponsePlan' {clientToken} -> clientToken) (\s@UpdateResponsePlan' {} a -> s {clientToken = a} :: UpdateResponsePlan)

-- | The long format name of the response plan. The display name can\'t
-- contain spaces.
updateResponsePlan_displayName :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe Prelude.Text)
updateResponsePlan_displayName = Lens.lens (\UpdateResponsePlan' {displayName} -> displayName) (\s@UpdateResponsePlan' {} a -> s {displayName = a} :: UpdateResponsePlan)

-- | The Amazon Resource Name (ARN) for the contacts and escalation plans
-- that the response plan engages during an incident.
updateResponsePlan_engagements :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe [Prelude.Text])
updateResponsePlan_engagements = Lens.lens (\UpdateResponsePlan' {engagements} -> engagements) (\s@UpdateResponsePlan' {} a -> s {engagements = a} :: UpdateResponsePlan) Prelude.. Lens.mapping Lens.coerced

-- | The string Incident Manager uses to prevent duplicate incidents from
-- being created by the same incident in the same account.
updateResponsePlan_incidentTemplateDedupeString :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe Prelude.Text)
updateResponsePlan_incidentTemplateDedupeString = Lens.lens (\UpdateResponsePlan' {incidentTemplateDedupeString} -> incidentTemplateDedupeString) (\s@UpdateResponsePlan' {} a -> s {incidentTemplateDedupeString = a} :: UpdateResponsePlan)

-- | Defines the impact to the customers. Providing an impact overwrites the
-- impact provided by a response plan.
--
-- __Possible impacts:__
--
-- -   @5@ - Severe impact
--
-- -   @4@ - High impact
--
-- -   @3@ - Medium impact
--
-- -   @2@ - Low impact
--
-- -   @1@ - No impact
updateResponsePlan_incidentTemplateImpact :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe Prelude.Natural)
updateResponsePlan_incidentTemplateImpact = Lens.lens (\UpdateResponsePlan' {incidentTemplateImpact} -> incidentTemplateImpact) (\s@UpdateResponsePlan' {} a -> s {incidentTemplateImpact = a} :: UpdateResponsePlan)

-- | The Amazon SNS targets that are notified when updates are made to an
-- incident.
updateResponsePlan_incidentTemplateNotificationTargets :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe [NotificationTargetItem])
updateResponsePlan_incidentTemplateNotificationTargets = Lens.lens (\UpdateResponsePlan' {incidentTemplateNotificationTargets} -> incidentTemplateNotificationTargets) (\s@UpdateResponsePlan' {} a -> s {incidentTemplateNotificationTargets = a} :: UpdateResponsePlan) Prelude.. Lens.mapping Lens.coerced

-- | A brief summary of the incident. This typically contains what has
-- happened, what\'s currently happening, and next steps.
updateResponsePlan_incidentTemplateSummary :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe Prelude.Text)
updateResponsePlan_incidentTemplateSummary = Lens.lens (\UpdateResponsePlan' {incidentTemplateSummary} -> incidentTemplateSummary) (\s@UpdateResponsePlan' {} a -> s {incidentTemplateSummary = a} :: UpdateResponsePlan)

-- | Tags to assign to the template. When the @StartIncident@ API action is
-- called, Incident Manager assigns the tags specified in the template to
-- the incident. To call this action, you must also have permission to call
-- the @TagResource@ API action for the incident record resource.
updateResponsePlan_incidentTemplateTags :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
updateResponsePlan_incidentTemplateTags = Lens.lens (\UpdateResponsePlan' {incidentTemplateTags} -> incidentTemplateTags) (\s@UpdateResponsePlan' {} a -> s {incidentTemplateTags = a} :: UpdateResponsePlan) Prelude.. Lens.mapping Lens.coerced

-- | The short format name of the incident. The title can\'t contain spaces.
updateResponsePlan_incidentTemplateTitle :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe Prelude.Text)
updateResponsePlan_incidentTemplateTitle = Lens.lens (\UpdateResponsePlan' {incidentTemplateTitle} -> incidentTemplateTitle) (\s@UpdateResponsePlan' {} a -> s {incidentTemplateTitle = a} :: UpdateResponsePlan)

-- | Information about third-party services integrated into the response
-- plan.
updateResponsePlan_integrations :: Lens.Lens' UpdateResponsePlan (Prelude.Maybe [Integration])
updateResponsePlan_integrations = Lens.lens (\UpdateResponsePlan' {integrations} -> integrations) (\s@UpdateResponsePlan' {} a -> s {integrations = a} :: UpdateResponsePlan) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the response plan.
updateResponsePlan_arn :: Lens.Lens' UpdateResponsePlan Prelude.Text
updateResponsePlan_arn = Lens.lens (\UpdateResponsePlan' {arn} -> arn) (\s@UpdateResponsePlan' {} a -> s {arn = a} :: UpdateResponsePlan)

instance Core.AWSRequest UpdateResponsePlan where
  type
    AWSResponse UpdateResponsePlan =
      UpdateResponsePlanResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateResponsePlanResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateResponsePlan where
  hashWithSalt _salt UpdateResponsePlan' {..} =
    _salt `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` chatChannel
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` engagements
      `Prelude.hashWithSalt` incidentTemplateDedupeString
      `Prelude.hashWithSalt` incidentTemplateImpact
      `Prelude.hashWithSalt` incidentTemplateNotificationTargets
      `Prelude.hashWithSalt` incidentTemplateSummary
      `Prelude.hashWithSalt` incidentTemplateTags
      `Prelude.hashWithSalt` incidentTemplateTitle
      `Prelude.hashWithSalt` integrations
      `Prelude.hashWithSalt` arn

instance Prelude.NFData UpdateResponsePlan where
  rnf UpdateResponsePlan' {..} =
    Prelude.rnf actions
      `Prelude.seq` Prelude.rnf chatChannel
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf engagements
      `Prelude.seq` Prelude.rnf incidentTemplateDedupeString
      `Prelude.seq` Prelude.rnf incidentTemplateImpact
      `Prelude.seq` Prelude.rnf incidentTemplateNotificationTargets
      `Prelude.seq` Prelude.rnf incidentTemplateSummary
      `Prelude.seq` Prelude.rnf incidentTemplateTags
      `Prelude.seq` Prelude.rnf incidentTemplateTitle
      `Prelude.seq` Prelude.rnf integrations
      `Prelude.seq` Prelude.rnf arn

instance Data.ToHeaders UpdateResponsePlan where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateResponsePlan where
  toJSON UpdateResponsePlan' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("actions" Data..=) Prelude.<$> actions,
            ("chatChannel" Data..=) Prelude.<$> chatChannel,
            ("clientToken" Data..=) Prelude.<$> clientToken,
            ("displayName" Data..=) Prelude.<$> displayName,
            ("engagements" Data..=) Prelude.<$> engagements,
            ("incidentTemplateDedupeString" Data..=)
              Prelude.<$> incidentTemplateDedupeString,
            ("incidentTemplateImpact" Data..=)
              Prelude.<$> incidentTemplateImpact,
            ("incidentTemplateNotificationTargets" Data..=)
              Prelude.<$> incidentTemplateNotificationTargets,
            ("incidentTemplateSummary" Data..=)
              Prelude.<$> incidentTemplateSummary,
            ("incidentTemplateTags" Data..=)
              Prelude.<$> incidentTemplateTags,
            ("incidentTemplateTitle" Data..=)
              Prelude.<$> incidentTemplateTitle,
            ("integrations" Data..=) Prelude.<$> integrations,
            Prelude.Just ("arn" Data..= arn)
          ]
      )

instance Data.ToPath UpdateResponsePlan where
  toPath = Prelude.const "/updateResponsePlan"

instance Data.ToQuery UpdateResponsePlan where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateResponsePlanResponse' smart constructor.
data UpdateResponsePlanResponse = UpdateResponsePlanResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResponsePlanResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateResponsePlanResponse_httpStatus' - The response's http status code.
newUpdateResponsePlanResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateResponsePlanResponse
newUpdateResponsePlanResponse pHttpStatus_ =
  UpdateResponsePlanResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateResponsePlanResponse_httpStatus :: Lens.Lens' UpdateResponsePlanResponse Prelude.Int
updateResponsePlanResponse_httpStatus = Lens.lens (\UpdateResponsePlanResponse' {httpStatus} -> httpStatus) (\s@UpdateResponsePlanResponse' {} a -> s {httpStatus = a} :: UpdateResponsePlanResponse)

instance Prelude.NFData UpdateResponsePlanResponse where
  rnf UpdateResponsePlanResponse' {..} =
    Prelude.rnf httpStatus
