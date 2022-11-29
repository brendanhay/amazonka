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
-- Module      : Amazonka.Chime.CreateSipRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a SIP rule which can be used to run a SIP media application as a
-- target for a specific trigger type.
module Amazonka.Chime.CreateSipRule
  ( -- * Creating a Request
    CreateSipRule (..),
    newCreateSipRule,

    -- * Request Lenses
    createSipRule_disabled,
    createSipRule_name,
    createSipRule_triggerType,
    createSipRule_triggerValue,
    createSipRule_targetApplications,

    -- * Destructuring the Response
    CreateSipRuleResponse (..),
    newCreateSipRuleResponse,

    -- * Response Lenses
    createSipRuleResponse_sipRule,
    createSipRuleResponse_httpStatus,
  )
where

import Amazonka.Chime.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSipRule' smart constructor.
data CreateSipRule = CreateSipRule'
  { -- | Enables or disables a rule. You must disable rules before you can delete
    -- them.
    disabled :: Prelude.Maybe Prelude.Bool,
    -- | The name of the SIP rule.
    name :: Prelude.Text,
    -- | The type of trigger assigned to the SIP rule in @TriggerValue@,
    -- currently @RequestUriHostname@ or @ToPhoneNumber@.
    triggerType :: SipRuleTriggerType,
    -- | If @TriggerType@ is @RequestUriHostname@, the value can be the outbound
    -- host name of an Amazon Chime Voice Connector. If @TriggerType@ is
    -- @ToPhoneNumber@, the value can be a customer-owned phone number in the
    -- E164 format. The @SipMediaApplication@ specified in the @SipRule@ is
    -- triggered if the request URI in an incoming SIP request matches the
    -- @RequestUriHostname@, or if the @To@ header in the incoming SIP request
    -- matches the @ToPhoneNumber@ value.
    triggerValue :: Prelude.Text,
    -- | List of SIP media applications with priority and AWS Region. Only one
    -- SIP application per AWS Region can be used.
    targetApplications :: Prelude.NonEmpty SipRuleTargetApplication
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSipRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disabled', 'createSipRule_disabled' - Enables or disables a rule. You must disable rules before you can delete
-- them.
--
-- 'name', 'createSipRule_name' - The name of the SIP rule.
--
-- 'triggerType', 'createSipRule_triggerType' - The type of trigger assigned to the SIP rule in @TriggerValue@,
-- currently @RequestUriHostname@ or @ToPhoneNumber@.
--
-- 'triggerValue', 'createSipRule_triggerValue' - If @TriggerType@ is @RequestUriHostname@, the value can be the outbound
-- host name of an Amazon Chime Voice Connector. If @TriggerType@ is
-- @ToPhoneNumber@, the value can be a customer-owned phone number in the
-- E164 format. The @SipMediaApplication@ specified in the @SipRule@ is
-- triggered if the request URI in an incoming SIP request matches the
-- @RequestUriHostname@, or if the @To@ header in the incoming SIP request
-- matches the @ToPhoneNumber@ value.
--
-- 'targetApplications', 'createSipRule_targetApplications' - List of SIP media applications with priority and AWS Region. Only one
-- SIP application per AWS Region can be used.
newCreateSipRule ::
  -- | 'name'
  Prelude.Text ->
  -- | 'triggerType'
  SipRuleTriggerType ->
  -- | 'triggerValue'
  Prelude.Text ->
  -- | 'targetApplications'
  Prelude.NonEmpty SipRuleTargetApplication ->
  CreateSipRule
newCreateSipRule
  pName_
  pTriggerType_
  pTriggerValue_
  pTargetApplications_ =
    CreateSipRule'
      { disabled = Prelude.Nothing,
        name = pName_,
        triggerType = pTriggerType_,
        triggerValue = pTriggerValue_,
        targetApplications =
          Lens.coerced Lens.# pTargetApplications_
      }

-- | Enables or disables a rule. You must disable rules before you can delete
-- them.
createSipRule_disabled :: Lens.Lens' CreateSipRule (Prelude.Maybe Prelude.Bool)
createSipRule_disabled = Lens.lens (\CreateSipRule' {disabled} -> disabled) (\s@CreateSipRule' {} a -> s {disabled = a} :: CreateSipRule)

-- | The name of the SIP rule.
createSipRule_name :: Lens.Lens' CreateSipRule Prelude.Text
createSipRule_name = Lens.lens (\CreateSipRule' {name} -> name) (\s@CreateSipRule' {} a -> s {name = a} :: CreateSipRule)

-- | The type of trigger assigned to the SIP rule in @TriggerValue@,
-- currently @RequestUriHostname@ or @ToPhoneNumber@.
createSipRule_triggerType :: Lens.Lens' CreateSipRule SipRuleTriggerType
createSipRule_triggerType = Lens.lens (\CreateSipRule' {triggerType} -> triggerType) (\s@CreateSipRule' {} a -> s {triggerType = a} :: CreateSipRule)

-- | If @TriggerType@ is @RequestUriHostname@, the value can be the outbound
-- host name of an Amazon Chime Voice Connector. If @TriggerType@ is
-- @ToPhoneNumber@, the value can be a customer-owned phone number in the
-- E164 format. The @SipMediaApplication@ specified in the @SipRule@ is
-- triggered if the request URI in an incoming SIP request matches the
-- @RequestUriHostname@, or if the @To@ header in the incoming SIP request
-- matches the @ToPhoneNumber@ value.
createSipRule_triggerValue :: Lens.Lens' CreateSipRule Prelude.Text
createSipRule_triggerValue = Lens.lens (\CreateSipRule' {triggerValue} -> triggerValue) (\s@CreateSipRule' {} a -> s {triggerValue = a} :: CreateSipRule)

-- | List of SIP media applications with priority and AWS Region. Only one
-- SIP application per AWS Region can be used.
createSipRule_targetApplications :: Lens.Lens' CreateSipRule (Prelude.NonEmpty SipRuleTargetApplication)
createSipRule_targetApplications = Lens.lens (\CreateSipRule' {targetApplications} -> targetApplications) (\s@CreateSipRule' {} a -> s {targetApplications = a} :: CreateSipRule) Prelude.. Lens.coerced

instance Core.AWSRequest CreateSipRule where
  type
    AWSResponse CreateSipRule =
      CreateSipRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateSipRuleResponse'
            Prelude.<$> (x Core..?> "SipRule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSipRule where
  hashWithSalt _salt CreateSipRule' {..} =
    _salt `Prelude.hashWithSalt` disabled
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` triggerType
      `Prelude.hashWithSalt` triggerValue
      `Prelude.hashWithSalt` targetApplications

instance Prelude.NFData CreateSipRule where
  rnf CreateSipRule' {..} =
    Prelude.rnf disabled
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf triggerType
      `Prelude.seq` Prelude.rnf triggerValue
      `Prelude.seq` Prelude.rnf targetApplications

instance Core.ToHeaders CreateSipRule where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToJSON CreateSipRule where
  toJSON CreateSipRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Disabled" Core..=) Prelude.<$> disabled,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just ("TriggerType" Core..= triggerType),
            Prelude.Just ("TriggerValue" Core..= triggerValue),
            Prelude.Just
              ("TargetApplications" Core..= targetApplications)
          ]
      )

instance Core.ToPath CreateSipRule where
  toPath = Prelude.const "/sip-rules"

instance Core.ToQuery CreateSipRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSipRuleResponse' smart constructor.
data CreateSipRuleResponse = CreateSipRuleResponse'
  { -- | Returns the SIP rule information, including the rule ID, triggers, and
    -- target applications.
    sipRule :: Prelude.Maybe SipRule,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateSipRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sipRule', 'createSipRuleResponse_sipRule' - Returns the SIP rule information, including the rule ID, triggers, and
-- target applications.
--
-- 'httpStatus', 'createSipRuleResponse_httpStatus' - The response's http status code.
newCreateSipRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateSipRuleResponse
newCreateSipRuleResponse pHttpStatus_ =
  CreateSipRuleResponse'
    { sipRule = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns the SIP rule information, including the rule ID, triggers, and
-- target applications.
createSipRuleResponse_sipRule :: Lens.Lens' CreateSipRuleResponse (Prelude.Maybe SipRule)
createSipRuleResponse_sipRule = Lens.lens (\CreateSipRuleResponse' {sipRule} -> sipRule) (\s@CreateSipRuleResponse' {} a -> s {sipRule = a} :: CreateSipRuleResponse)

-- | The response's http status code.
createSipRuleResponse_httpStatus :: Lens.Lens' CreateSipRuleResponse Prelude.Int
createSipRuleResponse_httpStatus = Lens.lens (\CreateSipRuleResponse' {httpStatus} -> httpStatus) (\s@CreateSipRuleResponse' {} a -> s {httpStatus = a} :: CreateSipRuleResponse)

instance Prelude.NFData CreateSipRuleResponse where
  rnf CreateSipRuleResponse' {..} =
    Prelude.rnf sipRule
      `Prelude.seq` Prelude.rnf httpStatus
