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
-- Module      : Amazonka.ChimeSdkVoice.CreateSipRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.CreateSipRule
  ( -- * Creating a Request
    CreateSipRule (..),
    newCreateSipRule,

    -- * Request Lenses
    createSipRule_disabled,
    createSipRule_targetApplications,
    createSipRule_name,
    createSipRule_triggerType,
    createSipRule_triggerValue,

    -- * Destructuring the Response
    CreateSipRuleResponse (..),
    newCreateSipRuleResponse,

    -- * Response Lenses
    createSipRuleResponse_sipRule,
    createSipRuleResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateSipRule' smart constructor.
data CreateSipRule = CreateSipRule'
  { disabled :: Prelude.Maybe Prelude.Bool,
    targetApplications :: Prelude.Maybe (Prelude.NonEmpty SipRuleTargetApplication),
    name :: Prelude.Text,
    triggerType :: SipRuleTriggerType,
    triggerValue :: Prelude.Text
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
-- 'disabled', 'createSipRule_disabled' - Undocumented member.
--
-- 'targetApplications', 'createSipRule_targetApplications' - Undocumented member.
--
-- 'name', 'createSipRule_name' - Undocumented member.
--
-- 'triggerType', 'createSipRule_triggerType' - Undocumented member.
--
-- 'triggerValue', 'createSipRule_triggerValue' - Undocumented member.
newCreateSipRule ::
  -- | 'name'
  Prelude.Text ->
  -- | 'triggerType'
  SipRuleTriggerType ->
  -- | 'triggerValue'
  Prelude.Text ->
  CreateSipRule
newCreateSipRule pName_ pTriggerType_ pTriggerValue_ =
  CreateSipRule'
    { disabled = Prelude.Nothing,
      targetApplications = Prelude.Nothing,
      name = pName_,
      triggerType = pTriggerType_,
      triggerValue = pTriggerValue_
    }

-- | Undocumented member.
createSipRule_disabled :: Lens.Lens' CreateSipRule (Prelude.Maybe Prelude.Bool)
createSipRule_disabled = Lens.lens (\CreateSipRule' {disabled} -> disabled) (\s@CreateSipRule' {} a -> s {disabled = a} :: CreateSipRule)

-- | Undocumented member.
createSipRule_targetApplications :: Lens.Lens' CreateSipRule (Prelude.Maybe (Prelude.NonEmpty SipRuleTargetApplication))
createSipRule_targetApplications = Lens.lens (\CreateSipRule' {targetApplications} -> targetApplications) (\s@CreateSipRule' {} a -> s {targetApplications = a} :: CreateSipRule) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
createSipRule_name :: Lens.Lens' CreateSipRule Prelude.Text
createSipRule_name = Lens.lens (\CreateSipRule' {name} -> name) (\s@CreateSipRule' {} a -> s {name = a} :: CreateSipRule)

-- | Undocumented member.
createSipRule_triggerType :: Lens.Lens' CreateSipRule SipRuleTriggerType
createSipRule_triggerType = Lens.lens (\CreateSipRule' {triggerType} -> triggerType) (\s@CreateSipRule' {} a -> s {triggerType = a} :: CreateSipRule)

-- | Undocumented member.
createSipRule_triggerValue :: Lens.Lens' CreateSipRule Prelude.Text
createSipRule_triggerValue = Lens.lens (\CreateSipRule' {triggerValue} -> triggerValue) (\s@CreateSipRule' {} a -> s {triggerValue = a} :: CreateSipRule)

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
            Prelude.<$> (x Data..?> "SipRule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateSipRule where
  hashWithSalt _salt CreateSipRule' {..} =
    _salt
      `Prelude.hashWithSalt` disabled
      `Prelude.hashWithSalt` targetApplications
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` triggerType
      `Prelude.hashWithSalt` triggerValue

instance Prelude.NFData CreateSipRule where
  rnf CreateSipRule' {..} =
    Prelude.rnf disabled
      `Prelude.seq` Prelude.rnf targetApplications
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf triggerType
      `Prelude.seq` Prelude.rnf triggerValue

instance Data.ToHeaders CreateSipRule where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON CreateSipRule where
  toJSON CreateSipRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Disabled" Data..=) Prelude.<$> disabled,
            ("TargetApplications" Data..=)
              Prelude.<$> targetApplications,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("TriggerType" Data..= triggerType),
            Prelude.Just ("TriggerValue" Data..= triggerValue)
          ]
      )

instance Data.ToPath CreateSipRule where
  toPath = Prelude.const "/sip-rules"

instance Data.ToQuery CreateSipRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateSipRuleResponse' smart constructor.
data CreateSipRuleResponse = CreateSipRuleResponse'
  { sipRule :: Prelude.Maybe SipRule,
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
-- 'sipRule', 'createSipRuleResponse_sipRule' - Undocumented member.
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

-- | Undocumented member.
createSipRuleResponse_sipRule :: Lens.Lens' CreateSipRuleResponse (Prelude.Maybe SipRule)
createSipRuleResponse_sipRule = Lens.lens (\CreateSipRuleResponse' {sipRule} -> sipRule) (\s@CreateSipRuleResponse' {} a -> s {sipRule = a} :: CreateSipRuleResponse)

-- | The response's http status code.
createSipRuleResponse_httpStatus :: Lens.Lens' CreateSipRuleResponse Prelude.Int
createSipRuleResponse_httpStatus = Lens.lens (\CreateSipRuleResponse' {httpStatus} -> httpStatus) (\s@CreateSipRuleResponse' {} a -> s {httpStatus = a} :: CreateSipRuleResponse)

instance Prelude.NFData CreateSipRuleResponse where
  rnf CreateSipRuleResponse' {..} =
    Prelude.rnf sipRule
      `Prelude.seq` Prelude.rnf httpStatus
