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
-- Module      : Amazonka.ChimeSdkVoice.UpdateSipRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- -- | Undocumented operation.
module Amazonka.ChimeSdkVoice.UpdateSipRule
  ( -- * Creating a Request
    UpdateSipRule (..),
    newUpdateSipRule,

    -- * Request Lenses
    updateSipRule_disabled,
    updateSipRule_targetApplications,
    updateSipRule_sipRuleId,
    updateSipRule_name,

    -- * Destructuring the Response
    UpdateSipRuleResponse (..),
    newUpdateSipRuleResponse,

    -- * Response Lenses
    updateSipRuleResponse_sipRule,
    updateSipRuleResponse_httpStatus,
  )
where

import Amazonka.ChimeSdkVoice.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSipRule' smart constructor.
data UpdateSipRule = UpdateSipRule'
  { disabled :: Prelude.Maybe Prelude.Bool,
    targetApplications :: Prelude.Maybe (Prelude.NonEmpty SipRuleTargetApplication),
    sipRuleId :: Prelude.Text,
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSipRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'disabled', 'updateSipRule_disabled' - Undocumented member.
--
-- 'targetApplications', 'updateSipRule_targetApplications' - Undocumented member.
--
-- 'sipRuleId', 'updateSipRule_sipRuleId' - Undocumented member.
--
-- 'name', 'updateSipRule_name' - Undocumented member.
newUpdateSipRule ::
  -- | 'sipRuleId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  UpdateSipRule
newUpdateSipRule pSipRuleId_ pName_ =
  UpdateSipRule'
    { disabled = Prelude.Nothing,
      targetApplications = Prelude.Nothing,
      sipRuleId = pSipRuleId_,
      name = pName_
    }

-- | Undocumented member.
updateSipRule_disabled :: Lens.Lens' UpdateSipRule (Prelude.Maybe Prelude.Bool)
updateSipRule_disabled = Lens.lens (\UpdateSipRule' {disabled} -> disabled) (\s@UpdateSipRule' {} a -> s {disabled = a} :: UpdateSipRule)

-- | Undocumented member.
updateSipRule_targetApplications :: Lens.Lens' UpdateSipRule (Prelude.Maybe (Prelude.NonEmpty SipRuleTargetApplication))
updateSipRule_targetApplications = Lens.lens (\UpdateSipRule' {targetApplications} -> targetApplications) (\s@UpdateSipRule' {} a -> s {targetApplications = a} :: UpdateSipRule) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateSipRule_sipRuleId :: Lens.Lens' UpdateSipRule Prelude.Text
updateSipRule_sipRuleId = Lens.lens (\UpdateSipRule' {sipRuleId} -> sipRuleId) (\s@UpdateSipRule' {} a -> s {sipRuleId = a} :: UpdateSipRule)

-- | Undocumented member.
updateSipRule_name :: Lens.Lens' UpdateSipRule Prelude.Text
updateSipRule_name = Lens.lens (\UpdateSipRule' {name} -> name) (\s@UpdateSipRule' {} a -> s {name = a} :: UpdateSipRule)

instance Core.AWSRequest UpdateSipRule where
  type
    AWSResponse UpdateSipRule =
      UpdateSipRuleResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSipRuleResponse'
            Prelude.<$> (x Data..?> "SipRule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSipRule where
  hashWithSalt _salt UpdateSipRule' {..} =
    _salt
      `Prelude.hashWithSalt` disabled
      `Prelude.hashWithSalt` targetApplications
      `Prelude.hashWithSalt` sipRuleId
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateSipRule where
  rnf UpdateSipRule' {..} =
    Prelude.rnf disabled
      `Prelude.seq` Prelude.rnf targetApplications
      `Prelude.seq` Prelude.rnf sipRuleId
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateSipRule where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON UpdateSipRule where
  toJSON UpdateSipRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Disabled" Data..=) Prelude.<$> disabled,
            ("TargetApplications" Data..=)
              Prelude.<$> targetApplications,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath UpdateSipRule where
  toPath UpdateSipRule' {..} =
    Prelude.mconcat
      ["/sip-rules/", Data.toBS sipRuleId]

instance Data.ToQuery UpdateSipRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSipRuleResponse' smart constructor.
data UpdateSipRuleResponse = UpdateSipRuleResponse'
  { sipRule :: Prelude.Maybe SipRule,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSipRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sipRule', 'updateSipRuleResponse_sipRule' - Undocumented member.
--
-- 'httpStatus', 'updateSipRuleResponse_httpStatus' - The response's http status code.
newUpdateSipRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSipRuleResponse
newUpdateSipRuleResponse pHttpStatus_ =
  UpdateSipRuleResponse'
    { sipRule = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateSipRuleResponse_sipRule :: Lens.Lens' UpdateSipRuleResponse (Prelude.Maybe SipRule)
updateSipRuleResponse_sipRule = Lens.lens (\UpdateSipRuleResponse' {sipRule} -> sipRule) (\s@UpdateSipRuleResponse' {} a -> s {sipRule = a} :: UpdateSipRuleResponse)

-- | The response's http status code.
updateSipRuleResponse_httpStatus :: Lens.Lens' UpdateSipRuleResponse Prelude.Int
updateSipRuleResponse_httpStatus = Lens.lens (\UpdateSipRuleResponse' {httpStatus} -> httpStatus) (\s@UpdateSipRuleResponse' {} a -> s {httpStatus = a} :: UpdateSipRuleResponse)

instance Prelude.NFData UpdateSipRuleResponse where
  rnf UpdateSipRuleResponse' {..} =
    Prelude.rnf sipRule
      `Prelude.seq` Prelude.rnf httpStatus
