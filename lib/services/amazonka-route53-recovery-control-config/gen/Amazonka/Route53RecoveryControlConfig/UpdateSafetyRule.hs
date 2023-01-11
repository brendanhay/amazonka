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
-- Module      : Amazonka.Route53RecoveryControlConfig.UpdateSafetyRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a safety rule (an assertion rule or gating rule). You can only
-- update the name and the waiting period for a safety rule. To make other
-- updates, delete the safety rule and create a new one.
module Amazonka.Route53RecoveryControlConfig.UpdateSafetyRule
  ( -- * Creating a Request
    UpdateSafetyRule (..),
    newUpdateSafetyRule,

    -- * Request Lenses
    updateSafetyRule_assertionRuleUpdate,
    updateSafetyRule_gatingRuleUpdate,

    -- * Destructuring the Response
    UpdateSafetyRuleResponse (..),
    newUpdateSafetyRuleResponse,

    -- * Response Lenses
    updateSafetyRuleResponse_assertionRule,
    updateSafetyRuleResponse_gatingRule,
    updateSafetyRuleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryControlConfig.Types

-- | A rule that you add to Application Recovery Controller to ensure that
-- recovery actions don\'t accidentally impair your application\'s
-- availability.
--
-- /See:/ 'newUpdateSafetyRule' smart constructor.
data UpdateSafetyRule = UpdateSafetyRule'
  { -- | The assertion rule to update.
    assertionRuleUpdate :: Prelude.Maybe AssertionRuleUpdate,
    -- | The gating rule to update.
    gatingRuleUpdate :: Prelude.Maybe GatingRuleUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSafetyRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assertionRuleUpdate', 'updateSafetyRule_assertionRuleUpdate' - The assertion rule to update.
--
-- 'gatingRuleUpdate', 'updateSafetyRule_gatingRuleUpdate' - The gating rule to update.
newUpdateSafetyRule ::
  UpdateSafetyRule
newUpdateSafetyRule =
  UpdateSafetyRule'
    { assertionRuleUpdate =
        Prelude.Nothing,
      gatingRuleUpdate = Prelude.Nothing
    }

-- | The assertion rule to update.
updateSafetyRule_assertionRuleUpdate :: Lens.Lens' UpdateSafetyRule (Prelude.Maybe AssertionRuleUpdate)
updateSafetyRule_assertionRuleUpdate = Lens.lens (\UpdateSafetyRule' {assertionRuleUpdate} -> assertionRuleUpdate) (\s@UpdateSafetyRule' {} a -> s {assertionRuleUpdate = a} :: UpdateSafetyRule)

-- | The gating rule to update.
updateSafetyRule_gatingRuleUpdate :: Lens.Lens' UpdateSafetyRule (Prelude.Maybe GatingRuleUpdate)
updateSafetyRule_gatingRuleUpdate = Lens.lens (\UpdateSafetyRule' {gatingRuleUpdate} -> gatingRuleUpdate) (\s@UpdateSafetyRule' {} a -> s {gatingRuleUpdate = a} :: UpdateSafetyRule)

instance Core.AWSRequest UpdateSafetyRule where
  type
    AWSResponse UpdateSafetyRule =
      UpdateSafetyRuleResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSafetyRuleResponse'
            Prelude.<$> (x Data..?> "AssertionRule")
            Prelude.<*> (x Data..?> "GatingRule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSafetyRule where
  hashWithSalt _salt UpdateSafetyRule' {..} =
    _salt `Prelude.hashWithSalt` assertionRuleUpdate
      `Prelude.hashWithSalt` gatingRuleUpdate

instance Prelude.NFData UpdateSafetyRule where
  rnf UpdateSafetyRule' {..} =
    Prelude.rnf assertionRuleUpdate
      `Prelude.seq` Prelude.rnf gatingRuleUpdate

instance Data.ToHeaders UpdateSafetyRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSafetyRule where
  toJSON UpdateSafetyRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AssertionRuleUpdate" Data..=)
              Prelude.<$> assertionRuleUpdate,
            ("GatingRuleUpdate" Data..=)
              Prelude.<$> gatingRuleUpdate
          ]
      )

instance Data.ToPath UpdateSafetyRule where
  toPath = Prelude.const "/safetyrule"

instance Data.ToQuery UpdateSafetyRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSafetyRuleResponse' smart constructor.
data UpdateSafetyRuleResponse = UpdateSafetyRuleResponse'
  { -- | The assertion rule updated.
    assertionRule :: Prelude.Maybe AssertionRule,
    -- | The gating rule updated.
    gatingRule :: Prelude.Maybe GatingRule,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSafetyRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assertionRule', 'updateSafetyRuleResponse_assertionRule' - The assertion rule updated.
--
-- 'gatingRule', 'updateSafetyRuleResponse_gatingRule' - The gating rule updated.
--
-- 'httpStatus', 'updateSafetyRuleResponse_httpStatus' - The response's http status code.
newUpdateSafetyRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSafetyRuleResponse
newUpdateSafetyRuleResponse pHttpStatus_ =
  UpdateSafetyRuleResponse'
    { assertionRule =
        Prelude.Nothing,
      gatingRule = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The assertion rule updated.
updateSafetyRuleResponse_assertionRule :: Lens.Lens' UpdateSafetyRuleResponse (Prelude.Maybe AssertionRule)
updateSafetyRuleResponse_assertionRule = Lens.lens (\UpdateSafetyRuleResponse' {assertionRule} -> assertionRule) (\s@UpdateSafetyRuleResponse' {} a -> s {assertionRule = a} :: UpdateSafetyRuleResponse)

-- | The gating rule updated.
updateSafetyRuleResponse_gatingRule :: Lens.Lens' UpdateSafetyRuleResponse (Prelude.Maybe GatingRule)
updateSafetyRuleResponse_gatingRule = Lens.lens (\UpdateSafetyRuleResponse' {gatingRule} -> gatingRule) (\s@UpdateSafetyRuleResponse' {} a -> s {gatingRule = a} :: UpdateSafetyRuleResponse)

-- | The response's http status code.
updateSafetyRuleResponse_httpStatus :: Lens.Lens' UpdateSafetyRuleResponse Prelude.Int
updateSafetyRuleResponse_httpStatus = Lens.lens (\UpdateSafetyRuleResponse' {httpStatus} -> httpStatus) (\s@UpdateSafetyRuleResponse' {} a -> s {httpStatus = a} :: UpdateSafetyRuleResponse)

instance Prelude.NFData UpdateSafetyRuleResponse where
  rnf UpdateSafetyRuleResponse' {..} =
    Prelude.rnf assertionRule
      `Prelude.seq` Prelude.rnf gatingRule
      `Prelude.seq` Prelude.rnf httpStatus
