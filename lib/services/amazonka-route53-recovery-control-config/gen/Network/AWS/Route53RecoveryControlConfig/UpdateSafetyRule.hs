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
-- Module      : Network.AWS.Route53RecoveryControlConfig.UpdateSafetyRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a safety rule (an assertion rule or gating rule) for the routing
-- controls in a control panel. You can only update the name and the
-- waiting period for a safety rule. To make other updates, delete the
-- safety rule and create a new safety rule.
module Network.AWS.Route53RecoveryControlConfig.UpdateSafetyRule
  ( -- * Creating a Request
    UpdateSafetyRule (..),
    newUpdateSafetyRule,

    -- * Request Lenses
    updateSafetyRule_gatingRuleUpdate,
    updateSafetyRule_assertionRuleUpdate,

    -- * Destructuring the Response
    UpdateSafetyRuleResponse (..),
    newUpdateSafetyRuleResponse,

    -- * Response Lenses
    updateSafetyRuleResponse_assertionRule,
    updateSafetyRuleResponse_gatingRule,
    updateSafetyRuleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Route53RecoveryControlConfig.Types

-- | /See:/ 'newUpdateSafetyRule' smart constructor.
data UpdateSafetyRule = UpdateSafetyRule'
  { gatingRuleUpdate :: Prelude.Maybe GatingRuleUpdate,
    assertionRuleUpdate :: Prelude.Maybe AssertionRuleUpdate
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
-- 'gatingRuleUpdate', 'updateSafetyRule_gatingRuleUpdate' - Undocumented member.
--
-- 'assertionRuleUpdate', 'updateSafetyRule_assertionRuleUpdate' - Undocumented member.
newUpdateSafetyRule ::
  UpdateSafetyRule
newUpdateSafetyRule =
  UpdateSafetyRule'
    { gatingRuleUpdate =
        Prelude.Nothing,
      assertionRuleUpdate = Prelude.Nothing
    }

-- | Undocumented member.
updateSafetyRule_gatingRuleUpdate :: Lens.Lens' UpdateSafetyRule (Prelude.Maybe GatingRuleUpdate)
updateSafetyRule_gatingRuleUpdate = Lens.lens (\UpdateSafetyRule' {gatingRuleUpdate} -> gatingRuleUpdate) (\s@UpdateSafetyRule' {} a -> s {gatingRuleUpdate = a} :: UpdateSafetyRule)

-- | Undocumented member.
updateSafetyRule_assertionRuleUpdate :: Lens.Lens' UpdateSafetyRule (Prelude.Maybe AssertionRuleUpdate)
updateSafetyRule_assertionRuleUpdate = Lens.lens (\UpdateSafetyRule' {assertionRuleUpdate} -> assertionRuleUpdate) (\s@UpdateSafetyRule' {} a -> s {assertionRuleUpdate = a} :: UpdateSafetyRule)

instance Core.AWSRequest UpdateSafetyRule where
  type
    AWSResponse UpdateSafetyRule =
      UpdateSafetyRuleResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSafetyRuleResponse'
            Prelude.<$> (x Core..?> "AssertionRule")
            Prelude.<*> (x Core..?> "GatingRule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSafetyRule

instance Prelude.NFData UpdateSafetyRule

instance Core.ToHeaders UpdateSafetyRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateSafetyRule where
  toJSON UpdateSafetyRule' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("GatingRuleUpdate" Core..=)
              Prelude.<$> gatingRuleUpdate,
            ("AssertionRuleUpdate" Core..=)
              Prelude.<$> assertionRuleUpdate
          ]
      )

instance Core.ToPath UpdateSafetyRule where
  toPath = Prelude.const "/safetyrule"

instance Core.ToQuery UpdateSafetyRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSafetyRuleResponse' smart constructor.
data UpdateSafetyRuleResponse = UpdateSafetyRuleResponse'
  { assertionRule :: Prelude.Maybe AssertionRule,
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
-- 'assertionRule', 'updateSafetyRuleResponse_assertionRule' - Undocumented member.
--
-- 'gatingRule', 'updateSafetyRuleResponse_gatingRule' - Undocumented member.
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

-- | Undocumented member.
updateSafetyRuleResponse_assertionRule :: Lens.Lens' UpdateSafetyRuleResponse (Prelude.Maybe AssertionRule)
updateSafetyRuleResponse_assertionRule = Lens.lens (\UpdateSafetyRuleResponse' {assertionRule} -> assertionRule) (\s@UpdateSafetyRuleResponse' {} a -> s {assertionRule = a} :: UpdateSafetyRuleResponse)

-- | Undocumented member.
updateSafetyRuleResponse_gatingRule :: Lens.Lens' UpdateSafetyRuleResponse (Prelude.Maybe GatingRule)
updateSafetyRuleResponse_gatingRule = Lens.lens (\UpdateSafetyRuleResponse' {gatingRule} -> gatingRule) (\s@UpdateSafetyRuleResponse' {} a -> s {gatingRule = a} :: UpdateSafetyRuleResponse)

-- | The response's http status code.
updateSafetyRuleResponse_httpStatus :: Lens.Lens' UpdateSafetyRuleResponse Prelude.Int
updateSafetyRuleResponse_httpStatus = Lens.lens (\UpdateSafetyRuleResponse' {httpStatus} -> httpStatus) (\s@UpdateSafetyRuleResponse' {} a -> s {httpStatus = a} :: UpdateSafetyRuleResponse)

instance Prelude.NFData UpdateSafetyRuleResponse
