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
-- Module      : Amazonka.Route53RecoveryCluster.UpdateRoutingControlStates
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set multiple routing control states. You can set the value for each
-- state to be On or Off. When the state is On, traffic flows to a cell.
-- When it\'s Off, traffic does not flow.
--
-- With Route 53 ARC, you can add safety rules for routing controls, which
-- are safeguards for routing control state updates that help prevent
-- unexpected outcomes, like fail open traffic routing. However, there are
-- scenarios when you might want to bypass the routing control safeguards
-- that are enforced with safety rules that you\'ve configured. For
-- example, you might want to fail over quickly for disaster recovery, and
-- one or more safety rules might be unexpectedly preventing you from
-- updating a routing control state to reroute traffic. In a \"break
-- glass\" scenario like this, you can override one or more safety rules to
-- change a routing control state and fail over your application.
--
-- The @SafetyRulesToOverride@ property enables you override one or more
-- safety rules and update routing control states. For more information,
-- see
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.override-safety-rule.html Override safety rules to reroute traffic>
-- in the Amazon Route 53 Application Recovery Controller Developer Guide.
--
-- /You must specify Regional endpoints when you work with API cluster
-- operations to get or update routing control states in Route 53 ARC./
--
-- To see a code example for getting a routing control state, including
-- accessing Regional cluster endpoints in sequence, see
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/service_code_examples_actions.html API examples>
-- in the Amazon Route 53 Application Recovery Controller Developer Guide.
--
-- -   <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.update.html Viewing and updating routing control states>
--
-- -   <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.html Working with routing controls overall>
module Amazonka.Route53RecoveryCluster.UpdateRoutingControlStates
  ( -- * Creating a Request
    UpdateRoutingControlStates (..),
    newUpdateRoutingControlStates,

    -- * Request Lenses
    updateRoutingControlStates_safetyRulesToOverride,
    updateRoutingControlStates_updateRoutingControlStateEntries,

    -- * Destructuring the Response
    UpdateRoutingControlStatesResponse (..),
    newUpdateRoutingControlStatesResponse,

    -- * Response Lenses
    updateRoutingControlStatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryCluster.Types

-- | /See:/ 'newUpdateRoutingControlStates' smart constructor.
data UpdateRoutingControlStates = UpdateRoutingControlStates'
  { -- | The Amazon Resource Names (ARNs) for the safety rules that you want to
    -- override when you\'re updating routing control states. You can override
    -- one safety rule or multiple safety rules by including one or more ARNs,
    -- separated by commas.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.override-safety-rule.html Override safety rules to reroute traffic>
    -- in the Amazon Route 53 Application Recovery Controller Developer Guide.
    safetyRulesToOverride :: Prelude.Maybe [Prelude.Text],
    -- | A set of routing control entries that you want to update.
    updateRoutingControlStateEntries :: [UpdateRoutingControlStateEntry]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoutingControlStates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'safetyRulesToOverride', 'updateRoutingControlStates_safetyRulesToOverride' - The Amazon Resource Names (ARNs) for the safety rules that you want to
-- override when you\'re updating routing control states. You can override
-- one safety rule or multiple safety rules by including one or more ARNs,
-- separated by commas.
--
-- For more information, see
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.override-safety-rule.html Override safety rules to reroute traffic>
-- in the Amazon Route 53 Application Recovery Controller Developer Guide.
--
-- 'updateRoutingControlStateEntries', 'updateRoutingControlStates_updateRoutingControlStateEntries' - A set of routing control entries that you want to update.
newUpdateRoutingControlStates ::
  UpdateRoutingControlStates
newUpdateRoutingControlStates =
  UpdateRoutingControlStates'
    { safetyRulesToOverride =
        Prelude.Nothing,
      updateRoutingControlStateEntries =
        Prelude.mempty
    }

-- | The Amazon Resource Names (ARNs) for the safety rules that you want to
-- override when you\'re updating routing control states. You can override
-- one safety rule or multiple safety rules by including one or more ARNs,
-- separated by commas.
--
-- For more information, see
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.override-safety-rule.html Override safety rules to reroute traffic>
-- in the Amazon Route 53 Application Recovery Controller Developer Guide.
updateRoutingControlStates_safetyRulesToOverride :: Lens.Lens' UpdateRoutingControlStates (Prelude.Maybe [Prelude.Text])
updateRoutingControlStates_safetyRulesToOverride = Lens.lens (\UpdateRoutingControlStates' {safetyRulesToOverride} -> safetyRulesToOverride) (\s@UpdateRoutingControlStates' {} a -> s {safetyRulesToOverride = a} :: UpdateRoutingControlStates) Prelude.. Lens.mapping Lens.coerced

-- | A set of routing control entries that you want to update.
updateRoutingControlStates_updateRoutingControlStateEntries :: Lens.Lens' UpdateRoutingControlStates [UpdateRoutingControlStateEntry]
updateRoutingControlStates_updateRoutingControlStateEntries = Lens.lens (\UpdateRoutingControlStates' {updateRoutingControlStateEntries} -> updateRoutingControlStateEntries) (\s@UpdateRoutingControlStates' {} a -> s {updateRoutingControlStateEntries = a} :: UpdateRoutingControlStates) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateRoutingControlStates where
  type
    AWSResponse UpdateRoutingControlStates =
      UpdateRoutingControlStatesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateRoutingControlStatesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRoutingControlStates where
  hashWithSalt _salt UpdateRoutingControlStates' {..} =
    _salt
      `Prelude.hashWithSalt` safetyRulesToOverride
      `Prelude.hashWithSalt` updateRoutingControlStateEntries

instance Prelude.NFData UpdateRoutingControlStates where
  rnf UpdateRoutingControlStates' {..} =
    Prelude.rnf safetyRulesToOverride
      `Prelude.seq` Prelude.rnf updateRoutingControlStateEntries

instance Data.ToHeaders UpdateRoutingControlStates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ToggleCustomerAPI.UpdateRoutingControlStates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRoutingControlStates where
  toJSON UpdateRoutingControlStates' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SafetyRulesToOverride" Data..=)
              Prelude.<$> safetyRulesToOverride,
            Prelude.Just
              ( "UpdateRoutingControlStateEntries"
                  Data..= updateRoutingControlStateEntries
              )
          ]
      )

instance Data.ToPath UpdateRoutingControlStates where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRoutingControlStates where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRoutingControlStatesResponse' smart constructor.
data UpdateRoutingControlStatesResponse = UpdateRoutingControlStatesResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRoutingControlStatesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateRoutingControlStatesResponse_httpStatus' - The response's http status code.
newUpdateRoutingControlStatesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRoutingControlStatesResponse
newUpdateRoutingControlStatesResponse pHttpStatus_ =
  UpdateRoutingControlStatesResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateRoutingControlStatesResponse_httpStatus :: Lens.Lens' UpdateRoutingControlStatesResponse Prelude.Int
updateRoutingControlStatesResponse_httpStatus = Lens.lens (\UpdateRoutingControlStatesResponse' {httpStatus} -> httpStatus) (\s@UpdateRoutingControlStatesResponse' {} a -> s {httpStatus = a} :: UpdateRoutingControlStatesResponse)

instance
  Prelude.NFData
    UpdateRoutingControlStatesResponse
  where
  rnf UpdateRoutingControlStatesResponse' {..} =
    Prelude.rnf httpStatus
