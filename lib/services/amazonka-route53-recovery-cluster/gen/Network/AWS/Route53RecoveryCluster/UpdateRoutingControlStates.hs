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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Set multiple routing control states. You can set the value for each
-- state to be On or Off. When the state is On, traffic flows to a cell.
-- When it\'s off, traffic does not flow.
--
-- For more information about working with routing controls, see
-- <https://docs.aws.amazon.com/r53recovery/latest/dg/routing-control.html Routing control>
-- in the Route 53 Application Recovery Controller Developer Guide.
module Amazonka.Route53RecoveryCluster.UpdateRoutingControlStates
  ( -- * Creating a Request
    UpdateRoutingControlStates (..),
    newUpdateRoutingControlStates,

    -- * Request Lenses
    updateRoutingControlStates_updateRoutingControlStateEntries,

    -- * Destructuring the Response
    UpdateRoutingControlStatesResponse (..),
    newUpdateRoutingControlStatesResponse,

    -- * Response Lenses
    updateRoutingControlStatesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53RecoveryCluster.Types

-- | /See:/ 'newUpdateRoutingControlStates' smart constructor.
data UpdateRoutingControlStates = UpdateRoutingControlStates'
  { -- | A set of routing control entries that you want to update.
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
-- 'updateRoutingControlStateEntries', 'updateRoutingControlStates_updateRoutingControlStateEntries' - A set of routing control entries that you want to update.
newUpdateRoutingControlStates ::
  UpdateRoutingControlStates
newUpdateRoutingControlStates =
  UpdateRoutingControlStates'
    { updateRoutingControlStateEntries =
        Prelude.mempty
    }

-- | A set of routing control entries that you want to update.
updateRoutingControlStates_updateRoutingControlStateEntries :: Lens.Lens' UpdateRoutingControlStates [UpdateRoutingControlStateEntry]
updateRoutingControlStates_updateRoutingControlStateEntries = Lens.lens (\UpdateRoutingControlStates' {updateRoutingControlStateEntries} -> updateRoutingControlStateEntries) (\s@UpdateRoutingControlStates' {} a -> s {updateRoutingControlStateEntries = a} :: UpdateRoutingControlStates) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateRoutingControlStates where
  type
    AWSResponse UpdateRoutingControlStates =
      UpdateRoutingControlStatesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateRoutingControlStatesResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRoutingControlStates

instance Prelude.NFData UpdateRoutingControlStates

instance Core.ToHeaders UpdateRoutingControlStates where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ToggleCustomerAPI.UpdateRoutingControlStates" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateRoutingControlStates where
  toJSON UpdateRoutingControlStates' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "UpdateRoutingControlStateEntries"
                  Core..= updateRoutingControlStateEntries
              )
          ]
      )

instance Core.ToPath UpdateRoutingControlStates where
  toPath = Prelude.const "/"

instance Core.ToQuery UpdateRoutingControlStates where
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
