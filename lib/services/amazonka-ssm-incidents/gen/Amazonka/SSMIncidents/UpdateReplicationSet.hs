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
-- Module      : Amazonka.SSMIncidents.UpdateReplicationSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add or delete Regions from your replication set.
module Amazonka.SSMIncidents.UpdateReplicationSet
  ( -- * Creating a Request
    UpdateReplicationSet (..),
    newUpdateReplicationSet,

    -- * Request Lenses
    updateReplicationSet_clientToken,
    updateReplicationSet_actions,
    updateReplicationSet_arn,

    -- * Destructuring the Response
    UpdateReplicationSetResponse (..),
    newUpdateReplicationSetResponse,

    -- * Response Lenses
    updateReplicationSetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newUpdateReplicationSet' smart constructor.
data UpdateReplicationSet = UpdateReplicationSet'
  { -- | A token that ensures that the operation is called only once with the
    -- specified details.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | An action to add or delete a Region.
    actions :: Prelude.NonEmpty UpdateReplicationSetAction,
    -- | The Amazon Resource Name (ARN) of the replication set you\'re updating.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateReplicationSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateReplicationSet_clientToken' - A token that ensures that the operation is called only once with the
-- specified details.
--
-- 'actions', 'updateReplicationSet_actions' - An action to add or delete a Region.
--
-- 'arn', 'updateReplicationSet_arn' - The Amazon Resource Name (ARN) of the replication set you\'re updating.
newUpdateReplicationSet ::
  -- | 'actions'
  Prelude.NonEmpty UpdateReplicationSetAction ->
  -- | 'arn'
  Prelude.Text ->
  UpdateReplicationSet
newUpdateReplicationSet pActions_ pArn_ =
  UpdateReplicationSet'
    { clientToken =
        Prelude.Nothing,
      actions = Lens.coerced Lens.# pActions_,
      arn = pArn_
    }

-- | A token that ensures that the operation is called only once with the
-- specified details.
updateReplicationSet_clientToken :: Lens.Lens' UpdateReplicationSet (Prelude.Maybe Prelude.Text)
updateReplicationSet_clientToken = Lens.lens (\UpdateReplicationSet' {clientToken} -> clientToken) (\s@UpdateReplicationSet' {} a -> s {clientToken = a} :: UpdateReplicationSet)

-- | An action to add or delete a Region.
updateReplicationSet_actions :: Lens.Lens' UpdateReplicationSet (Prelude.NonEmpty UpdateReplicationSetAction)
updateReplicationSet_actions = Lens.lens (\UpdateReplicationSet' {actions} -> actions) (\s@UpdateReplicationSet' {} a -> s {actions = a} :: UpdateReplicationSet) Prelude.. Lens.coerced

-- | The Amazon Resource Name (ARN) of the replication set you\'re updating.
updateReplicationSet_arn :: Lens.Lens' UpdateReplicationSet Prelude.Text
updateReplicationSet_arn = Lens.lens (\UpdateReplicationSet' {arn} -> arn) (\s@UpdateReplicationSet' {} a -> s {arn = a} :: UpdateReplicationSet)

instance Core.AWSRequest UpdateReplicationSet where
  type
    AWSResponse UpdateReplicationSet =
      UpdateReplicationSetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateReplicationSetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateReplicationSet where
  hashWithSalt _salt UpdateReplicationSet' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` actions
      `Prelude.hashWithSalt` arn

instance Prelude.NFData UpdateReplicationSet where
  rnf UpdateReplicationSet' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf actions
      `Prelude.seq` Prelude.rnf arn

instance Data.ToHeaders UpdateReplicationSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateReplicationSet where
  toJSON UpdateReplicationSet' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("actions" Data..= actions),
            Prelude.Just ("arn" Data..= arn)
          ]
      )

instance Data.ToPath UpdateReplicationSet where
  toPath = Prelude.const "/updateReplicationSet"

instance Data.ToQuery UpdateReplicationSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateReplicationSetResponse' smart constructor.
data UpdateReplicationSetResponse = UpdateReplicationSetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateReplicationSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateReplicationSetResponse_httpStatus' - The response's http status code.
newUpdateReplicationSetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateReplicationSetResponse
newUpdateReplicationSetResponse pHttpStatus_ =
  UpdateReplicationSetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateReplicationSetResponse_httpStatus :: Lens.Lens' UpdateReplicationSetResponse Prelude.Int
updateReplicationSetResponse_httpStatus = Lens.lens (\UpdateReplicationSetResponse' {httpStatus} -> httpStatus) (\s@UpdateReplicationSetResponse' {} a -> s {httpStatus = a} :: UpdateReplicationSetResponse)

instance Prelude.NFData UpdateReplicationSetResponse where
  rnf UpdateReplicationSetResponse' {..} =
    Prelude.rnf httpStatus
