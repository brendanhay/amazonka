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
-- Module      : Amazonka.Proton.RejectEnvironmentAccountConnection
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- In a management account, reject an environment account connection from
-- another environment account.
--
-- After you reject an environment account connection request, you /can\'t/
-- accept or use the rejected environment account connection.
--
-- You /can’t/ reject an environment account connection that\'s connected
-- to an environment.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-env-account-connections.html Environment account connections>
-- in the /Proton User guide/.
module Amazonka.Proton.RejectEnvironmentAccountConnection
  ( -- * Creating a Request
    RejectEnvironmentAccountConnection (..),
    newRejectEnvironmentAccountConnection,

    -- * Request Lenses
    rejectEnvironmentAccountConnection_id,

    -- * Destructuring the Response
    RejectEnvironmentAccountConnectionResponse (..),
    newRejectEnvironmentAccountConnectionResponse,

    -- * Response Lenses
    rejectEnvironmentAccountConnectionResponse_httpStatus,
    rejectEnvironmentAccountConnectionResponse_environmentAccountConnection,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRejectEnvironmentAccountConnection' smart constructor.
data RejectEnvironmentAccountConnection = RejectEnvironmentAccountConnection'
  { -- | The ID of the environment account connection to reject.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectEnvironmentAccountConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'rejectEnvironmentAccountConnection_id' - The ID of the environment account connection to reject.
newRejectEnvironmentAccountConnection ::
  -- | 'id'
  Prelude.Text ->
  RejectEnvironmentAccountConnection
newRejectEnvironmentAccountConnection pId_ =
  RejectEnvironmentAccountConnection' {id = pId_}

-- | The ID of the environment account connection to reject.
rejectEnvironmentAccountConnection_id :: Lens.Lens' RejectEnvironmentAccountConnection Prelude.Text
rejectEnvironmentAccountConnection_id = Lens.lens (\RejectEnvironmentAccountConnection' {id} -> id) (\s@RejectEnvironmentAccountConnection' {} a -> s {id = a} :: RejectEnvironmentAccountConnection)

instance
  Core.AWSRequest
    RejectEnvironmentAccountConnection
  where
  type
    AWSResponse RejectEnvironmentAccountConnection =
      RejectEnvironmentAccountConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RejectEnvironmentAccountConnectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "environmentAccountConnection")
      )

instance
  Prelude.Hashable
    RejectEnvironmentAccountConnection
  where
  hashWithSalt
    _salt
    RejectEnvironmentAccountConnection' {..} =
      _salt `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    RejectEnvironmentAccountConnection
  where
  rnf RejectEnvironmentAccountConnection' {..} =
    Prelude.rnf id

instance
  Data.ToHeaders
    RejectEnvironmentAccountConnection
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.RejectEnvironmentAccountConnection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    RejectEnvironmentAccountConnection
  where
  toJSON RejectEnvironmentAccountConnection' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("id" Data..= id)])

instance
  Data.ToPath
    RejectEnvironmentAccountConnection
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    RejectEnvironmentAccountConnection
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRejectEnvironmentAccountConnectionResponse' smart constructor.
data RejectEnvironmentAccountConnectionResponse = RejectEnvironmentAccountConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The environment connection account detail data that\'s returned by
    -- Proton.
    environmentAccountConnection :: EnvironmentAccountConnection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectEnvironmentAccountConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'rejectEnvironmentAccountConnectionResponse_httpStatus' - The response's http status code.
--
-- 'environmentAccountConnection', 'rejectEnvironmentAccountConnectionResponse_environmentAccountConnection' - The environment connection account detail data that\'s returned by
-- Proton.
newRejectEnvironmentAccountConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'environmentAccountConnection'
  EnvironmentAccountConnection ->
  RejectEnvironmentAccountConnectionResponse
newRejectEnvironmentAccountConnectionResponse
  pHttpStatus_
  pEnvironmentAccountConnection_ =
    RejectEnvironmentAccountConnectionResponse'
      { httpStatus =
          pHttpStatus_,
        environmentAccountConnection =
          pEnvironmentAccountConnection_
      }

-- | The response's http status code.
rejectEnvironmentAccountConnectionResponse_httpStatus :: Lens.Lens' RejectEnvironmentAccountConnectionResponse Prelude.Int
rejectEnvironmentAccountConnectionResponse_httpStatus = Lens.lens (\RejectEnvironmentAccountConnectionResponse' {httpStatus} -> httpStatus) (\s@RejectEnvironmentAccountConnectionResponse' {} a -> s {httpStatus = a} :: RejectEnvironmentAccountConnectionResponse)

-- | The environment connection account detail data that\'s returned by
-- Proton.
rejectEnvironmentAccountConnectionResponse_environmentAccountConnection :: Lens.Lens' RejectEnvironmentAccountConnectionResponse EnvironmentAccountConnection
rejectEnvironmentAccountConnectionResponse_environmentAccountConnection = Lens.lens (\RejectEnvironmentAccountConnectionResponse' {environmentAccountConnection} -> environmentAccountConnection) (\s@RejectEnvironmentAccountConnectionResponse' {} a -> s {environmentAccountConnection = a} :: RejectEnvironmentAccountConnectionResponse)

instance
  Prelude.NFData
    RejectEnvironmentAccountConnectionResponse
  where
  rnf RejectEnvironmentAccountConnectionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf environmentAccountConnection
