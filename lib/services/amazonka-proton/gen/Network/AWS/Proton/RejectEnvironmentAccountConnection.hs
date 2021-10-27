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
-- Module      : Network.AWS.Proton.RejectEnvironmentAccountConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- In a management account, reject an environment account connection from
-- another environment account.
--
-- After you reject an environment account connection request, you /won’t/
-- be able to accept or use the rejected environment account connection.
--
-- You /can’t/ reject an environment account connection that is connected
-- to an environment.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/adminguide/ag-env-account-connections.html Environment account connections>
-- in the /AWS Proton Administrator guide/.
module Network.AWS.Proton.RejectEnvironmentAccountConnection
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.Proton.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          RejectEnvironmentAccountConnectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Core..:> "environmentAccountConnection")
      )

instance
  Prelude.Hashable
    RejectEnvironmentAccountConnection

instance
  Prelude.NFData
    RejectEnvironmentAccountConnection

instance
  Core.ToHeaders
    RejectEnvironmentAccountConnection
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AwsProton20200720.RejectEnvironmentAccountConnection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    RejectEnvironmentAccountConnection
  where
  toJSON RejectEnvironmentAccountConnection' {..} =
    Core.object
      (Prelude.catMaybes [Prelude.Just ("id" Core..= id)])

instance
  Core.ToPath
    RejectEnvironmentAccountConnection
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    RejectEnvironmentAccountConnection
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRejectEnvironmentAccountConnectionResponse' smart constructor.
data RejectEnvironmentAccountConnectionResponse = RejectEnvironmentAccountConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The environment connection account detail data that\'s returned by AWS
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
-- 'environmentAccountConnection', 'rejectEnvironmentAccountConnectionResponse_environmentAccountConnection' - The environment connection account detail data that\'s returned by AWS
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

-- | The environment connection account detail data that\'s returned by AWS
-- Proton.
rejectEnvironmentAccountConnectionResponse_environmentAccountConnection :: Lens.Lens' RejectEnvironmentAccountConnectionResponse EnvironmentAccountConnection
rejectEnvironmentAccountConnectionResponse_environmentAccountConnection = Lens.lens (\RejectEnvironmentAccountConnectionResponse' {environmentAccountConnection} -> environmentAccountConnection) (\s@RejectEnvironmentAccountConnectionResponse' {} a -> s {environmentAccountConnection = a} :: RejectEnvironmentAccountConnectionResponse)

instance
  Prelude.NFData
    RejectEnvironmentAccountConnectionResponse
