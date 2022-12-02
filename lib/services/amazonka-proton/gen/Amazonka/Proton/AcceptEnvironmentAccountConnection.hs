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
-- Module      : Amazonka.Proton.AcceptEnvironmentAccountConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- In a management account, an environment account connection request is
-- accepted. When the environment account connection request is accepted,
-- Proton can use the associated IAM role to provision environment
-- infrastructure resources in the associated environment account.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-env-account-connections.html Environment account connections>
-- in the /Proton User guide/.
module Amazonka.Proton.AcceptEnvironmentAccountConnection
  ( -- * Creating a Request
    AcceptEnvironmentAccountConnection (..),
    newAcceptEnvironmentAccountConnection,

    -- * Request Lenses
    acceptEnvironmentAccountConnection_id,

    -- * Destructuring the Response
    AcceptEnvironmentAccountConnectionResponse (..),
    newAcceptEnvironmentAccountConnectionResponse,

    -- * Response Lenses
    acceptEnvironmentAccountConnectionResponse_httpStatus,
    acceptEnvironmentAccountConnectionResponse_environmentAccountConnection,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAcceptEnvironmentAccountConnection' smart constructor.
data AcceptEnvironmentAccountConnection = AcceptEnvironmentAccountConnection'
  { -- | The ID of the environment account connection.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptEnvironmentAccountConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'acceptEnvironmentAccountConnection_id' - The ID of the environment account connection.
newAcceptEnvironmentAccountConnection ::
  -- | 'id'
  Prelude.Text ->
  AcceptEnvironmentAccountConnection
newAcceptEnvironmentAccountConnection pId_ =
  AcceptEnvironmentAccountConnection' {id = pId_}

-- | The ID of the environment account connection.
acceptEnvironmentAccountConnection_id :: Lens.Lens' AcceptEnvironmentAccountConnection Prelude.Text
acceptEnvironmentAccountConnection_id = Lens.lens (\AcceptEnvironmentAccountConnection' {id} -> id) (\s@AcceptEnvironmentAccountConnection' {} a -> s {id = a} :: AcceptEnvironmentAccountConnection)

instance
  Core.AWSRequest
    AcceptEnvironmentAccountConnection
  where
  type
    AWSResponse AcceptEnvironmentAccountConnection =
      AcceptEnvironmentAccountConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AcceptEnvironmentAccountConnectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> (x Data..:> "environmentAccountConnection")
      )

instance
  Prelude.Hashable
    AcceptEnvironmentAccountConnection
  where
  hashWithSalt
    _salt
    AcceptEnvironmentAccountConnection' {..} =
      _salt `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    AcceptEnvironmentAccountConnection
  where
  rnf AcceptEnvironmentAccountConnection' {..} =
    Prelude.rnf id

instance
  Data.ToHeaders
    AcceptEnvironmentAccountConnection
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.AcceptEnvironmentAccountConnection" ::
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
    AcceptEnvironmentAccountConnection
  where
  toJSON AcceptEnvironmentAccountConnection' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("id" Data..= id)])

instance
  Data.ToPath
    AcceptEnvironmentAccountConnection
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    AcceptEnvironmentAccountConnection
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAcceptEnvironmentAccountConnectionResponse' smart constructor.
data AcceptEnvironmentAccountConnectionResponse = AcceptEnvironmentAccountConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The environment account connection data that\'s returned by Proton.
    environmentAccountConnection :: EnvironmentAccountConnection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AcceptEnvironmentAccountConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'acceptEnvironmentAccountConnectionResponse_httpStatus' - The response's http status code.
--
-- 'environmentAccountConnection', 'acceptEnvironmentAccountConnectionResponse_environmentAccountConnection' - The environment account connection data that\'s returned by Proton.
newAcceptEnvironmentAccountConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'environmentAccountConnection'
  EnvironmentAccountConnection ->
  AcceptEnvironmentAccountConnectionResponse
newAcceptEnvironmentAccountConnectionResponse
  pHttpStatus_
  pEnvironmentAccountConnection_ =
    AcceptEnvironmentAccountConnectionResponse'
      { httpStatus =
          pHttpStatus_,
        environmentAccountConnection =
          pEnvironmentAccountConnection_
      }

-- | The response's http status code.
acceptEnvironmentAccountConnectionResponse_httpStatus :: Lens.Lens' AcceptEnvironmentAccountConnectionResponse Prelude.Int
acceptEnvironmentAccountConnectionResponse_httpStatus = Lens.lens (\AcceptEnvironmentAccountConnectionResponse' {httpStatus} -> httpStatus) (\s@AcceptEnvironmentAccountConnectionResponse' {} a -> s {httpStatus = a} :: AcceptEnvironmentAccountConnectionResponse)

-- | The environment account connection data that\'s returned by Proton.
acceptEnvironmentAccountConnectionResponse_environmentAccountConnection :: Lens.Lens' AcceptEnvironmentAccountConnectionResponse EnvironmentAccountConnection
acceptEnvironmentAccountConnectionResponse_environmentAccountConnection = Lens.lens (\AcceptEnvironmentAccountConnectionResponse' {environmentAccountConnection} -> environmentAccountConnection) (\s@AcceptEnvironmentAccountConnectionResponse' {} a -> s {environmentAccountConnection = a} :: AcceptEnvironmentAccountConnectionResponse)

instance
  Prelude.NFData
    AcceptEnvironmentAccountConnectionResponse
  where
  rnf AcceptEnvironmentAccountConnectionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf environmentAccountConnection
