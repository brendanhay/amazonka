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
-- Module      : Amazonka.Proton.GetEnvironmentAccountConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- In an environment account, get the detailed data for an environment
-- account connection.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-env-account-connections.html Environment account connections>
-- in the /Proton User guide/.
module Amazonka.Proton.GetEnvironmentAccountConnection
  ( -- * Creating a Request
    GetEnvironmentAccountConnection (..),
    newGetEnvironmentAccountConnection,

    -- * Request Lenses
    getEnvironmentAccountConnection_id,

    -- * Destructuring the Response
    GetEnvironmentAccountConnectionResponse (..),
    newGetEnvironmentAccountConnectionResponse,

    -- * Response Lenses
    getEnvironmentAccountConnectionResponse_httpStatus,
    getEnvironmentAccountConnectionResponse_environmentAccountConnection,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetEnvironmentAccountConnection' smart constructor.
data GetEnvironmentAccountConnection = GetEnvironmentAccountConnection'
  { -- | The ID of the environment account connection that you want to get the
    -- detailed data for.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEnvironmentAccountConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getEnvironmentAccountConnection_id' - The ID of the environment account connection that you want to get the
-- detailed data for.
newGetEnvironmentAccountConnection ::
  -- | 'id'
  Prelude.Text ->
  GetEnvironmentAccountConnection
newGetEnvironmentAccountConnection pId_ =
  GetEnvironmentAccountConnection' {id = pId_}

-- | The ID of the environment account connection that you want to get the
-- detailed data for.
getEnvironmentAccountConnection_id :: Lens.Lens' GetEnvironmentAccountConnection Prelude.Text
getEnvironmentAccountConnection_id = Lens.lens (\GetEnvironmentAccountConnection' {id} -> id) (\s@GetEnvironmentAccountConnection' {} a -> s {id = a} :: GetEnvironmentAccountConnection)

instance
  Core.AWSRequest
    GetEnvironmentAccountConnection
  where
  type
    AWSResponse GetEnvironmentAccountConnection =
      GetEnvironmentAccountConnectionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetEnvironmentAccountConnectionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "environmentAccountConnection")
      )

instance
  Prelude.Hashable
    GetEnvironmentAccountConnection
  where
  hashWithSalt
    _salt
    GetEnvironmentAccountConnection' {..} =
      _salt `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    GetEnvironmentAccountConnection
  where
  rnf GetEnvironmentAccountConnection' {..} =
    Prelude.rnf id

instance
  Data.ToHeaders
    GetEnvironmentAccountConnection
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.GetEnvironmentAccountConnection" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetEnvironmentAccountConnection where
  toJSON GetEnvironmentAccountConnection' {..} =
    Data.object
      (Prelude.catMaybes [Prelude.Just ("id" Data..= id)])

instance Data.ToPath GetEnvironmentAccountConnection where
  toPath = Prelude.const "/"

instance Data.ToQuery GetEnvironmentAccountConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetEnvironmentAccountConnectionResponse' smart constructor.
data GetEnvironmentAccountConnectionResponse = GetEnvironmentAccountConnectionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The detailed data of the requested environment account connection.
    environmentAccountConnection :: EnvironmentAccountConnection
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetEnvironmentAccountConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getEnvironmentAccountConnectionResponse_httpStatus' - The response's http status code.
--
-- 'environmentAccountConnection', 'getEnvironmentAccountConnectionResponse_environmentAccountConnection' - The detailed data of the requested environment account connection.
newGetEnvironmentAccountConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'environmentAccountConnection'
  EnvironmentAccountConnection ->
  GetEnvironmentAccountConnectionResponse
newGetEnvironmentAccountConnectionResponse
  pHttpStatus_
  pEnvironmentAccountConnection_ =
    GetEnvironmentAccountConnectionResponse'
      { httpStatus =
          pHttpStatus_,
        environmentAccountConnection =
          pEnvironmentAccountConnection_
      }

-- | The response's http status code.
getEnvironmentAccountConnectionResponse_httpStatus :: Lens.Lens' GetEnvironmentAccountConnectionResponse Prelude.Int
getEnvironmentAccountConnectionResponse_httpStatus = Lens.lens (\GetEnvironmentAccountConnectionResponse' {httpStatus} -> httpStatus) (\s@GetEnvironmentAccountConnectionResponse' {} a -> s {httpStatus = a} :: GetEnvironmentAccountConnectionResponse)

-- | The detailed data of the requested environment account connection.
getEnvironmentAccountConnectionResponse_environmentAccountConnection :: Lens.Lens' GetEnvironmentAccountConnectionResponse EnvironmentAccountConnection
getEnvironmentAccountConnectionResponse_environmentAccountConnection = Lens.lens (\GetEnvironmentAccountConnectionResponse' {environmentAccountConnection} -> environmentAccountConnection) (\s@GetEnvironmentAccountConnectionResponse' {} a -> s {environmentAccountConnection = a} :: GetEnvironmentAccountConnectionResponse)

instance
  Prelude.NFData
    GetEnvironmentAccountConnectionResponse
  where
  rnf GetEnvironmentAccountConnectionResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf environmentAccountConnection
