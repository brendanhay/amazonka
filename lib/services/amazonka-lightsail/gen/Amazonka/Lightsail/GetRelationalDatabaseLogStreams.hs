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
-- Module      : Amazonka.Lightsail.GetRelationalDatabaseLogStreams
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of available log streams for a specific database in
-- Amazon Lightsail.
module Amazonka.Lightsail.GetRelationalDatabaseLogStreams
  ( -- * Creating a Request
    GetRelationalDatabaseLogStreams (..),
    newGetRelationalDatabaseLogStreams,

    -- * Request Lenses
    getRelationalDatabaseLogStreams_relationalDatabaseName,

    -- * Destructuring the Response
    GetRelationalDatabaseLogStreamsResponse (..),
    newGetRelationalDatabaseLogStreamsResponse,

    -- * Response Lenses
    getRelationalDatabaseLogStreamsResponse_logStreams,
    getRelationalDatabaseLogStreamsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRelationalDatabaseLogStreams' smart constructor.
data GetRelationalDatabaseLogStreams = GetRelationalDatabaseLogStreams'
  { -- | The name of your database for which to get log streams.
    relationalDatabaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseLogStreams' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'relationalDatabaseName', 'getRelationalDatabaseLogStreams_relationalDatabaseName' - The name of your database for which to get log streams.
newGetRelationalDatabaseLogStreams ::
  -- | 'relationalDatabaseName'
  Prelude.Text ->
  GetRelationalDatabaseLogStreams
newGetRelationalDatabaseLogStreams
  pRelationalDatabaseName_ =
    GetRelationalDatabaseLogStreams'
      { relationalDatabaseName =
          pRelationalDatabaseName_
      }

-- | The name of your database for which to get log streams.
getRelationalDatabaseLogStreams_relationalDatabaseName :: Lens.Lens' GetRelationalDatabaseLogStreams Prelude.Text
getRelationalDatabaseLogStreams_relationalDatabaseName = Lens.lens (\GetRelationalDatabaseLogStreams' {relationalDatabaseName} -> relationalDatabaseName) (\s@GetRelationalDatabaseLogStreams' {} a -> s {relationalDatabaseName = a} :: GetRelationalDatabaseLogStreams)

instance
  Core.AWSRequest
    GetRelationalDatabaseLogStreams
  where
  type
    AWSResponse GetRelationalDatabaseLogStreams =
      GetRelationalDatabaseLogStreamsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseLogStreamsResponse'
            Prelude.<$> (x Data..?> "logStreams" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetRelationalDatabaseLogStreams
  where
  hashWithSalt
    _salt
    GetRelationalDatabaseLogStreams' {..} =
      _salt `Prelude.hashWithSalt` relationalDatabaseName

instance
  Prelude.NFData
    GetRelationalDatabaseLogStreams
  where
  rnf GetRelationalDatabaseLogStreams' {..} =
    Prelude.rnf relationalDatabaseName

instance
  Data.ToHeaders
    GetRelationalDatabaseLogStreams
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetRelationalDatabaseLogStreams" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRelationalDatabaseLogStreams where
  toJSON GetRelationalDatabaseLogStreams' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "relationalDatabaseName"
                  Data..= relationalDatabaseName
              )
          ]
      )

instance Data.ToPath GetRelationalDatabaseLogStreams where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRelationalDatabaseLogStreams where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRelationalDatabaseLogStreamsResponse' smart constructor.
data GetRelationalDatabaseLogStreamsResponse = GetRelationalDatabaseLogStreamsResponse'
  { -- | An object describing the result of your get relational database log
    -- streams request.
    logStreams :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRelationalDatabaseLogStreamsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'logStreams', 'getRelationalDatabaseLogStreamsResponse_logStreams' - An object describing the result of your get relational database log
-- streams request.
--
-- 'httpStatus', 'getRelationalDatabaseLogStreamsResponse_httpStatus' - The response's http status code.
newGetRelationalDatabaseLogStreamsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRelationalDatabaseLogStreamsResponse
newGetRelationalDatabaseLogStreamsResponse
  pHttpStatus_ =
    GetRelationalDatabaseLogStreamsResponse'
      { logStreams =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An object describing the result of your get relational database log
-- streams request.
getRelationalDatabaseLogStreamsResponse_logStreams :: Lens.Lens' GetRelationalDatabaseLogStreamsResponse (Prelude.Maybe [Prelude.Text])
getRelationalDatabaseLogStreamsResponse_logStreams = Lens.lens (\GetRelationalDatabaseLogStreamsResponse' {logStreams} -> logStreams) (\s@GetRelationalDatabaseLogStreamsResponse' {} a -> s {logStreams = a} :: GetRelationalDatabaseLogStreamsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRelationalDatabaseLogStreamsResponse_httpStatus :: Lens.Lens' GetRelationalDatabaseLogStreamsResponse Prelude.Int
getRelationalDatabaseLogStreamsResponse_httpStatus = Lens.lens (\GetRelationalDatabaseLogStreamsResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabaseLogStreamsResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabaseLogStreamsResponse)

instance
  Prelude.NFData
    GetRelationalDatabaseLogStreamsResponse
  where
  rnf GetRelationalDatabaseLogStreamsResponse' {..} =
    Prelude.rnf logStreams
      `Prelude.seq` Prelude.rnf httpStatus
