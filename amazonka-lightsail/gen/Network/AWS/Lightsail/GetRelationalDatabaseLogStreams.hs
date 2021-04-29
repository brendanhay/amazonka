{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Lightsail.GetRelationalDatabaseLogStreams
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of available log streams for a specific database in
-- Amazon Lightsail.
module Network.AWS.Lightsail.GetRelationalDatabaseLogStreams
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRelationalDatabaseLogStreams' smart constructor.
data GetRelationalDatabaseLogStreams = GetRelationalDatabaseLogStreams'
  { -- | The name of your database for which to get log streams.
    relationalDatabaseName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.AWSRequest
    GetRelationalDatabaseLogStreams
  where
  type
    Rs GetRelationalDatabaseLogStreams =
      GetRelationalDatabaseLogStreamsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseLogStreamsResponse'
            Prelude.<$> ( x Prelude..?> "logStreams"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetRelationalDatabaseLogStreams

instance
  Prelude.NFData
    GetRelationalDatabaseLogStreams

instance
  Prelude.ToHeaders
    GetRelationalDatabaseLogStreams
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Lightsail_20161128.GetRelationalDatabaseLogStreams" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    GetRelationalDatabaseLogStreams
  where
  toJSON GetRelationalDatabaseLogStreams' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "relationalDatabaseName"
                  Prelude..= relationalDatabaseName
              )
          ]
      )

instance
  Prelude.ToPath
    GetRelationalDatabaseLogStreams
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    GetRelationalDatabaseLogStreams
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRelationalDatabaseLogStreamsResponse' smart constructor.
data GetRelationalDatabaseLogStreamsResponse = GetRelationalDatabaseLogStreamsResponse'
  { -- | An object describing the result of your get relational database log
    -- streams request.
    logStreams :: Prelude.Maybe [Prelude.Text],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
getRelationalDatabaseLogStreamsResponse_logStreams = Lens.lens (\GetRelationalDatabaseLogStreamsResponse' {logStreams} -> logStreams) (\s@GetRelationalDatabaseLogStreamsResponse' {} a -> s {logStreams = a} :: GetRelationalDatabaseLogStreamsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getRelationalDatabaseLogStreamsResponse_httpStatus :: Lens.Lens' GetRelationalDatabaseLogStreamsResponse Prelude.Int
getRelationalDatabaseLogStreamsResponse_httpStatus = Lens.lens (\GetRelationalDatabaseLogStreamsResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabaseLogStreamsResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabaseLogStreamsResponse)

instance
  Prelude.NFData
    GetRelationalDatabaseLogStreamsResponse
