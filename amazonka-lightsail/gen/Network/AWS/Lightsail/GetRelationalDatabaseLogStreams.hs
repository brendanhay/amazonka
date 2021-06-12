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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetRelationalDatabaseLogStreams' smart constructor.
data GetRelationalDatabaseLogStreams = GetRelationalDatabaseLogStreams'
  { -- | The name of your database for which to get log streams.
    relationalDatabaseName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetRelationalDatabaseLogStreams
newGetRelationalDatabaseLogStreams
  pRelationalDatabaseName_ =
    GetRelationalDatabaseLogStreams'
      { relationalDatabaseName =
          pRelationalDatabaseName_
      }

-- | The name of your database for which to get log streams.
getRelationalDatabaseLogStreams_relationalDatabaseName :: Lens.Lens' GetRelationalDatabaseLogStreams Core.Text
getRelationalDatabaseLogStreams_relationalDatabaseName = Lens.lens (\GetRelationalDatabaseLogStreams' {relationalDatabaseName} -> relationalDatabaseName) (\s@GetRelationalDatabaseLogStreams' {} a -> s {relationalDatabaseName = a} :: GetRelationalDatabaseLogStreams)

instance
  Core.AWSRequest
    GetRelationalDatabaseLogStreams
  where
  type
    AWSResponse GetRelationalDatabaseLogStreams =
      GetRelationalDatabaseLogStreamsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRelationalDatabaseLogStreamsResponse'
            Core.<$> (x Core..?> "logStreams" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetRelationalDatabaseLogStreams

instance Core.NFData GetRelationalDatabaseLogStreams

instance
  Core.ToHeaders
    GetRelationalDatabaseLogStreams
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetRelationalDatabaseLogStreams" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetRelationalDatabaseLogStreams where
  toJSON GetRelationalDatabaseLogStreams' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ( "relationalDatabaseName"
                  Core..= relationalDatabaseName
              )
          ]
      )

instance Core.ToPath GetRelationalDatabaseLogStreams where
  toPath = Core.const "/"

instance Core.ToQuery GetRelationalDatabaseLogStreams where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetRelationalDatabaseLogStreamsResponse' smart constructor.
data GetRelationalDatabaseLogStreamsResponse = GetRelationalDatabaseLogStreamsResponse'
  { -- | An object describing the result of your get relational database log
    -- streams request.
    logStreams :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetRelationalDatabaseLogStreamsResponse
newGetRelationalDatabaseLogStreamsResponse
  pHttpStatus_ =
    GetRelationalDatabaseLogStreamsResponse'
      { logStreams =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An object describing the result of your get relational database log
-- streams request.
getRelationalDatabaseLogStreamsResponse_logStreams :: Lens.Lens' GetRelationalDatabaseLogStreamsResponse (Core.Maybe [Core.Text])
getRelationalDatabaseLogStreamsResponse_logStreams = Lens.lens (\GetRelationalDatabaseLogStreamsResponse' {logStreams} -> logStreams) (\s@GetRelationalDatabaseLogStreamsResponse' {} a -> s {logStreams = a} :: GetRelationalDatabaseLogStreamsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getRelationalDatabaseLogStreamsResponse_httpStatus :: Lens.Lens' GetRelationalDatabaseLogStreamsResponse Core.Int
getRelationalDatabaseLogStreamsResponse_httpStatus = Lens.lens (\GetRelationalDatabaseLogStreamsResponse' {httpStatus} -> httpStatus) (\s@GetRelationalDatabaseLogStreamsResponse' {} a -> s {httpStatus = a} :: GetRelationalDatabaseLogStreamsResponse)

instance
  Core.NFData
    GetRelationalDatabaseLogStreamsResponse
