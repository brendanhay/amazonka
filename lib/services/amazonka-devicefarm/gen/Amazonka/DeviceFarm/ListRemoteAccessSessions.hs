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
-- Module      : Amazonka.DeviceFarm.ListRemoteAccessSessions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all currently running remote access sessions.
--
-- This operation returns paginated results.
module Amazonka.DeviceFarm.ListRemoteAccessSessions
  ( -- * Creating a Request
    ListRemoteAccessSessions (..),
    newListRemoteAccessSessions,

    -- * Request Lenses
    listRemoteAccessSessions_nextToken,
    listRemoteAccessSessions_arn,

    -- * Destructuring the Response
    ListRemoteAccessSessionsResponse (..),
    newListRemoteAccessSessionsResponse,

    -- * Response Lenses
    listRemoteAccessSessionsResponse_nextToken,
    listRemoteAccessSessionsResponse_remoteAccessSessions,
    listRemoteAccessSessionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DeviceFarm.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the request to return information about the remote access
-- session.
--
-- /See:/ 'newListRemoteAccessSessions' smart constructor.
data ListRemoteAccessSessions = ListRemoteAccessSessions'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the project about which you are
    -- requesting information.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRemoteAccessSessions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRemoteAccessSessions_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'arn', 'listRemoteAccessSessions_arn' - The Amazon Resource Name (ARN) of the project about which you are
-- requesting information.
newListRemoteAccessSessions ::
  -- | 'arn'
  Prelude.Text ->
  ListRemoteAccessSessions
newListRemoteAccessSessions pArn_ =
  ListRemoteAccessSessions'
    { nextToken =
        Prelude.Nothing,
      arn = pArn_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listRemoteAccessSessions_nextToken :: Lens.Lens' ListRemoteAccessSessions (Prelude.Maybe Prelude.Text)
listRemoteAccessSessions_nextToken = Lens.lens (\ListRemoteAccessSessions' {nextToken} -> nextToken) (\s@ListRemoteAccessSessions' {} a -> s {nextToken = a} :: ListRemoteAccessSessions)

-- | The Amazon Resource Name (ARN) of the project about which you are
-- requesting information.
listRemoteAccessSessions_arn :: Lens.Lens' ListRemoteAccessSessions Prelude.Text
listRemoteAccessSessions_arn = Lens.lens (\ListRemoteAccessSessions' {arn} -> arn) (\s@ListRemoteAccessSessions' {} a -> s {arn = a} :: ListRemoteAccessSessions)

instance Core.AWSPager ListRemoteAccessSessions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRemoteAccessSessionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRemoteAccessSessionsResponse_remoteAccessSessions
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRemoteAccessSessions_nextToken
          Lens..~ rs
          Lens.^? listRemoteAccessSessionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListRemoteAccessSessions where
  type
    AWSResponse ListRemoteAccessSessions =
      ListRemoteAccessSessionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRemoteAccessSessionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "remoteAccessSessions"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListRemoteAccessSessions where
  hashWithSalt _salt ListRemoteAccessSessions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` arn

instance Prelude.NFData ListRemoteAccessSessions where
  rnf ListRemoteAccessSessions' {..} =
    Prelude.rnf nextToken `Prelude.seq` Prelude.rnf arn

instance Data.ToHeaders ListRemoteAccessSessions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DeviceFarm_20150623.ListRemoteAccessSessions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListRemoteAccessSessions where
  toJSON ListRemoteAccessSessions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("nextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("arn" Data..= arn)
          ]
      )

instance Data.ToPath ListRemoteAccessSessions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListRemoteAccessSessions where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the response from the server after AWS Device Farm makes a
-- request to return information about the remote access session.
--
-- /See:/ 'newListRemoteAccessSessionsResponse' smart constructor.
data ListRemoteAccessSessionsResponse = ListRemoteAccessSessionsResponse'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A container that represents the metadata from the service about each
    -- remote access session you are requesting.
    remoteAccessSessions :: Prelude.Maybe [RemoteAccessSession],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRemoteAccessSessionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRemoteAccessSessionsResponse_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'remoteAccessSessions', 'listRemoteAccessSessionsResponse_remoteAccessSessions' - A container that represents the metadata from the service about each
-- remote access session you are requesting.
--
-- 'httpStatus', 'listRemoteAccessSessionsResponse_httpStatus' - The response's http status code.
newListRemoteAccessSessionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRemoteAccessSessionsResponse
newListRemoteAccessSessionsResponse pHttpStatus_ =
  ListRemoteAccessSessionsResponse'
    { nextToken =
        Prelude.Nothing,
      remoteAccessSessions = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listRemoteAccessSessionsResponse_nextToken :: Lens.Lens' ListRemoteAccessSessionsResponse (Prelude.Maybe Prelude.Text)
listRemoteAccessSessionsResponse_nextToken = Lens.lens (\ListRemoteAccessSessionsResponse' {nextToken} -> nextToken) (\s@ListRemoteAccessSessionsResponse' {} a -> s {nextToken = a} :: ListRemoteAccessSessionsResponse)

-- | A container that represents the metadata from the service about each
-- remote access session you are requesting.
listRemoteAccessSessionsResponse_remoteAccessSessions :: Lens.Lens' ListRemoteAccessSessionsResponse (Prelude.Maybe [RemoteAccessSession])
listRemoteAccessSessionsResponse_remoteAccessSessions = Lens.lens (\ListRemoteAccessSessionsResponse' {remoteAccessSessions} -> remoteAccessSessions) (\s@ListRemoteAccessSessionsResponse' {} a -> s {remoteAccessSessions = a} :: ListRemoteAccessSessionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRemoteAccessSessionsResponse_httpStatus :: Lens.Lens' ListRemoteAccessSessionsResponse Prelude.Int
listRemoteAccessSessionsResponse_httpStatus = Lens.lens (\ListRemoteAccessSessionsResponse' {httpStatus} -> httpStatus) (\s@ListRemoteAccessSessionsResponse' {} a -> s {httpStatus = a} :: ListRemoteAccessSessionsResponse)

instance
  Prelude.NFData
    ListRemoteAccessSessionsResponse
  where
  rnf ListRemoteAccessSessionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf remoteAccessSessions
      `Prelude.seq` Prelude.rnf httpStatus
