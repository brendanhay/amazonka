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
-- Module      : Network.AWS.DeviceFarm.ListRemoteAccessSessions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all currently running remote access sessions.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListRemoteAccessSessions
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

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the request to return information about the remote access
-- session.
--
-- /See:/ 'newListRemoteAccessSessions' smart constructor.
data ListRemoteAccessSessions = ListRemoteAccessSessions'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the project about which you are
    -- requesting information.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  ListRemoteAccessSessions
newListRemoteAccessSessions pArn_ =
  ListRemoteAccessSessions'
    { nextToken = Core.Nothing,
      arn = pArn_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listRemoteAccessSessions_nextToken :: Lens.Lens' ListRemoteAccessSessions (Core.Maybe Core.Text)
listRemoteAccessSessions_nextToken = Lens.lens (\ListRemoteAccessSessions' {nextToken} -> nextToken) (\s@ListRemoteAccessSessions' {} a -> s {nextToken = a} :: ListRemoteAccessSessions)

-- | The Amazon Resource Name (ARN) of the project about which you are
-- requesting information.
listRemoteAccessSessions_arn :: Lens.Lens' ListRemoteAccessSessions Core.Text
listRemoteAccessSessions_arn = Lens.lens (\ListRemoteAccessSessions' {arn} -> arn) (\s@ListRemoteAccessSessions' {} a -> s {arn = a} :: ListRemoteAccessSessions)

instance Core.AWSPager ListRemoteAccessSessions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRemoteAccessSessionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listRemoteAccessSessionsResponse_remoteAccessSessions
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listRemoteAccessSessions_nextToken
          Lens..~ rs
          Lens.^? listRemoteAccessSessionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListRemoteAccessSessions where
  type
    AWSResponse ListRemoteAccessSessions =
      ListRemoteAccessSessionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRemoteAccessSessionsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> ( x Core..?> "remoteAccessSessions"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListRemoteAccessSessions

instance Core.NFData ListRemoteAccessSessions

instance Core.ToHeaders ListRemoteAccessSessions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListRemoteAccessSessions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListRemoteAccessSessions where
  toJSON ListRemoteAccessSessions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            Core.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath ListRemoteAccessSessions where
  toPath = Core.const "/"

instance Core.ToQuery ListRemoteAccessSessions where
  toQuery = Core.const Core.mempty

-- | Represents the response from the server after AWS Device Farm makes a
-- request to return information about the remote access session.
--
-- /See:/ 'newListRemoteAccessSessionsResponse' smart constructor.
data ListRemoteAccessSessionsResponse = ListRemoteAccessSessionsResponse'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | A container that represents the metadata from the service about each
    -- remote access session you are requesting.
    remoteAccessSessions :: Core.Maybe [RemoteAccessSession],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListRemoteAccessSessionsResponse
newListRemoteAccessSessionsResponse pHttpStatus_ =
  ListRemoteAccessSessionsResponse'
    { nextToken =
        Core.Nothing,
      remoteAccessSessions = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listRemoteAccessSessionsResponse_nextToken :: Lens.Lens' ListRemoteAccessSessionsResponse (Core.Maybe Core.Text)
listRemoteAccessSessionsResponse_nextToken = Lens.lens (\ListRemoteAccessSessionsResponse' {nextToken} -> nextToken) (\s@ListRemoteAccessSessionsResponse' {} a -> s {nextToken = a} :: ListRemoteAccessSessionsResponse)

-- | A container that represents the metadata from the service about each
-- remote access session you are requesting.
listRemoteAccessSessionsResponse_remoteAccessSessions :: Lens.Lens' ListRemoteAccessSessionsResponse (Core.Maybe [RemoteAccessSession])
listRemoteAccessSessionsResponse_remoteAccessSessions = Lens.lens (\ListRemoteAccessSessionsResponse' {remoteAccessSessions} -> remoteAccessSessions) (\s@ListRemoteAccessSessionsResponse' {} a -> s {remoteAccessSessions = a} :: ListRemoteAccessSessionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listRemoteAccessSessionsResponse_httpStatus :: Lens.Lens' ListRemoteAccessSessionsResponse Core.Int
listRemoteAccessSessionsResponse_httpStatus = Lens.lens (\ListRemoteAccessSessionsResponse' {httpStatus} -> httpStatus) (\s@ListRemoteAccessSessionsResponse' {} a -> s {httpStatus = a} :: ListRemoteAccessSessionsResponse)

instance Core.NFData ListRemoteAccessSessionsResponse
