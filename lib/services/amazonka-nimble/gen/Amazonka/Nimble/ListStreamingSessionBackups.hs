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
-- Module      : Amazonka.Nimble.ListStreamingSessionBackups
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the backups of a streaming session in a studio.
--
-- This operation returns paginated results.
module Amazonka.Nimble.ListStreamingSessionBackups
  ( -- * Creating a Request
    ListStreamingSessionBackups (..),
    newListStreamingSessionBackups,

    -- * Request Lenses
    listStreamingSessionBackups_nextToken,
    listStreamingSessionBackups_ownedBy,
    listStreamingSessionBackups_studioId,

    -- * Destructuring the Response
    ListStreamingSessionBackupsResponse (..),
    newListStreamingSessionBackupsResponse,

    -- * Response Lenses
    listStreamingSessionBackupsResponse_nextToken,
    listStreamingSessionBackupsResponse_streamingSessionBackups,
    listStreamingSessionBackupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Nimble.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStreamingSessionBackups' smart constructor.
data ListStreamingSessionBackups = ListStreamingSessionBackups'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The user ID of the user that owns the streaming session.
    ownedBy :: Prelude.Maybe Prelude.Text,
    -- | The studio ID.
    studioId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStreamingSessionBackups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStreamingSessionBackups_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'ownedBy', 'listStreamingSessionBackups_ownedBy' - The user ID of the user that owns the streaming session.
--
-- 'studioId', 'listStreamingSessionBackups_studioId' - The studio ID.
newListStreamingSessionBackups ::
  -- | 'studioId'
  Prelude.Text ->
  ListStreamingSessionBackups
newListStreamingSessionBackups pStudioId_ =
  ListStreamingSessionBackups'
    { nextToken =
        Prelude.Nothing,
      ownedBy = Prelude.Nothing,
      studioId = pStudioId_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listStreamingSessionBackups_nextToken :: Lens.Lens' ListStreamingSessionBackups (Prelude.Maybe Prelude.Text)
listStreamingSessionBackups_nextToken = Lens.lens (\ListStreamingSessionBackups' {nextToken} -> nextToken) (\s@ListStreamingSessionBackups' {} a -> s {nextToken = a} :: ListStreamingSessionBackups)

-- | The user ID of the user that owns the streaming session.
listStreamingSessionBackups_ownedBy :: Lens.Lens' ListStreamingSessionBackups (Prelude.Maybe Prelude.Text)
listStreamingSessionBackups_ownedBy = Lens.lens (\ListStreamingSessionBackups' {ownedBy} -> ownedBy) (\s@ListStreamingSessionBackups' {} a -> s {ownedBy = a} :: ListStreamingSessionBackups)

-- | The studio ID.
listStreamingSessionBackups_studioId :: Lens.Lens' ListStreamingSessionBackups Prelude.Text
listStreamingSessionBackups_studioId = Lens.lens (\ListStreamingSessionBackups' {studioId} -> studioId) (\s@ListStreamingSessionBackups' {} a -> s {studioId = a} :: ListStreamingSessionBackups)

instance Core.AWSPager ListStreamingSessionBackups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStreamingSessionBackupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStreamingSessionBackupsResponse_streamingSessionBackups
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listStreamingSessionBackups_nextToken
          Lens..~ rs
          Lens.^? listStreamingSessionBackupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListStreamingSessionBackups where
  type
    AWSResponse ListStreamingSessionBackups =
      ListStreamingSessionBackupsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStreamingSessionBackupsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> ( x Data..?> "streamingSessionBackups"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStreamingSessionBackups where
  hashWithSalt _salt ListStreamingSessionBackups' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` ownedBy
      `Prelude.hashWithSalt` studioId

instance Prelude.NFData ListStreamingSessionBackups where
  rnf ListStreamingSessionBackups' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf ownedBy
      `Prelude.seq` Prelude.rnf studioId

instance Data.ToHeaders ListStreamingSessionBackups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListStreamingSessionBackups where
  toPath ListStreamingSessionBackups' {..} =
    Prelude.mconcat
      [ "/2020-08-01/studios/",
        Data.toBS studioId,
        "/streaming-session-backups"
      ]

instance Data.ToQuery ListStreamingSessionBackups where
  toQuery ListStreamingSessionBackups' {..} =
    Prelude.mconcat
      [ "nextToken" Data.=: nextToken,
        "ownedBy" Data.=: ownedBy
      ]

-- | /See:/ 'newListStreamingSessionBackupsResponse' smart constructor.
data ListStreamingSessionBackupsResponse = ListStreamingSessionBackupsResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the streaming session backups.
    streamingSessionBackups :: Prelude.Maybe [StreamingSessionBackup],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStreamingSessionBackupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStreamingSessionBackupsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'streamingSessionBackups', 'listStreamingSessionBackupsResponse_streamingSessionBackups' - Information about the streaming session backups.
--
-- 'httpStatus', 'listStreamingSessionBackupsResponse_httpStatus' - The response's http status code.
newListStreamingSessionBackupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStreamingSessionBackupsResponse
newListStreamingSessionBackupsResponse pHttpStatus_ =
  ListStreamingSessionBackupsResponse'
    { nextToken =
        Prelude.Nothing,
      streamingSessionBackups =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listStreamingSessionBackupsResponse_nextToken :: Lens.Lens' ListStreamingSessionBackupsResponse (Prelude.Maybe Prelude.Text)
listStreamingSessionBackupsResponse_nextToken = Lens.lens (\ListStreamingSessionBackupsResponse' {nextToken} -> nextToken) (\s@ListStreamingSessionBackupsResponse' {} a -> s {nextToken = a} :: ListStreamingSessionBackupsResponse)

-- | Information about the streaming session backups.
listStreamingSessionBackupsResponse_streamingSessionBackups :: Lens.Lens' ListStreamingSessionBackupsResponse (Prelude.Maybe [StreamingSessionBackup])
listStreamingSessionBackupsResponse_streamingSessionBackups = Lens.lens (\ListStreamingSessionBackupsResponse' {streamingSessionBackups} -> streamingSessionBackups) (\s@ListStreamingSessionBackupsResponse' {} a -> s {streamingSessionBackups = a} :: ListStreamingSessionBackupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listStreamingSessionBackupsResponse_httpStatus :: Lens.Lens' ListStreamingSessionBackupsResponse Prelude.Int
listStreamingSessionBackupsResponse_httpStatus = Lens.lens (\ListStreamingSessionBackupsResponse' {httpStatus} -> httpStatus) (\s@ListStreamingSessionBackupsResponse' {} a -> s {httpStatus = a} :: ListStreamingSessionBackupsResponse)

instance
  Prelude.NFData
    ListStreamingSessionBackupsResponse
  where
  rnf ListStreamingSessionBackupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf streamingSessionBackups
      `Prelude.seq` Prelude.rnf httpStatus
