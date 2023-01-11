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
-- Module      : Amazonka.IVS.ListPlaybackKeyPairs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets summary information about playback key pairs. For more information,
-- see
-- <https://docs.aws.amazon.com/ivs/latest/userguide/private-channels.html Setting Up Private Channels>
-- in the /Amazon IVS User Guide/.
--
-- This operation returns paginated results.
module Amazonka.IVS.ListPlaybackKeyPairs
  ( -- * Creating a Request
    ListPlaybackKeyPairs (..),
    newListPlaybackKeyPairs,

    -- * Request Lenses
    listPlaybackKeyPairs_maxResults,
    listPlaybackKeyPairs_nextToken,

    -- * Destructuring the Response
    ListPlaybackKeyPairsResponse (..),
    newListPlaybackKeyPairsResponse,

    -- * Response Lenses
    listPlaybackKeyPairsResponse_nextToken,
    listPlaybackKeyPairsResponse_httpStatus,
    listPlaybackKeyPairsResponse_keyPairs,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVS.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListPlaybackKeyPairs' smart constructor.
data ListPlaybackKeyPairs = ListPlaybackKeyPairs'
  { -- | Maximum number of key pairs to return. Default: your service quota or
    -- 100, whichever is smaller.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The first key pair to retrieve. This is used for pagination; see the
    -- @nextToken@ response field.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPlaybackKeyPairs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listPlaybackKeyPairs_maxResults' - Maximum number of key pairs to return. Default: your service quota or
-- 100, whichever is smaller.
--
-- 'nextToken', 'listPlaybackKeyPairs_nextToken' - The first key pair to retrieve. This is used for pagination; see the
-- @nextToken@ response field.
newListPlaybackKeyPairs ::
  ListPlaybackKeyPairs
newListPlaybackKeyPairs =
  ListPlaybackKeyPairs'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Maximum number of key pairs to return. Default: your service quota or
-- 100, whichever is smaller.
listPlaybackKeyPairs_maxResults :: Lens.Lens' ListPlaybackKeyPairs (Prelude.Maybe Prelude.Natural)
listPlaybackKeyPairs_maxResults = Lens.lens (\ListPlaybackKeyPairs' {maxResults} -> maxResults) (\s@ListPlaybackKeyPairs' {} a -> s {maxResults = a} :: ListPlaybackKeyPairs)

-- | The first key pair to retrieve. This is used for pagination; see the
-- @nextToken@ response field.
listPlaybackKeyPairs_nextToken :: Lens.Lens' ListPlaybackKeyPairs (Prelude.Maybe Prelude.Text)
listPlaybackKeyPairs_nextToken = Lens.lens (\ListPlaybackKeyPairs' {nextToken} -> nextToken) (\s@ListPlaybackKeyPairs' {} a -> s {nextToken = a} :: ListPlaybackKeyPairs)

instance Core.AWSPager ListPlaybackKeyPairs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listPlaybackKeyPairsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listPlaybackKeyPairsResponse_keyPairs) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listPlaybackKeyPairs_nextToken
          Lens..~ rs
          Lens.^? listPlaybackKeyPairsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListPlaybackKeyPairs where
  type
    AWSResponse ListPlaybackKeyPairs =
      ListPlaybackKeyPairsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListPlaybackKeyPairsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "keyPairs" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListPlaybackKeyPairs where
  hashWithSalt _salt ListPlaybackKeyPairs' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListPlaybackKeyPairs where
  rnf ListPlaybackKeyPairs' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListPlaybackKeyPairs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListPlaybackKeyPairs where
  toJSON ListPlaybackKeyPairs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListPlaybackKeyPairs where
  toPath = Prelude.const "/ListPlaybackKeyPairs"

instance Data.ToQuery ListPlaybackKeyPairs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListPlaybackKeyPairsResponse' smart constructor.
data ListPlaybackKeyPairsResponse = ListPlaybackKeyPairsResponse'
  { -- | If there are more key pairs than @maxResults@, use @nextToken@ in the
    -- request to get the next set.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | List of key pairs.
    keyPairs :: [PlaybackKeyPairSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListPlaybackKeyPairsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listPlaybackKeyPairsResponse_nextToken' - If there are more key pairs than @maxResults@, use @nextToken@ in the
-- request to get the next set.
--
-- 'httpStatus', 'listPlaybackKeyPairsResponse_httpStatus' - The response's http status code.
--
-- 'keyPairs', 'listPlaybackKeyPairsResponse_keyPairs' - List of key pairs.
newListPlaybackKeyPairsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListPlaybackKeyPairsResponse
newListPlaybackKeyPairsResponse pHttpStatus_ =
  ListPlaybackKeyPairsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      keyPairs = Prelude.mempty
    }

-- | If there are more key pairs than @maxResults@, use @nextToken@ in the
-- request to get the next set.
listPlaybackKeyPairsResponse_nextToken :: Lens.Lens' ListPlaybackKeyPairsResponse (Prelude.Maybe Prelude.Text)
listPlaybackKeyPairsResponse_nextToken = Lens.lens (\ListPlaybackKeyPairsResponse' {nextToken} -> nextToken) (\s@ListPlaybackKeyPairsResponse' {} a -> s {nextToken = a} :: ListPlaybackKeyPairsResponse)

-- | The response's http status code.
listPlaybackKeyPairsResponse_httpStatus :: Lens.Lens' ListPlaybackKeyPairsResponse Prelude.Int
listPlaybackKeyPairsResponse_httpStatus = Lens.lens (\ListPlaybackKeyPairsResponse' {httpStatus} -> httpStatus) (\s@ListPlaybackKeyPairsResponse' {} a -> s {httpStatus = a} :: ListPlaybackKeyPairsResponse)

-- | List of key pairs.
listPlaybackKeyPairsResponse_keyPairs :: Lens.Lens' ListPlaybackKeyPairsResponse [PlaybackKeyPairSummary]
listPlaybackKeyPairsResponse_keyPairs = Lens.lens (\ListPlaybackKeyPairsResponse' {keyPairs} -> keyPairs) (\s@ListPlaybackKeyPairsResponse' {} a -> s {keyPairs = a} :: ListPlaybackKeyPairsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListPlaybackKeyPairsResponse where
  rnf ListPlaybackKeyPairsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf keyPairs
