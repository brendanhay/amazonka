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
-- Module      : Amazonka.DataSync.ListStorageSystems
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the on-premises storage systems that you\'re using with DataSync
-- Discovery.
--
-- This operation returns paginated results.
module Amazonka.DataSync.ListStorageSystems
  ( -- * Creating a Request
    ListStorageSystems (..),
    newListStorageSystems,

    -- * Request Lenses
    listStorageSystems_maxResults,
    listStorageSystems_nextToken,

    -- * Destructuring the Response
    ListStorageSystemsResponse (..),
    newListStorageSystemsResponse,

    -- * Response Lenses
    listStorageSystemsResponse_nextToken,
    listStorageSystemsResponse_storageSystems,
    listStorageSystemsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListStorageSystems' smart constructor.
data ListStorageSystems = ListStorageSystems'
  { -- | Specifies how many results you want in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Specifies an opaque string that indicates the position to begin the next
    -- list of results in the response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStorageSystems' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listStorageSystems_maxResults' - Specifies how many results you want in the response.
--
-- 'nextToken', 'listStorageSystems_nextToken' - Specifies an opaque string that indicates the position to begin the next
-- list of results in the response.
newListStorageSystems ::
  ListStorageSystems
newListStorageSystems =
  ListStorageSystems'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Specifies how many results you want in the response.
listStorageSystems_maxResults :: Lens.Lens' ListStorageSystems (Prelude.Maybe Prelude.Natural)
listStorageSystems_maxResults = Lens.lens (\ListStorageSystems' {maxResults} -> maxResults) (\s@ListStorageSystems' {} a -> s {maxResults = a} :: ListStorageSystems)

-- | Specifies an opaque string that indicates the position to begin the next
-- list of results in the response.
listStorageSystems_nextToken :: Lens.Lens' ListStorageSystems (Prelude.Maybe Prelude.Text)
listStorageSystems_nextToken = Lens.lens (\ListStorageSystems' {nextToken} -> nextToken) (\s@ListStorageSystems' {} a -> s {nextToken = a} :: ListStorageSystems)

instance Core.AWSPager ListStorageSystems where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listStorageSystemsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listStorageSystemsResponse_storageSystems
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listStorageSystems_nextToken
          Lens..~ rs
          Lens.^? listStorageSystemsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListStorageSystems where
  type
    AWSResponse ListStorageSystems =
      ListStorageSystemsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListStorageSystemsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "StorageSystems" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListStorageSystems where
  hashWithSalt _salt ListStorageSystems' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListStorageSystems where
  rnf ListStorageSystems' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListStorageSystems where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.ListStorageSystems" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListStorageSystems where
  toJSON ListStorageSystems' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListStorageSystems where
  toPath = Prelude.const "/"

instance Data.ToQuery ListStorageSystems where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListStorageSystemsResponse' smart constructor.
data ListStorageSystemsResponse = ListStorageSystemsResponse'
  { -- | The opaque string that indicates the position to begin the next list of
    -- results in the response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Names ARNs) of the on-premises storage systems that
    -- you\'re using with DataSync Discovery.
    storageSystems :: Prelude.Maybe [StorageSystemListEntry],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListStorageSystemsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listStorageSystemsResponse_nextToken' - The opaque string that indicates the position to begin the next list of
-- results in the response.
--
-- 'storageSystems', 'listStorageSystemsResponse_storageSystems' - The Amazon Resource Names ARNs) of the on-premises storage systems that
-- you\'re using with DataSync Discovery.
--
-- 'httpStatus', 'listStorageSystemsResponse_httpStatus' - The response's http status code.
newListStorageSystemsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListStorageSystemsResponse
newListStorageSystemsResponse pHttpStatus_ =
  ListStorageSystemsResponse'
    { nextToken =
        Prelude.Nothing,
      storageSystems = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The opaque string that indicates the position to begin the next list of
-- results in the response.
listStorageSystemsResponse_nextToken :: Lens.Lens' ListStorageSystemsResponse (Prelude.Maybe Prelude.Text)
listStorageSystemsResponse_nextToken = Lens.lens (\ListStorageSystemsResponse' {nextToken} -> nextToken) (\s@ListStorageSystemsResponse' {} a -> s {nextToken = a} :: ListStorageSystemsResponse)

-- | The Amazon Resource Names ARNs) of the on-premises storage systems that
-- you\'re using with DataSync Discovery.
listStorageSystemsResponse_storageSystems :: Lens.Lens' ListStorageSystemsResponse (Prelude.Maybe [StorageSystemListEntry])
listStorageSystemsResponse_storageSystems = Lens.lens (\ListStorageSystemsResponse' {storageSystems} -> storageSystems) (\s@ListStorageSystemsResponse' {} a -> s {storageSystems = a} :: ListStorageSystemsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listStorageSystemsResponse_httpStatus :: Lens.Lens' ListStorageSystemsResponse Prelude.Int
listStorageSystemsResponse_httpStatus = Lens.lens (\ListStorageSystemsResponse' {httpStatus} -> httpStatus) (\s@ListStorageSystemsResponse' {} a -> s {httpStatus = a} :: ListStorageSystemsResponse)

instance Prelude.NFData ListStorageSystemsResponse where
  rnf ListStorageSystemsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf storageSystems
      `Prelude.seq` Prelude.rnf httpStatus
