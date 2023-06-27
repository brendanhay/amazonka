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
-- Module      : Amazonka.FraudDetector.GetListsMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the metadata of either all the lists under the account or the
-- specified list.
module Amazonka.FraudDetector.GetListsMetadata
  ( -- * Creating a Request
    GetListsMetadata (..),
    newGetListsMetadata,

    -- * Request Lenses
    getListsMetadata_maxResults,
    getListsMetadata_name,
    getListsMetadata_nextToken,

    -- * Destructuring the Response
    GetListsMetadataResponse (..),
    newGetListsMetadataResponse,

    -- * Response Lenses
    getListsMetadataResponse_lists,
    getListsMetadataResponse_nextToken,
    getListsMetadataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetListsMetadata' smart constructor.
data GetListsMetadata = GetListsMetadata'
  { -- | The maximum number of objects to return for the request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The name of the list.
    name :: Prelude.Maybe Prelude.Text,
    -- | The next token for the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetListsMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getListsMetadata_maxResults' - The maximum number of objects to return for the request.
--
-- 'name', 'getListsMetadata_name' - The name of the list.
--
-- 'nextToken', 'getListsMetadata_nextToken' - The next token for the subsequent request.
newGetListsMetadata ::
  GetListsMetadata
newGetListsMetadata =
  GetListsMetadata'
    { maxResults = Prelude.Nothing,
      name = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of objects to return for the request.
getListsMetadata_maxResults :: Lens.Lens' GetListsMetadata (Prelude.Maybe Prelude.Natural)
getListsMetadata_maxResults = Lens.lens (\GetListsMetadata' {maxResults} -> maxResults) (\s@GetListsMetadata' {} a -> s {maxResults = a} :: GetListsMetadata)

-- | The name of the list.
getListsMetadata_name :: Lens.Lens' GetListsMetadata (Prelude.Maybe Prelude.Text)
getListsMetadata_name = Lens.lens (\GetListsMetadata' {name} -> name) (\s@GetListsMetadata' {} a -> s {name = a} :: GetListsMetadata)

-- | The next token for the subsequent request.
getListsMetadata_nextToken :: Lens.Lens' GetListsMetadata (Prelude.Maybe Prelude.Text)
getListsMetadata_nextToken = Lens.lens (\GetListsMetadata' {nextToken} -> nextToken) (\s@GetListsMetadata' {} a -> s {nextToken = a} :: GetListsMetadata)

instance Core.AWSRequest GetListsMetadata where
  type
    AWSResponse GetListsMetadata =
      GetListsMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetListsMetadataResponse'
            Prelude.<$> (x Data..?> "lists" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetListsMetadata where
  hashWithSalt _salt GetListsMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetListsMetadata where
  rnf GetListsMetadata' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetListsMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.GetListsMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetListsMetadata where
  toJSON GetListsMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("maxResults" Data..=) Prelude.<$> maxResults,
            ("name" Data..=) Prelude.<$> name,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetListsMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery GetListsMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetListsMetadataResponse' smart constructor.
data GetListsMetadataResponse = GetListsMetadataResponse'
  { -- | The metadata of the specified list or all lists under the account.
    lists :: Prelude.Maybe [AllowDenyList],
    -- | The next page token.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetListsMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lists', 'getListsMetadataResponse_lists' - The metadata of the specified list or all lists under the account.
--
-- 'nextToken', 'getListsMetadataResponse_nextToken' - The next page token.
--
-- 'httpStatus', 'getListsMetadataResponse_httpStatus' - The response's http status code.
newGetListsMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetListsMetadataResponse
newGetListsMetadataResponse pHttpStatus_ =
  GetListsMetadataResponse'
    { lists = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The metadata of the specified list or all lists under the account.
getListsMetadataResponse_lists :: Lens.Lens' GetListsMetadataResponse (Prelude.Maybe [AllowDenyList])
getListsMetadataResponse_lists = Lens.lens (\GetListsMetadataResponse' {lists} -> lists) (\s@GetListsMetadataResponse' {} a -> s {lists = a} :: GetListsMetadataResponse) Prelude.. Lens.mapping Lens.coerced

-- | The next page token.
getListsMetadataResponse_nextToken :: Lens.Lens' GetListsMetadataResponse (Prelude.Maybe Prelude.Text)
getListsMetadataResponse_nextToken = Lens.lens (\GetListsMetadataResponse' {nextToken} -> nextToken) (\s@GetListsMetadataResponse' {} a -> s {nextToken = a} :: GetListsMetadataResponse)

-- | The response's http status code.
getListsMetadataResponse_httpStatus :: Lens.Lens' GetListsMetadataResponse Prelude.Int
getListsMetadataResponse_httpStatus = Lens.lens (\GetListsMetadataResponse' {httpStatus} -> httpStatus) (\s@GetListsMetadataResponse' {} a -> s {httpStatus = a} :: GetListsMetadataResponse)

instance Prelude.NFData GetListsMetadataResponse where
  rnf GetListsMetadataResponse' {..} =
    Prelude.rnf lists
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
