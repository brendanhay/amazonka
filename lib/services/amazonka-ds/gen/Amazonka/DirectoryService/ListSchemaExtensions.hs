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
-- Module      : Amazonka.DirectoryService.ListSchemaExtensions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all schema extensions applied to a Microsoft AD Directory.
--
-- This operation returns paginated results.
module Amazonka.DirectoryService.ListSchemaExtensions
  ( -- * Creating a Request
    ListSchemaExtensions (..),
    newListSchemaExtensions,

    -- * Request Lenses
    listSchemaExtensions_limit,
    listSchemaExtensions_nextToken,
    listSchemaExtensions_directoryId,

    -- * Destructuring the Response
    ListSchemaExtensionsResponse (..),
    newListSchemaExtensionsResponse,

    -- * Response Lenses
    listSchemaExtensionsResponse_nextToken,
    listSchemaExtensionsResponse_schemaExtensionsInfo,
    listSchemaExtensionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListSchemaExtensions' smart constructor.
data ListSchemaExtensions = ListSchemaExtensions'
  { -- | The maximum number of items to return.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @ListSchemaExtensions.NextToken@ value from a previous call to
    -- @ListSchemaExtensions@. Pass null if this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the directory from which to retrieve the schema
    -- extension information.
    directoryId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSchemaExtensions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'limit', 'listSchemaExtensions_limit' - The maximum number of items to return.
--
-- 'nextToken', 'listSchemaExtensions_nextToken' - The @ListSchemaExtensions.NextToken@ value from a previous call to
-- @ListSchemaExtensions@. Pass null if this is the first call.
--
-- 'directoryId', 'listSchemaExtensions_directoryId' - The identifier of the directory from which to retrieve the schema
-- extension information.
newListSchemaExtensions ::
  -- | 'directoryId'
  Prelude.Text ->
  ListSchemaExtensions
newListSchemaExtensions pDirectoryId_ =
  ListSchemaExtensions'
    { limit = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      directoryId = pDirectoryId_
    }

-- | The maximum number of items to return.
listSchemaExtensions_limit :: Lens.Lens' ListSchemaExtensions (Prelude.Maybe Prelude.Natural)
listSchemaExtensions_limit = Lens.lens (\ListSchemaExtensions' {limit} -> limit) (\s@ListSchemaExtensions' {} a -> s {limit = a} :: ListSchemaExtensions)

-- | The @ListSchemaExtensions.NextToken@ value from a previous call to
-- @ListSchemaExtensions@. Pass null if this is the first call.
listSchemaExtensions_nextToken :: Lens.Lens' ListSchemaExtensions (Prelude.Maybe Prelude.Text)
listSchemaExtensions_nextToken = Lens.lens (\ListSchemaExtensions' {nextToken} -> nextToken) (\s@ListSchemaExtensions' {} a -> s {nextToken = a} :: ListSchemaExtensions)

-- | The identifier of the directory from which to retrieve the schema
-- extension information.
listSchemaExtensions_directoryId :: Lens.Lens' ListSchemaExtensions Prelude.Text
listSchemaExtensions_directoryId = Lens.lens (\ListSchemaExtensions' {directoryId} -> directoryId) (\s@ListSchemaExtensions' {} a -> s {directoryId = a} :: ListSchemaExtensions)

instance Core.AWSPager ListSchemaExtensions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listSchemaExtensionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listSchemaExtensionsResponse_schemaExtensionsInfo
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listSchemaExtensions_nextToken
          Lens..~ rs
          Lens.^? listSchemaExtensionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListSchemaExtensions where
  type
    AWSResponse ListSchemaExtensions =
      ListSchemaExtensionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSchemaExtensionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x
                            Data..?> "SchemaExtensionsInfo"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSchemaExtensions where
  hashWithSalt _salt ListSchemaExtensions' {..} =
    _salt
      `Prelude.hashWithSalt` limit
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` directoryId

instance Prelude.NFData ListSchemaExtensions where
  rnf ListSchemaExtensions' {..} =
    Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf directoryId

instance Data.ToHeaders ListSchemaExtensions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.ListSchemaExtensions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListSchemaExtensions where
  toJSON ListSchemaExtensions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("DirectoryId" Data..= directoryId)
          ]
      )

instance Data.ToPath ListSchemaExtensions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListSchemaExtensions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListSchemaExtensionsResponse' smart constructor.
data ListSchemaExtensionsResponse = ListSchemaExtensionsResponse'
  { -- | If not null, more results are available. Pass this value for the
    -- @NextToken@ parameter in a subsequent call to @ListSchemaExtensions@ to
    -- retrieve the next set of items.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the schema extensions applied to the directory.
    schemaExtensionsInfo :: Prelude.Maybe [SchemaExtensionInfo],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListSchemaExtensionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listSchemaExtensionsResponse_nextToken' - If not null, more results are available. Pass this value for the
-- @NextToken@ parameter in a subsequent call to @ListSchemaExtensions@ to
-- retrieve the next set of items.
--
-- 'schemaExtensionsInfo', 'listSchemaExtensionsResponse_schemaExtensionsInfo' - Information about the schema extensions applied to the directory.
--
-- 'httpStatus', 'listSchemaExtensionsResponse_httpStatus' - The response's http status code.
newListSchemaExtensionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListSchemaExtensionsResponse
newListSchemaExtensionsResponse pHttpStatus_ =
  ListSchemaExtensionsResponse'
    { nextToken =
        Prelude.Nothing,
      schemaExtensionsInfo = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If not null, more results are available. Pass this value for the
-- @NextToken@ parameter in a subsequent call to @ListSchemaExtensions@ to
-- retrieve the next set of items.
listSchemaExtensionsResponse_nextToken :: Lens.Lens' ListSchemaExtensionsResponse (Prelude.Maybe Prelude.Text)
listSchemaExtensionsResponse_nextToken = Lens.lens (\ListSchemaExtensionsResponse' {nextToken} -> nextToken) (\s@ListSchemaExtensionsResponse' {} a -> s {nextToken = a} :: ListSchemaExtensionsResponse)

-- | Information about the schema extensions applied to the directory.
listSchemaExtensionsResponse_schemaExtensionsInfo :: Lens.Lens' ListSchemaExtensionsResponse (Prelude.Maybe [SchemaExtensionInfo])
listSchemaExtensionsResponse_schemaExtensionsInfo = Lens.lens (\ListSchemaExtensionsResponse' {schemaExtensionsInfo} -> schemaExtensionsInfo) (\s@ListSchemaExtensionsResponse' {} a -> s {schemaExtensionsInfo = a} :: ListSchemaExtensionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listSchemaExtensionsResponse_httpStatus :: Lens.Lens' ListSchemaExtensionsResponse Prelude.Int
listSchemaExtensionsResponse_httpStatus = Lens.lens (\ListSchemaExtensionsResponse' {httpStatus} -> httpStatus) (\s@ListSchemaExtensionsResponse' {} a -> s {httpStatus = a} :: ListSchemaExtensionsResponse)

instance Prelude.NFData ListSchemaExtensionsResponse where
  rnf ListSchemaExtensionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf schemaExtensionsInfo
      `Prelude.seq` Prelude.rnf httpStatus
