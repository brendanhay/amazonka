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
-- Module      : Network.AWS.DirectoryService.ListSchemaExtensions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all schema extensions applied to a Microsoft AD Directory.
--
-- This operation returns paginated results.
module Network.AWS.DirectoryService.ListSchemaExtensions
  ( -- * Creating a Request
    ListSchemaExtensions (..),
    newListSchemaExtensions,

    -- * Request Lenses
    listSchemaExtensions_nextToken,
    listSchemaExtensions_limit,
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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListSchemaExtensions' smart constructor.
data ListSchemaExtensions = ListSchemaExtensions'
  { -- | The @ListSchemaExtensions.NextToken@ value from a previous call to
    -- @ListSchemaExtensions@. Pass null if this is the first call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of items to return.
    limit :: Prelude.Maybe Prelude.Natural,
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
-- 'nextToken', 'listSchemaExtensions_nextToken' - The @ListSchemaExtensions.NextToken@ value from a previous call to
-- @ListSchemaExtensions@. Pass null if this is the first call.
--
-- 'limit', 'listSchemaExtensions_limit' - The maximum number of items to return.
--
-- 'directoryId', 'listSchemaExtensions_directoryId' - The identifier of the directory from which to retrieve the schema
-- extension information.
newListSchemaExtensions ::
  -- | 'directoryId'
  Prelude.Text ->
  ListSchemaExtensions
newListSchemaExtensions pDirectoryId_ =
  ListSchemaExtensions'
    { nextToken = Prelude.Nothing,
      limit = Prelude.Nothing,
      directoryId = pDirectoryId_
    }

-- | The @ListSchemaExtensions.NextToken@ value from a previous call to
-- @ListSchemaExtensions@. Pass null if this is the first call.
listSchemaExtensions_nextToken :: Lens.Lens' ListSchemaExtensions (Prelude.Maybe Prelude.Text)
listSchemaExtensions_nextToken = Lens.lens (\ListSchemaExtensions' {nextToken} -> nextToken) (\s@ListSchemaExtensions' {} a -> s {nextToken = a} :: ListSchemaExtensions)

-- | The maximum number of items to return.
listSchemaExtensions_limit :: Lens.Lens' ListSchemaExtensions (Prelude.Maybe Prelude.Natural)
listSchemaExtensions_limit = Lens.lens (\ListSchemaExtensions' {limit} -> limit) (\s@ListSchemaExtensions' {} a -> s {limit = a} :: ListSchemaExtensions)

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
      Prelude.Just Prelude.$
        rq
          Prelude.& listSchemaExtensions_nextToken
          Lens..~ rs
          Lens.^? listSchemaExtensionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListSchemaExtensions where
  type
    AWSResponse ListSchemaExtensions =
      ListSchemaExtensionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListSchemaExtensionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "SchemaExtensionsInfo"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListSchemaExtensions

instance Prelude.NFData ListSchemaExtensions

instance Core.ToHeaders ListSchemaExtensions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.ListSchemaExtensions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListSchemaExtensions where
  toJSON ListSchemaExtensions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("Limit" Core..=) Prelude.<$> limit,
            Prelude.Just ("DirectoryId" Core..= directoryId)
          ]
      )

instance Core.ToPath ListSchemaExtensions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListSchemaExtensions where
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
listSchemaExtensionsResponse_schemaExtensionsInfo = Lens.lens (\ListSchemaExtensionsResponse' {schemaExtensionsInfo} -> schemaExtensionsInfo) (\s@ListSchemaExtensionsResponse' {} a -> s {schemaExtensionsInfo = a} :: ListSchemaExtensionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listSchemaExtensionsResponse_httpStatus :: Lens.Lens' ListSchemaExtensionsResponse Prelude.Int
listSchemaExtensionsResponse_httpStatus = Lens.lens (\ListSchemaExtensionsResponse' {httpStatus} -> httpStatus) (\s@ListSchemaExtensionsResponse' {} a -> s {httpStatus = a} :: ListSchemaExtensionsResponse)

instance Prelude.NFData ListSchemaExtensionsResponse
