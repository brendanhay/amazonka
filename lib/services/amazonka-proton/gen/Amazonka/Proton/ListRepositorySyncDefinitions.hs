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
-- Module      : Amazonka.Proton.ListRepositorySyncDefinitions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List repository sync definitions with detail data.
--
-- This operation returns paginated results.
module Amazonka.Proton.ListRepositorySyncDefinitions
  ( -- * Creating a Request
    ListRepositorySyncDefinitions (..),
    newListRepositorySyncDefinitions,

    -- * Request Lenses
    listRepositorySyncDefinitions_nextToken,
    listRepositorySyncDefinitions_repositoryName,
    listRepositorySyncDefinitions_repositoryProvider,
    listRepositorySyncDefinitions_syncType,

    -- * Destructuring the Response
    ListRepositorySyncDefinitionsResponse (..),
    newListRepositorySyncDefinitionsResponse,

    -- * Response Lenses
    listRepositorySyncDefinitionsResponse_nextToken,
    listRepositorySyncDefinitionsResponse_httpStatus,
    listRepositorySyncDefinitionsResponse_syncDefinitions,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListRepositorySyncDefinitions' smart constructor.
data ListRepositorySyncDefinitions = ListRepositorySyncDefinitions'
  { -- | A token that indicates the location of the next repository sync
    -- definition in the array of repository sync definitions, after the list
    -- of repository sync definitions previously requested.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The repository name.
    repositoryName :: Prelude.Text,
    -- | The repository provider.
    repositoryProvider :: RepositoryProvider,
    -- | The sync type. The only supported value is @TEMPLATE_SYNC@.
    syncType :: SyncType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRepositorySyncDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRepositorySyncDefinitions_nextToken' - A token that indicates the location of the next repository sync
-- definition in the array of repository sync definitions, after the list
-- of repository sync definitions previously requested.
--
-- 'repositoryName', 'listRepositorySyncDefinitions_repositoryName' - The repository name.
--
-- 'repositoryProvider', 'listRepositorySyncDefinitions_repositoryProvider' - The repository provider.
--
-- 'syncType', 'listRepositorySyncDefinitions_syncType' - The sync type. The only supported value is @TEMPLATE_SYNC@.
newListRepositorySyncDefinitions ::
  -- | 'repositoryName'
  Prelude.Text ->
  -- | 'repositoryProvider'
  RepositoryProvider ->
  -- | 'syncType'
  SyncType ->
  ListRepositorySyncDefinitions
newListRepositorySyncDefinitions
  pRepositoryName_
  pRepositoryProvider_
  pSyncType_ =
    ListRepositorySyncDefinitions'
      { nextToken =
          Prelude.Nothing,
        repositoryName = pRepositoryName_,
        repositoryProvider = pRepositoryProvider_,
        syncType = pSyncType_
      }

-- | A token that indicates the location of the next repository sync
-- definition in the array of repository sync definitions, after the list
-- of repository sync definitions previously requested.
listRepositorySyncDefinitions_nextToken :: Lens.Lens' ListRepositorySyncDefinitions (Prelude.Maybe Prelude.Text)
listRepositorySyncDefinitions_nextToken = Lens.lens (\ListRepositorySyncDefinitions' {nextToken} -> nextToken) (\s@ListRepositorySyncDefinitions' {} a -> s {nextToken = a} :: ListRepositorySyncDefinitions)

-- | The repository name.
listRepositorySyncDefinitions_repositoryName :: Lens.Lens' ListRepositorySyncDefinitions Prelude.Text
listRepositorySyncDefinitions_repositoryName = Lens.lens (\ListRepositorySyncDefinitions' {repositoryName} -> repositoryName) (\s@ListRepositorySyncDefinitions' {} a -> s {repositoryName = a} :: ListRepositorySyncDefinitions)

-- | The repository provider.
listRepositorySyncDefinitions_repositoryProvider :: Lens.Lens' ListRepositorySyncDefinitions RepositoryProvider
listRepositorySyncDefinitions_repositoryProvider = Lens.lens (\ListRepositorySyncDefinitions' {repositoryProvider} -> repositoryProvider) (\s@ListRepositorySyncDefinitions' {} a -> s {repositoryProvider = a} :: ListRepositorySyncDefinitions)

-- | The sync type. The only supported value is @TEMPLATE_SYNC@.
listRepositorySyncDefinitions_syncType :: Lens.Lens' ListRepositorySyncDefinitions SyncType
listRepositorySyncDefinitions_syncType = Lens.lens (\ListRepositorySyncDefinitions' {syncType} -> syncType) (\s@ListRepositorySyncDefinitions' {} a -> s {syncType = a} :: ListRepositorySyncDefinitions)

instance Core.AWSPager ListRepositorySyncDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRepositorySyncDefinitionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listRepositorySyncDefinitionsResponse_syncDefinitions
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRepositorySyncDefinitions_nextToken
          Lens..~ rs
          Lens.^? listRepositorySyncDefinitionsResponse_nextToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListRepositorySyncDefinitions
  where
  type
    AWSResponse ListRepositorySyncDefinitions =
      ListRepositorySyncDefinitionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRepositorySyncDefinitionsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "syncDefinitions"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListRepositorySyncDefinitions
  where
  hashWithSalt _salt ListRepositorySyncDefinitions' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` repositoryName
      `Prelude.hashWithSalt` repositoryProvider
      `Prelude.hashWithSalt` syncType

instance Prelude.NFData ListRepositorySyncDefinitions where
  rnf ListRepositorySyncDefinitions' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf repositoryName
      `Prelude.seq` Prelude.rnf repositoryProvider
      `Prelude.seq` Prelude.rnf syncType

instance Core.ToHeaders ListRepositorySyncDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AwsProton20200720.ListRepositorySyncDefinitions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListRepositorySyncDefinitions where
  toJSON ListRepositorySyncDefinitions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            Prelude.Just
              ("repositoryName" Core..= repositoryName),
            Prelude.Just
              ("repositoryProvider" Core..= repositoryProvider),
            Prelude.Just ("syncType" Core..= syncType)
          ]
      )

instance Core.ToPath ListRepositorySyncDefinitions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListRepositorySyncDefinitions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRepositorySyncDefinitionsResponse' smart constructor.
data ListRepositorySyncDefinitionsResponse = ListRepositorySyncDefinitionsResponse'
  { -- | A token that indicates the location of the next repository sync
    -- definition in the array of repository sync definitions, after the
    -- current requested list of repository sync definitions.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array of repository sync definitions.
    syncDefinitions :: [RepositorySyncDefinition]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRepositorySyncDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRepositorySyncDefinitionsResponse_nextToken' - A token that indicates the location of the next repository sync
-- definition in the array of repository sync definitions, after the
-- current requested list of repository sync definitions.
--
-- 'httpStatus', 'listRepositorySyncDefinitionsResponse_httpStatus' - The response's http status code.
--
-- 'syncDefinitions', 'listRepositorySyncDefinitionsResponse_syncDefinitions' - An array of repository sync definitions.
newListRepositorySyncDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRepositorySyncDefinitionsResponse
newListRepositorySyncDefinitionsResponse pHttpStatus_ =
  ListRepositorySyncDefinitionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      syncDefinitions = Prelude.mempty
    }

-- | A token that indicates the location of the next repository sync
-- definition in the array of repository sync definitions, after the
-- current requested list of repository sync definitions.
listRepositorySyncDefinitionsResponse_nextToken :: Lens.Lens' ListRepositorySyncDefinitionsResponse (Prelude.Maybe Prelude.Text)
listRepositorySyncDefinitionsResponse_nextToken = Lens.lens (\ListRepositorySyncDefinitionsResponse' {nextToken} -> nextToken) (\s@ListRepositorySyncDefinitionsResponse' {} a -> s {nextToken = a} :: ListRepositorySyncDefinitionsResponse)

-- | The response's http status code.
listRepositorySyncDefinitionsResponse_httpStatus :: Lens.Lens' ListRepositorySyncDefinitionsResponse Prelude.Int
listRepositorySyncDefinitionsResponse_httpStatus = Lens.lens (\ListRepositorySyncDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListRepositorySyncDefinitionsResponse' {} a -> s {httpStatus = a} :: ListRepositorySyncDefinitionsResponse)

-- | An array of repository sync definitions.
listRepositorySyncDefinitionsResponse_syncDefinitions :: Lens.Lens' ListRepositorySyncDefinitionsResponse [RepositorySyncDefinition]
listRepositorySyncDefinitionsResponse_syncDefinitions = Lens.lens (\ListRepositorySyncDefinitionsResponse' {syncDefinitions} -> syncDefinitions) (\s@ListRepositorySyncDefinitionsResponse' {} a -> s {syncDefinitions = a} :: ListRepositorySyncDefinitionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListRepositorySyncDefinitionsResponse
  where
  rnf ListRepositorySyncDefinitionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf syncDefinitions
