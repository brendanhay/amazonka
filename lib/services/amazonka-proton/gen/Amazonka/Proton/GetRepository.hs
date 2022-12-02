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
-- Module      : Amazonka.Proton.GetRepository
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get detail data for a linked repository.
module Amazonka.Proton.GetRepository
  ( -- * Creating a Request
    GetRepository (..),
    newGetRepository,

    -- * Request Lenses
    getRepository_name,
    getRepository_provider,

    -- * Destructuring the Response
    GetRepositoryResponse (..),
    newGetRepositoryResponse,

    -- * Response Lenses
    getRepositoryResponse_httpStatus,
    getRepositoryResponse_repository,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetRepository' smart constructor.
data GetRepository = GetRepository'
  { -- | The repository name, for example @myrepos\/myrepo@.
    name :: Prelude.Text,
    -- | The repository provider.
    provider :: RepositoryProvider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getRepository_name' - The repository name, for example @myrepos\/myrepo@.
--
-- 'provider', 'getRepository_provider' - The repository provider.
newGetRepository ::
  -- | 'name'
  Prelude.Text ->
  -- | 'provider'
  RepositoryProvider ->
  GetRepository
newGetRepository pName_ pProvider_ =
  GetRepository'
    { name = pName_,
      provider = pProvider_
    }

-- | The repository name, for example @myrepos\/myrepo@.
getRepository_name :: Lens.Lens' GetRepository Prelude.Text
getRepository_name = Lens.lens (\GetRepository' {name} -> name) (\s@GetRepository' {} a -> s {name = a} :: GetRepository)

-- | The repository provider.
getRepository_provider :: Lens.Lens' GetRepository RepositoryProvider
getRepository_provider = Lens.lens (\GetRepository' {provider} -> provider) (\s@GetRepository' {} a -> s {provider = a} :: GetRepository)

instance Core.AWSRequest GetRepository where
  type
    AWSResponse GetRepository =
      GetRepositoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRepositoryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "repository")
      )

instance Prelude.Hashable GetRepository where
  hashWithSalt _salt GetRepository' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` provider

instance Prelude.NFData GetRepository where
  rnf GetRepository' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf provider

instance Data.ToHeaders GetRepository where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.GetRepository" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRepository where
  toJSON GetRepository' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("name" Data..= name),
            Prelude.Just ("provider" Data..= provider)
          ]
      )

instance Data.ToPath GetRepository where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRepository where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRepositoryResponse' smart constructor.
data GetRepositoryResponse = GetRepositoryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The repository link\'s detail data that\'s returned by Proton.
    repository :: Repository
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRepositoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getRepositoryResponse_httpStatus' - The response's http status code.
--
-- 'repository', 'getRepositoryResponse_repository' - The repository link\'s detail data that\'s returned by Proton.
newGetRepositoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'repository'
  Repository ->
  GetRepositoryResponse
newGetRepositoryResponse pHttpStatus_ pRepository_ =
  GetRepositoryResponse'
    { httpStatus = pHttpStatus_,
      repository = pRepository_
    }

-- | The response's http status code.
getRepositoryResponse_httpStatus :: Lens.Lens' GetRepositoryResponse Prelude.Int
getRepositoryResponse_httpStatus = Lens.lens (\GetRepositoryResponse' {httpStatus} -> httpStatus) (\s@GetRepositoryResponse' {} a -> s {httpStatus = a} :: GetRepositoryResponse)

-- | The repository link\'s detail data that\'s returned by Proton.
getRepositoryResponse_repository :: Lens.Lens' GetRepositoryResponse Repository
getRepositoryResponse_repository = Lens.lens (\GetRepositoryResponse' {repository} -> repository) (\s@GetRepositoryResponse' {} a -> s {repository = a} :: GetRepositoryResponse)

instance Prelude.NFData GetRepositoryResponse where
  rnf GetRepositoryResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf repository
