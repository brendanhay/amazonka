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
-- Module      : Amazonka.Proton.CreateRepository
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create and register a link to a repository. Proton uses the link to
-- repeatedly access the repository, to either push to it (self-managed
-- provisioning) or pull from it (template sync). You can share a linked
-- repository across multiple resources (like environments using
-- self-managed provisioning, or synced templates). When you create a
-- repository link, Proton creates a
-- <https://docs.aws.amazon.com/proton/latest/userguide/using-service-linked-roles.html service-linked role>
-- for you.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-works-prov-methods.html#ag-works-prov-methods-self Self-managed provisioning>,
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-template-authoring.html#ag-template-bundles Template bundles>,
-- and
-- <https://docs.aws.amazon.com/proton/latest/userguide/ag-template-sync-configs.html Template sync configurations>
-- in the /Proton User Guide/.
module Amazonka.Proton.CreateRepository
  ( -- * Creating a Request
    CreateRepository (..),
    newCreateRepository,

    -- * Request Lenses
    createRepository_encryptionKey,
    createRepository_tags,
    createRepository_connectionArn,
    createRepository_name,
    createRepository_provider,

    -- * Destructuring the Response
    CreateRepositoryResponse (..),
    newCreateRepositoryResponse,

    -- * Response Lenses
    createRepositoryResponse_httpStatus,
    createRepositoryResponse_repository,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRepository' smart constructor.
data CreateRepository = CreateRepository'
  { -- | The ARN of your customer Amazon Web Services Key Management Service
    -- (Amazon Web Services KMS) key.
    encryptionKey :: Prelude.Maybe Prelude.Text,
    -- | An optional list of metadata items that you can associate with the
    -- Proton repository. A tag is a key-value pair.
    --
    -- For more information, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
    -- in the /Proton User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | The Amazon Resource Name (ARN) of your AWS CodeStar connection that
    -- connects Proton to your repository provider account. For more
    -- information, see
    -- <https://docs.aws.amazon.com/proton/latest/userguide/setting-up-for-service.html Setting up for Proton>
    -- in the /Proton User Guide/.
    connectionArn :: Prelude.Text,
    -- | The repository name (for example, @myrepos\/myrepo@).
    name :: Prelude.Text,
    -- | The repository provider.
    provider :: RepositoryProvider
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionKey', 'createRepository_encryptionKey' - The ARN of your customer Amazon Web Services Key Management Service
-- (Amazon Web Services KMS) key.
--
-- 'tags', 'createRepository_tags' - An optional list of metadata items that you can associate with the
-- Proton repository. A tag is a key-value pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
-- in the /Proton User Guide/.
--
-- 'connectionArn', 'createRepository_connectionArn' - The Amazon Resource Name (ARN) of your AWS CodeStar connection that
-- connects Proton to your repository provider account. For more
-- information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/setting-up-for-service.html Setting up for Proton>
-- in the /Proton User Guide/.
--
-- 'name', 'createRepository_name' - The repository name (for example, @myrepos\/myrepo@).
--
-- 'provider', 'createRepository_provider' - The repository provider.
newCreateRepository ::
  -- | 'connectionArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'provider'
  RepositoryProvider ->
  CreateRepository
newCreateRepository pConnectionArn_ pName_ pProvider_ =
  CreateRepository'
    { encryptionKey = Prelude.Nothing,
      tags = Prelude.Nothing,
      connectionArn = pConnectionArn_,
      name = pName_,
      provider = pProvider_
    }

-- | The ARN of your customer Amazon Web Services Key Management Service
-- (Amazon Web Services KMS) key.
createRepository_encryptionKey :: Lens.Lens' CreateRepository (Prelude.Maybe Prelude.Text)
createRepository_encryptionKey = Lens.lens (\CreateRepository' {encryptionKey} -> encryptionKey) (\s@CreateRepository' {} a -> s {encryptionKey = a} :: CreateRepository)

-- | An optional list of metadata items that you can associate with the
-- Proton repository. A tag is a key-value pair.
--
-- For more information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/resources.html Proton resources and tagging>
-- in the /Proton User Guide/.
createRepository_tags :: Lens.Lens' CreateRepository (Prelude.Maybe [Tag])
createRepository_tags = Lens.lens (\CreateRepository' {tags} -> tags) (\s@CreateRepository' {} a -> s {tags = a} :: CreateRepository) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of your AWS CodeStar connection that
-- connects Proton to your repository provider account. For more
-- information, see
-- <https://docs.aws.amazon.com/proton/latest/userguide/setting-up-for-service.html Setting up for Proton>
-- in the /Proton User Guide/.
createRepository_connectionArn :: Lens.Lens' CreateRepository Prelude.Text
createRepository_connectionArn = Lens.lens (\CreateRepository' {connectionArn} -> connectionArn) (\s@CreateRepository' {} a -> s {connectionArn = a} :: CreateRepository)

-- | The repository name (for example, @myrepos\/myrepo@).
createRepository_name :: Lens.Lens' CreateRepository Prelude.Text
createRepository_name = Lens.lens (\CreateRepository' {name} -> name) (\s@CreateRepository' {} a -> s {name = a} :: CreateRepository)

-- | The repository provider.
createRepository_provider :: Lens.Lens' CreateRepository RepositoryProvider
createRepository_provider = Lens.lens (\CreateRepository' {provider} -> provider) (\s@CreateRepository' {} a -> s {provider = a} :: CreateRepository)

instance Core.AWSRequest CreateRepository where
  type
    AWSResponse CreateRepository =
      CreateRepositoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRepositoryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "repository")
      )

instance Prelude.Hashable CreateRepository where
  hashWithSalt _salt CreateRepository' {..} =
    _salt
      `Prelude.hashWithSalt` encryptionKey
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` connectionArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` provider

instance Prelude.NFData CreateRepository where
  rnf CreateRepository' {..} =
    Prelude.rnf encryptionKey
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf connectionArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf provider

instance Data.ToHeaders CreateRepository where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AwsProton20200720.CreateRepository" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRepository where
  toJSON CreateRepository' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("encryptionKey" Data..=) Prelude.<$> encryptionKey,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("connectionArn" Data..= connectionArn),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("provider" Data..= provider)
          ]
      )

instance Data.ToPath CreateRepository where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateRepository where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRepositoryResponse' smart constructor.
data CreateRepositoryResponse = CreateRepositoryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The repository link\'s detail data that\'s returned by Proton.
    repository :: Repository
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRepositoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createRepositoryResponse_httpStatus' - The response's http status code.
--
-- 'repository', 'createRepositoryResponse_repository' - The repository link\'s detail data that\'s returned by Proton.
newCreateRepositoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'repository'
  Repository ->
  CreateRepositoryResponse
newCreateRepositoryResponse pHttpStatus_ pRepository_ =
  CreateRepositoryResponse'
    { httpStatus =
        pHttpStatus_,
      repository = pRepository_
    }

-- | The response's http status code.
createRepositoryResponse_httpStatus :: Lens.Lens' CreateRepositoryResponse Prelude.Int
createRepositoryResponse_httpStatus = Lens.lens (\CreateRepositoryResponse' {httpStatus} -> httpStatus) (\s@CreateRepositoryResponse' {} a -> s {httpStatus = a} :: CreateRepositoryResponse)

-- | The repository link\'s detail data that\'s returned by Proton.
createRepositoryResponse_repository :: Lens.Lens' CreateRepositoryResponse Repository
createRepositoryResponse_repository = Lens.lens (\CreateRepositoryResponse' {repository} -> repository) (\s@CreateRepositoryResponse' {} a -> s {repository = a} :: CreateRepositoryResponse)

instance Prelude.NFData CreateRepositoryResponse where
  rnf CreateRepositoryResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf repository
