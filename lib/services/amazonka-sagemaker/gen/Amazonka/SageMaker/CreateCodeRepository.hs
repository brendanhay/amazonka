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
-- Module      : Amazonka.SageMaker.CreateCodeRepository
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a Git repository as a resource in your SageMaker account. You
-- can associate the repository with notebook instances so that you can use
-- Git source control for the notebooks you create. The Git repository is a
-- resource in your SageMaker account, so it can be associated with more
-- than one notebook instance, and it persists independently from the
-- lifecycle of any notebook instances it is associated with.
--
-- The repository can be hosted either in
-- <https://docs.aws.amazon.com/codecommit/latest/userguide/welcome.html Amazon Web Services CodeCommit>
-- or in any other Git repository.
module Amazonka.SageMaker.CreateCodeRepository
  ( -- * Creating a Request
    CreateCodeRepository (..),
    newCreateCodeRepository,

    -- * Request Lenses
    createCodeRepository_tags,
    createCodeRepository_codeRepositoryName,
    createCodeRepository_gitConfig,

    -- * Destructuring the Response
    CreateCodeRepositoryResponse (..),
    newCreateCodeRepositoryResponse,

    -- * Response Lenses
    createCodeRepositoryResponse_httpStatus,
    createCodeRepositoryResponse_codeRepositoryArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newCreateCodeRepository' smart constructor.
data CreateCodeRepository = CreateCodeRepository'
  { -- | An array of key-value pairs. You can use tags to categorize your Amazon
    -- Web Services resources in different ways, for example, by purpose,
    -- owner, or environment. For more information, see
    -- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
    tags :: Prelude.Maybe [Tag],
    -- | The name of the Git repository. The name must have 1 to 63 characters.
    -- Valid characters are a-z, A-Z, 0-9, and - (hyphen).
    codeRepositoryName :: Prelude.Text,
    -- | Specifies details about the repository, including the URL where the
    -- repository is located, the default branch, and credentials to use to
    -- access the repository.
    gitConfig :: GitConfig
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCodeRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createCodeRepository_tags' - An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
--
-- 'codeRepositoryName', 'createCodeRepository_codeRepositoryName' - The name of the Git repository. The name must have 1 to 63 characters.
-- Valid characters are a-z, A-Z, 0-9, and - (hyphen).
--
-- 'gitConfig', 'createCodeRepository_gitConfig' - Specifies details about the repository, including the URL where the
-- repository is located, the default branch, and credentials to use to
-- access the repository.
newCreateCodeRepository ::
  -- | 'codeRepositoryName'
  Prelude.Text ->
  -- | 'gitConfig'
  GitConfig ->
  CreateCodeRepository
newCreateCodeRepository
  pCodeRepositoryName_
  pGitConfig_ =
    CreateCodeRepository'
      { tags = Prelude.Nothing,
        codeRepositoryName = pCodeRepositoryName_,
        gitConfig = pGitConfig_
      }

-- | An array of key-value pairs. You can use tags to categorize your Amazon
-- Web Services resources in different ways, for example, by purpose,
-- owner, or environment. For more information, see
-- <https://docs.aws.amazon.com/general/latest/gr/aws_tagging.html Tagging Amazon Web Services Resources>.
createCodeRepository_tags :: Lens.Lens' CreateCodeRepository (Prelude.Maybe [Tag])
createCodeRepository_tags = Lens.lens (\CreateCodeRepository' {tags} -> tags) (\s@CreateCodeRepository' {} a -> s {tags = a} :: CreateCodeRepository) Prelude.. Lens.mapping Lens.coerced

-- | The name of the Git repository. The name must have 1 to 63 characters.
-- Valid characters are a-z, A-Z, 0-9, and - (hyphen).
createCodeRepository_codeRepositoryName :: Lens.Lens' CreateCodeRepository Prelude.Text
createCodeRepository_codeRepositoryName = Lens.lens (\CreateCodeRepository' {codeRepositoryName} -> codeRepositoryName) (\s@CreateCodeRepository' {} a -> s {codeRepositoryName = a} :: CreateCodeRepository)

-- | Specifies details about the repository, including the URL where the
-- repository is located, the default branch, and credentials to use to
-- access the repository.
createCodeRepository_gitConfig :: Lens.Lens' CreateCodeRepository GitConfig
createCodeRepository_gitConfig = Lens.lens (\CreateCodeRepository' {gitConfig} -> gitConfig) (\s@CreateCodeRepository' {} a -> s {gitConfig = a} :: CreateCodeRepository)

instance Core.AWSRequest CreateCodeRepository where
  type
    AWSResponse CreateCodeRepository =
      CreateCodeRepositoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateCodeRepositoryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CodeRepositoryArn")
      )

instance Prelude.Hashable CreateCodeRepository where
  hashWithSalt _salt CreateCodeRepository' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` codeRepositoryName
      `Prelude.hashWithSalt` gitConfig

instance Prelude.NFData CreateCodeRepository where
  rnf CreateCodeRepository' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf codeRepositoryName
      `Prelude.seq` Prelude.rnf gitConfig

instance Data.ToHeaders CreateCodeRepository where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.CreateCodeRepository" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateCodeRepository where
  toJSON CreateCodeRepository' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("CodeRepositoryName" Data..= codeRepositoryName),
            Prelude.Just ("GitConfig" Data..= gitConfig)
          ]
      )

instance Data.ToPath CreateCodeRepository where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateCodeRepository where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateCodeRepositoryResponse' smart constructor.
data CreateCodeRepositoryResponse = CreateCodeRepositoryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the new repository.
    codeRepositoryArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateCodeRepositoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createCodeRepositoryResponse_httpStatus' - The response's http status code.
--
-- 'codeRepositoryArn', 'createCodeRepositoryResponse_codeRepositoryArn' - The Amazon Resource Name (ARN) of the new repository.
newCreateCodeRepositoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'codeRepositoryArn'
  Prelude.Text ->
  CreateCodeRepositoryResponse
newCreateCodeRepositoryResponse
  pHttpStatus_
  pCodeRepositoryArn_ =
    CreateCodeRepositoryResponse'
      { httpStatus =
          pHttpStatus_,
        codeRepositoryArn = pCodeRepositoryArn_
      }

-- | The response's http status code.
createCodeRepositoryResponse_httpStatus :: Lens.Lens' CreateCodeRepositoryResponse Prelude.Int
createCodeRepositoryResponse_httpStatus = Lens.lens (\CreateCodeRepositoryResponse' {httpStatus} -> httpStatus) (\s@CreateCodeRepositoryResponse' {} a -> s {httpStatus = a} :: CreateCodeRepositoryResponse)

-- | The Amazon Resource Name (ARN) of the new repository.
createCodeRepositoryResponse_codeRepositoryArn :: Lens.Lens' CreateCodeRepositoryResponse Prelude.Text
createCodeRepositoryResponse_codeRepositoryArn = Lens.lens (\CreateCodeRepositoryResponse' {codeRepositoryArn} -> codeRepositoryArn) (\s@CreateCodeRepositoryResponse' {} a -> s {codeRepositoryArn = a} :: CreateCodeRepositoryResponse)

instance Prelude.NFData CreateCodeRepositoryResponse where
  rnf CreateCodeRepositoryResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf codeRepositoryArn
