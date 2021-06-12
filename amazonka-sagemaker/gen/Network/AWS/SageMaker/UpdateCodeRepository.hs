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
-- Module      : Network.AWS.SageMaker.UpdateCodeRepository
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified Git repository with the specified values.
module Network.AWS.SageMaker.UpdateCodeRepository
  ( -- * Creating a Request
    UpdateCodeRepository (..),
    newUpdateCodeRepository,

    -- * Request Lenses
    updateCodeRepository_gitConfig,
    updateCodeRepository_codeRepositoryName,

    -- * Destructuring the Response
    UpdateCodeRepositoryResponse (..),
    newUpdateCodeRepositoryResponse,

    -- * Response Lenses
    updateCodeRepositoryResponse_httpStatus,
    updateCodeRepositoryResponse_codeRepositoryArn,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newUpdateCodeRepository' smart constructor.
data UpdateCodeRepository = UpdateCodeRepository'
  { -- | The configuration of the git repository, including the URL and the
    -- Amazon Resource Name (ARN) of the AWS Secrets Manager secret that
    -- contains the credentials used to access the repository. The secret must
    -- have a staging label of @AWSCURRENT@ and must be in the following
    -- format:
    --
    -- @{\"username\": UserName, \"password\": Password}@
    gitConfig :: Core.Maybe GitConfigForUpdate,
    -- | The name of the Git repository to update.
    codeRepositoryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCodeRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gitConfig', 'updateCodeRepository_gitConfig' - The configuration of the git repository, including the URL and the
-- Amazon Resource Name (ARN) of the AWS Secrets Manager secret that
-- contains the credentials used to access the repository. The secret must
-- have a staging label of @AWSCURRENT@ and must be in the following
-- format:
--
-- @{\"username\": UserName, \"password\": Password}@
--
-- 'codeRepositoryName', 'updateCodeRepository_codeRepositoryName' - The name of the Git repository to update.
newUpdateCodeRepository ::
  -- | 'codeRepositoryName'
  Core.Text ->
  UpdateCodeRepository
newUpdateCodeRepository pCodeRepositoryName_ =
  UpdateCodeRepository'
    { gitConfig = Core.Nothing,
      codeRepositoryName = pCodeRepositoryName_
    }

-- | The configuration of the git repository, including the URL and the
-- Amazon Resource Name (ARN) of the AWS Secrets Manager secret that
-- contains the credentials used to access the repository. The secret must
-- have a staging label of @AWSCURRENT@ and must be in the following
-- format:
--
-- @{\"username\": UserName, \"password\": Password}@
updateCodeRepository_gitConfig :: Lens.Lens' UpdateCodeRepository (Core.Maybe GitConfigForUpdate)
updateCodeRepository_gitConfig = Lens.lens (\UpdateCodeRepository' {gitConfig} -> gitConfig) (\s@UpdateCodeRepository' {} a -> s {gitConfig = a} :: UpdateCodeRepository)

-- | The name of the Git repository to update.
updateCodeRepository_codeRepositoryName :: Lens.Lens' UpdateCodeRepository Core.Text
updateCodeRepository_codeRepositoryName = Lens.lens (\UpdateCodeRepository' {codeRepositoryName} -> codeRepositoryName) (\s@UpdateCodeRepository' {} a -> s {codeRepositoryName = a} :: UpdateCodeRepository)

instance Core.AWSRequest UpdateCodeRepository where
  type
    AWSResponse UpdateCodeRepository =
      UpdateCodeRepositoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateCodeRepositoryResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "CodeRepositoryArn")
      )

instance Core.Hashable UpdateCodeRepository

instance Core.NFData UpdateCodeRepository

instance Core.ToHeaders UpdateCodeRepository where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.UpdateCodeRepository" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateCodeRepository where
  toJSON UpdateCodeRepository' {..} =
    Core.object
      ( Core.catMaybes
          [ ("GitConfig" Core..=) Core.<$> gitConfig,
            Core.Just
              ("CodeRepositoryName" Core..= codeRepositoryName)
          ]
      )

instance Core.ToPath UpdateCodeRepository where
  toPath = Core.const "/"

instance Core.ToQuery UpdateCodeRepository where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateCodeRepositoryResponse' smart constructor.
data UpdateCodeRepositoryResponse = UpdateCodeRepositoryResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The ARN of the Git repository.
    codeRepositoryArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateCodeRepositoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateCodeRepositoryResponse_httpStatus' - The response's http status code.
--
-- 'codeRepositoryArn', 'updateCodeRepositoryResponse_codeRepositoryArn' - The ARN of the Git repository.
newUpdateCodeRepositoryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'codeRepositoryArn'
  Core.Text ->
  UpdateCodeRepositoryResponse
newUpdateCodeRepositoryResponse
  pHttpStatus_
  pCodeRepositoryArn_ =
    UpdateCodeRepositoryResponse'
      { httpStatus =
          pHttpStatus_,
        codeRepositoryArn = pCodeRepositoryArn_
      }

-- | The response's http status code.
updateCodeRepositoryResponse_httpStatus :: Lens.Lens' UpdateCodeRepositoryResponse Core.Int
updateCodeRepositoryResponse_httpStatus = Lens.lens (\UpdateCodeRepositoryResponse' {httpStatus} -> httpStatus) (\s@UpdateCodeRepositoryResponse' {} a -> s {httpStatus = a} :: UpdateCodeRepositoryResponse)

-- | The ARN of the Git repository.
updateCodeRepositoryResponse_codeRepositoryArn :: Lens.Lens' UpdateCodeRepositoryResponse Core.Text
updateCodeRepositoryResponse_codeRepositoryArn = Lens.lens (\UpdateCodeRepositoryResponse' {codeRepositoryArn} -> codeRepositoryArn) (\s@UpdateCodeRepositoryResponse' {} a -> s {codeRepositoryArn = a} :: UpdateCodeRepositoryResponse)

instance Core.NFData UpdateCodeRepositoryResponse
