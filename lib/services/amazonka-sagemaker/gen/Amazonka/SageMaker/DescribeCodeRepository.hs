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
-- Module      : Amazonka.SageMaker.DescribeCodeRepository
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about the specified Git repository.
module Amazonka.SageMaker.DescribeCodeRepository
  ( -- * Creating a Request
    DescribeCodeRepository (..),
    newDescribeCodeRepository,

    -- * Request Lenses
    describeCodeRepository_codeRepositoryName,

    -- * Destructuring the Response
    DescribeCodeRepositoryResponse (..),
    newDescribeCodeRepositoryResponse,

    -- * Response Lenses
    describeCodeRepositoryResponse_gitConfig,
    describeCodeRepositoryResponse_httpStatus,
    describeCodeRepositoryResponse_codeRepositoryName,
    describeCodeRepositoryResponse_codeRepositoryArn,
    describeCodeRepositoryResponse_creationTime,
    describeCodeRepositoryResponse_lastModifiedTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newDescribeCodeRepository' smart constructor.
data DescribeCodeRepository = DescribeCodeRepository'
  { -- | The name of the Git repository to describe.
    codeRepositoryName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCodeRepository' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'codeRepositoryName', 'describeCodeRepository_codeRepositoryName' - The name of the Git repository to describe.
newDescribeCodeRepository ::
  -- | 'codeRepositoryName'
  Prelude.Text ->
  DescribeCodeRepository
newDescribeCodeRepository pCodeRepositoryName_ =
  DescribeCodeRepository'
    { codeRepositoryName =
        pCodeRepositoryName_
    }

-- | The name of the Git repository to describe.
describeCodeRepository_codeRepositoryName :: Lens.Lens' DescribeCodeRepository Prelude.Text
describeCodeRepository_codeRepositoryName = Lens.lens (\DescribeCodeRepository' {codeRepositoryName} -> codeRepositoryName) (\s@DescribeCodeRepository' {} a -> s {codeRepositoryName = a} :: DescribeCodeRepository)

instance Core.AWSRequest DescribeCodeRepository where
  type
    AWSResponse DescribeCodeRepository =
      DescribeCodeRepositoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCodeRepositoryResponse'
            Prelude.<$> (x Core..?> "GitConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "CodeRepositoryName")
            Prelude.<*> (x Core..:> "CodeRepositoryArn")
            Prelude.<*> (x Core..:> "CreationTime")
            Prelude.<*> (x Core..:> "LastModifiedTime")
      )

instance Prelude.Hashable DescribeCodeRepository where
  hashWithSalt _salt DescribeCodeRepository' {..} =
    _salt `Prelude.hashWithSalt` codeRepositoryName

instance Prelude.NFData DescribeCodeRepository where
  rnf DescribeCodeRepository' {..} =
    Prelude.rnf codeRepositoryName

instance Core.ToHeaders DescribeCodeRepository where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeCodeRepository" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeCodeRepository where
  toJSON DescribeCodeRepository' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("CodeRepositoryName" Core..= codeRepositoryName)
          ]
      )

instance Core.ToPath DescribeCodeRepository where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeCodeRepository where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeCodeRepositoryResponse' smart constructor.
data DescribeCodeRepositoryResponse = DescribeCodeRepositoryResponse'
  { -- | Configuration details about the repository, including the URL where the
    -- repository is located, the default branch, and the Amazon Resource Name
    -- (ARN) of the Amazon Web Services Secrets Manager secret that contains
    -- the credentials used to access the repository.
    gitConfig :: Prelude.Maybe GitConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the Git repository.
    codeRepositoryName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Git repository.
    codeRepositoryArn :: Prelude.Text,
    -- | The date and time that the repository was created.
    creationTime :: Core.POSIX,
    -- | The date and time that the repository was last changed.
    lastModifiedTime :: Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeCodeRepositoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gitConfig', 'describeCodeRepositoryResponse_gitConfig' - Configuration details about the repository, including the URL where the
-- repository is located, the default branch, and the Amazon Resource Name
-- (ARN) of the Amazon Web Services Secrets Manager secret that contains
-- the credentials used to access the repository.
--
-- 'httpStatus', 'describeCodeRepositoryResponse_httpStatus' - The response's http status code.
--
-- 'codeRepositoryName', 'describeCodeRepositoryResponse_codeRepositoryName' - The name of the Git repository.
--
-- 'codeRepositoryArn', 'describeCodeRepositoryResponse_codeRepositoryArn' - The Amazon Resource Name (ARN) of the Git repository.
--
-- 'creationTime', 'describeCodeRepositoryResponse_creationTime' - The date and time that the repository was created.
--
-- 'lastModifiedTime', 'describeCodeRepositoryResponse_lastModifiedTime' - The date and time that the repository was last changed.
newDescribeCodeRepositoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'codeRepositoryName'
  Prelude.Text ->
  -- | 'codeRepositoryArn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModifiedTime'
  Prelude.UTCTime ->
  DescribeCodeRepositoryResponse
newDescribeCodeRepositoryResponse
  pHttpStatus_
  pCodeRepositoryName_
  pCodeRepositoryArn_
  pCreationTime_
  pLastModifiedTime_ =
    DescribeCodeRepositoryResponse'
      { gitConfig =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        codeRepositoryName = pCodeRepositoryName_,
        codeRepositoryArn = pCodeRepositoryArn_,
        creationTime =
          Core._Time Lens.# pCreationTime_,
        lastModifiedTime =
          Core._Time Lens.# pLastModifiedTime_
      }

-- | Configuration details about the repository, including the URL where the
-- repository is located, the default branch, and the Amazon Resource Name
-- (ARN) of the Amazon Web Services Secrets Manager secret that contains
-- the credentials used to access the repository.
describeCodeRepositoryResponse_gitConfig :: Lens.Lens' DescribeCodeRepositoryResponse (Prelude.Maybe GitConfig)
describeCodeRepositoryResponse_gitConfig = Lens.lens (\DescribeCodeRepositoryResponse' {gitConfig} -> gitConfig) (\s@DescribeCodeRepositoryResponse' {} a -> s {gitConfig = a} :: DescribeCodeRepositoryResponse)

-- | The response's http status code.
describeCodeRepositoryResponse_httpStatus :: Lens.Lens' DescribeCodeRepositoryResponse Prelude.Int
describeCodeRepositoryResponse_httpStatus = Lens.lens (\DescribeCodeRepositoryResponse' {httpStatus} -> httpStatus) (\s@DescribeCodeRepositoryResponse' {} a -> s {httpStatus = a} :: DescribeCodeRepositoryResponse)

-- | The name of the Git repository.
describeCodeRepositoryResponse_codeRepositoryName :: Lens.Lens' DescribeCodeRepositoryResponse Prelude.Text
describeCodeRepositoryResponse_codeRepositoryName = Lens.lens (\DescribeCodeRepositoryResponse' {codeRepositoryName} -> codeRepositoryName) (\s@DescribeCodeRepositoryResponse' {} a -> s {codeRepositoryName = a} :: DescribeCodeRepositoryResponse)

-- | The Amazon Resource Name (ARN) of the Git repository.
describeCodeRepositoryResponse_codeRepositoryArn :: Lens.Lens' DescribeCodeRepositoryResponse Prelude.Text
describeCodeRepositoryResponse_codeRepositoryArn = Lens.lens (\DescribeCodeRepositoryResponse' {codeRepositoryArn} -> codeRepositoryArn) (\s@DescribeCodeRepositoryResponse' {} a -> s {codeRepositoryArn = a} :: DescribeCodeRepositoryResponse)

-- | The date and time that the repository was created.
describeCodeRepositoryResponse_creationTime :: Lens.Lens' DescribeCodeRepositoryResponse Prelude.UTCTime
describeCodeRepositoryResponse_creationTime = Lens.lens (\DescribeCodeRepositoryResponse' {creationTime} -> creationTime) (\s@DescribeCodeRepositoryResponse' {} a -> s {creationTime = a} :: DescribeCodeRepositoryResponse) Prelude.. Core._Time

-- | The date and time that the repository was last changed.
describeCodeRepositoryResponse_lastModifiedTime :: Lens.Lens' DescribeCodeRepositoryResponse Prelude.UTCTime
describeCodeRepositoryResponse_lastModifiedTime = Lens.lens (\DescribeCodeRepositoryResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeCodeRepositoryResponse' {} a -> s {lastModifiedTime = a} :: DescribeCodeRepositoryResponse) Prelude.. Core._Time

instance
  Prelude.NFData
    DescribeCodeRepositoryResponse
  where
  rnf DescribeCodeRepositoryResponse' {..} =
    Prelude.rnf gitConfig
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf codeRepositoryName
      `Prelude.seq` Prelude.rnf codeRepositoryArn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModifiedTime
