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
-- Module      : Network.AWS.SageMaker.DescribeCodeRepository
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details about the specified Git repository.
module Network.AWS.SageMaker.DescribeCodeRepository
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newDescribeCodeRepository' smart constructor.
data DescribeCodeRepository = DescribeCodeRepository'
  { -- | The name of the Git repository to describe.
    codeRepositoryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeCodeRepository
newDescribeCodeRepository pCodeRepositoryName_ =
  DescribeCodeRepository'
    { codeRepositoryName =
        pCodeRepositoryName_
    }

-- | The name of the Git repository to describe.
describeCodeRepository_codeRepositoryName :: Lens.Lens' DescribeCodeRepository Core.Text
describeCodeRepository_codeRepositoryName = Lens.lens (\DescribeCodeRepository' {codeRepositoryName} -> codeRepositoryName) (\s@DescribeCodeRepository' {} a -> s {codeRepositoryName = a} :: DescribeCodeRepository)

instance Core.AWSRequest DescribeCodeRepository where
  type
    AWSResponse DescribeCodeRepository =
      DescribeCodeRepositoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeCodeRepositoryResponse'
            Core.<$> (x Core..?> "GitConfig")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "CodeRepositoryName")
            Core.<*> (x Core..:> "CodeRepositoryArn")
            Core.<*> (x Core..:> "CreationTime")
            Core.<*> (x Core..:> "LastModifiedTime")
      )

instance Core.Hashable DescribeCodeRepository

instance Core.NFData DescribeCodeRepository

instance Core.ToHeaders DescribeCodeRepository where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.DescribeCodeRepository" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeCodeRepository where
  toJSON DescribeCodeRepository' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("CodeRepositoryName" Core..= codeRepositoryName)
          ]
      )

instance Core.ToPath DescribeCodeRepository where
  toPath = Core.const "/"

instance Core.ToQuery DescribeCodeRepository where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeCodeRepositoryResponse' smart constructor.
data DescribeCodeRepositoryResponse = DescribeCodeRepositoryResponse'
  { -- | Configuration details about the repository, including the URL where the
    -- repository is located, the default branch, and the Amazon Resource Name
    -- (ARN) of the AWS Secrets Manager secret that contains the credentials
    -- used to access the repository.
    gitConfig :: Core.Maybe GitConfig,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | The name of the Git repository.
    codeRepositoryName :: Core.Text,
    -- | The Amazon Resource Name (ARN) of the Git repository.
    codeRepositoryArn :: Core.Text,
    -- | The date and time that the repository was created.
    creationTime :: Core.POSIX,
    -- | The date and time that the repository was last changed.
    lastModifiedTime :: Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- (ARN) of the AWS Secrets Manager secret that contains the credentials
-- used to access the repository.
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
  Core.Int ->
  -- | 'codeRepositoryName'
  Core.Text ->
  -- | 'codeRepositoryArn'
  Core.Text ->
  -- | 'creationTime'
  Core.UTCTime ->
  -- | 'lastModifiedTime'
  Core.UTCTime ->
  DescribeCodeRepositoryResponse
newDescribeCodeRepositoryResponse
  pHttpStatus_
  pCodeRepositoryName_
  pCodeRepositoryArn_
  pCreationTime_
  pLastModifiedTime_ =
    DescribeCodeRepositoryResponse'
      { gitConfig =
          Core.Nothing,
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
-- (ARN) of the AWS Secrets Manager secret that contains the credentials
-- used to access the repository.
describeCodeRepositoryResponse_gitConfig :: Lens.Lens' DescribeCodeRepositoryResponse (Core.Maybe GitConfig)
describeCodeRepositoryResponse_gitConfig = Lens.lens (\DescribeCodeRepositoryResponse' {gitConfig} -> gitConfig) (\s@DescribeCodeRepositoryResponse' {} a -> s {gitConfig = a} :: DescribeCodeRepositoryResponse)

-- | The response's http status code.
describeCodeRepositoryResponse_httpStatus :: Lens.Lens' DescribeCodeRepositoryResponse Core.Int
describeCodeRepositoryResponse_httpStatus = Lens.lens (\DescribeCodeRepositoryResponse' {httpStatus} -> httpStatus) (\s@DescribeCodeRepositoryResponse' {} a -> s {httpStatus = a} :: DescribeCodeRepositoryResponse)

-- | The name of the Git repository.
describeCodeRepositoryResponse_codeRepositoryName :: Lens.Lens' DescribeCodeRepositoryResponse Core.Text
describeCodeRepositoryResponse_codeRepositoryName = Lens.lens (\DescribeCodeRepositoryResponse' {codeRepositoryName} -> codeRepositoryName) (\s@DescribeCodeRepositoryResponse' {} a -> s {codeRepositoryName = a} :: DescribeCodeRepositoryResponse)

-- | The Amazon Resource Name (ARN) of the Git repository.
describeCodeRepositoryResponse_codeRepositoryArn :: Lens.Lens' DescribeCodeRepositoryResponse Core.Text
describeCodeRepositoryResponse_codeRepositoryArn = Lens.lens (\DescribeCodeRepositoryResponse' {codeRepositoryArn} -> codeRepositoryArn) (\s@DescribeCodeRepositoryResponse' {} a -> s {codeRepositoryArn = a} :: DescribeCodeRepositoryResponse)

-- | The date and time that the repository was created.
describeCodeRepositoryResponse_creationTime :: Lens.Lens' DescribeCodeRepositoryResponse Core.UTCTime
describeCodeRepositoryResponse_creationTime = Lens.lens (\DescribeCodeRepositoryResponse' {creationTime} -> creationTime) (\s@DescribeCodeRepositoryResponse' {} a -> s {creationTime = a} :: DescribeCodeRepositoryResponse) Core.. Core._Time

-- | The date and time that the repository was last changed.
describeCodeRepositoryResponse_lastModifiedTime :: Lens.Lens' DescribeCodeRepositoryResponse Core.UTCTime
describeCodeRepositoryResponse_lastModifiedTime = Lens.lens (\DescribeCodeRepositoryResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribeCodeRepositoryResponse' {} a -> s {lastModifiedTime = a} :: DescribeCodeRepositoryResponse) Core.. Core._Time

instance Core.NFData DescribeCodeRepositoryResponse
