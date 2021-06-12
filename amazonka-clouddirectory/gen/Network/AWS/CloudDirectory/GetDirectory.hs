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
-- Module      : Network.AWS.CloudDirectory.GetDirectory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves metadata about a directory.
module Network.AWS.CloudDirectory.GetDirectory
  ( -- * Creating a Request
    GetDirectory (..),
    newGetDirectory,

    -- * Request Lenses
    getDirectory_directoryArn,

    -- * Destructuring the Response
    GetDirectoryResponse (..),
    newGetDirectoryResponse,

    -- * Response Lenses
    getDirectoryResponse_httpStatus,
    getDirectoryResponse_directory,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDirectory' smart constructor.
data GetDirectory = GetDirectory'
  { -- | The ARN of the directory.
    directoryArn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryArn', 'getDirectory_directoryArn' - The ARN of the directory.
newGetDirectory ::
  -- | 'directoryArn'
  Core.Text ->
  GetDirectory
newGetDirectory pDirectoryArn_ =
  GetDirectory' {directoryArn = pDirectoryArn_}

-- | The ARN of the directory.
getDirectory_directoryArn :: Lens.Lens' GetDirectory Core.Text
getDirectory_directoryArn = Lens.lens (\GetDirectory' {directoryArn} -> directoryArn) (\s@GetDirectory' {} a -> s {directoryArn = a} :: GetDirectory)

instance Core.AWSRequest GetDirectory where
  type AWSResponse GetDirectory = GetDirectoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDirectoryResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..:> "Directory")
      )

instance Core.Hashable GetDirectory

instance Core.NFData GetDirectory

instance Core.ToHeaders GetDirectory where
  toHeaders GetDirectory' {..} =
    Core.mconcat
      ["x-amz-data-partition" Core.=# directoryArn]

instance Core.ToJSON GetDirectory where
  toJSON = Core.const (Core.Object Core.mempty)

instance Core.ToPath GetDirectory where
  toPath =
    Core.const
      "/amazonclouddirectory/2017-01-11/directory/get"

instance Core.ToQuery GetDirectory where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDirectoryResponse' smart constructor.
data GetDirectoryResponse = GetDirectoryResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Metadata about the directory.
    directory :: Directory
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDirectoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getDirectoryResponse_httpStatus' - The response's http status code.
--
-- 'directory', 'getDirectoryResponse_directory' - Metadata about the directory.
newGetDirectoryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  -- | 'directory'
  Directory ->
  GetDirectoryResponse
newGetDirectoryResponse pHttpStatus_ pDirectory_ =
  GetDirectoryResponse'
    { httpStatus = pHttpStatus_,
      directory = pDirectory_
    }

-- | The response's http status code.
getDirectoryResponse_httpStatus :: Lens.Lens' GetDirectoryResponse Core.Int
getDirectoryResponse_httpStatus = Lens.lens (\GetDirectoryResponse' {httpStatus} -> httpStatus) (\s@GetDirectoryResponse' {} a -> s {httpStatus = a} :: GetDirectoryResponse)

-- | Metadata about the directory.
getDirectoryResponse_directory :: Lens.Lens' GetDirectoryResponse Directory
getDirectoryResponse_directory = Lens.lens (\GetDirectoryResponse' {directory} -> directory) (\s@GetDirectoryResponse' {} a -> s {directory = a} :: GetDirectoryResponse)

instance Core.NFData GetDirectoryResponse
