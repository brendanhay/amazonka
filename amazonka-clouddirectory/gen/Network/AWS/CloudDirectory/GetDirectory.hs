{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDirectory' smart constructor.
data GetDirectory = GetDirectory'
  { -- | The ARN of the directory.
    directoryArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  GetDirectory
newGetDirectory pDirectoryArn_ =
  GetDirectory' {directoryArn = pDirectoryArn_}

-- | The ARN of the directory.
getDirectory_directoryArn :: Lens.Lens' GetDirectory Prelude.Text
getDirectory_directoryArn = Lens.lens (\GetDirectory' {directoryArn} -> directoryArn) (\s@GetDirectory' {} a -> s {directoryArn = a} :: GetDirectory)

instance Prelude.AWSRequest GetDirectory where
  type Rs GetDirectory = GetDirectoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDirectoryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "Directory")
      )

instance Prelude.Hashable GetDirectory

instance Prelude.NFData GetDirectory

instance Prelude.ToHeaders GetDirectory where
  toHeaders GetDirectory' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Prelude.=# directoryArn]

instance Prelude.ToJSON GetDirectory where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath GetDirectory where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/directory/get"

instance Prelude.ToQuery GetDirectory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDirectoryResponse' smart constructor.
data GetDirectoryResponse = GetDirectoryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Metadata about the directory.
    directory :: Directory
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  -- | 'directory'
  Directory ->
  GetDirectoryResponse
newGetDirectoryResponse pHttpStatus_ pDirectory_ =
  GetDirectoryResponse'
    { httpStatus = pHttpStatus_,
      directory = pDirectory_
    }

-- | The response's http status code.
getDirectoryResponse_httpStatus :: Lens.Lens' GetDirectoryResponse Prelude.Int
getDirectoryResponse_httpStatus = Lens.lens (\GetDirectoryResponse' {httpStatus} -> httpStatus) (\s@GetDirectoryResponse' {} a -> s {httpStatus = a} :: GetDirectoryResponse)

-- | Metadata about the directory.
getDirectoryResponse_directory :: Lens.Lens' GetDirectoryResponse Directory
getDirectoryResponse_directory = Lens.lens (\GetDirectoryResponse' {directory} -> directory) (\s@GetDirectoryResponse' {} a -> s {directory = a} :: GetDirectoryResponse)

instance Prelude.NFData GetDirectoryResponse
