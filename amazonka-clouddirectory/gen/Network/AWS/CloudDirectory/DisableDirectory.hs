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
-- Module      : Network.AWS.CloudDirectory.DisableDirectory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disables the specified directory. Disabled directories cannot be read or
-- written to. Only enabled directories can be disabled. Disabled
-- directories may be reenabled.
module Network.AWS.CloudDirectory.DisableDirectory
  ( -- * Creating a Request
    DisableDirectory (..),
    newDisableDirectory,

    -- * Request Lenses
    disableDirectory_directoryArn,

    -- * Destructuring the Response
    DisableDirectoryResponse (..),
    newDisableDirectoryResponse,

    -- * Response Lenses
    disableDirectoryResponse_httpStatus,
    disableDirectoryResponse_directoryArn,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDisableDirectory' smart constructor.
data DisableDirectory = DisableDirectory'
  { -- | The ARN of the directory to disable.
    directoryArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisableDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryArn', 'disableDirectory_directoryArn' - The ARN of the directory to disable.
newDisableDirectory ::
  -- | 'directoryArn'
  Prelude.Text ->
  DisableDirectory
newDisableDirectory pDirectoryArn_ =
  DisableDirectory' {directoryArn = pDirectoryArn_}

-- | The ARN of the directory to disable.
disableDirectory_directoryArn :: Lens.Lens' DisableDirectory Prelude.Text
disableDirectory_directoryArn = Lens.lens (\DisableDirectory' {directoryArn} -> directoryArn) (\s@DisableDirectory' {} a -> s {directoryArn = a} :: DisableDirectory)

instance Prelude.AWSRequest DisableDirectory where
  type Rs DisableDirectory = DisableDirectoryResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DisableDirectoryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "DirectoryArn")
      )

instance Prelude.Hashable DisableDirectory

instance Prelude.NFData DisableDirectory

instance Prelude.ToHeaders DisableDirectory where
  toHeaders DisableDirectory' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Prelude.=# directoryArn]

instance Prelude.ToJSON DisableDirectory where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath DisableDirectory where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/directory/disable"

instance Prelude.ToQuery DisableDirectory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisableDirectoryResponse' smart constructor.
data DisableDirectoryResponse = DisableDirectoryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the directory that has been disabled.
    directoryArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DisableDirectoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'disableDirectoryResponse_httpStatus' - The response's http status code.
--
-- 'directoryArn', 'disableDirectoryResponse_directoryArn' - The ARN of the directory that has been disabled.
newDisableDirectoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'directoryArn'
  Prelude.Text ->
  DisableDirectoryResponse
newDisableDirectoryResponse
  pHttpStatus_
  pDirectoryArn_ =
    DisableDirectoryResponse'
      { httpStatus =
          pHttpStatus_,
        directoryArn = pDirectoryArn_
      }

-- | The response's http status code.
disableDirectoryResponse_httpStatus :: Lens.Lens' DisableDirectoryResponse Prelude.Int
disableDirectoryResponse_httpStatus = Lens.lens (\DisableDirectoryResponse' {httpStatus} -> httpStatus) (\s@DisableDirectoryResponse' {} a -> s {httpStatus = a} :: DisableDirectoryResponse)

-- | The ARN of the directory that has been disabled.
disableDirectoryResponse_directoryArn :: Lens.Lens' DisableDirectoryResponse Prelude.Text
disableDirectoryResponse_directoryArn = Lens.lens (\DisableDirectoryResponse' {directoryArn} -> directoryArn) (\s@DisableDirectoryResponse' {} a -> s {directoryArn = a} :: DisableDirectoryResponse)

instance Prelude.NFData DisableDirectoryResponse
