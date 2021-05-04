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
-- Module      : Network.AWS.CloudDirectory.EnableDirectory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Enables the specified directory. Only disabled directories can be
-- enabled. Once enabled, the directory can then be read and written to.
module Network.AWS.CloudDirectory.EnableDirectory
  ( -- * Creating a Request
    EnableDirectory (..),
    newEnableDirectory,

    -- * Request Lenses
    enableDirectory_directoryArn,

    -- * Destructuring the Response
    EnableDirectoryResponse (..),
    newEnableDirectoryResponse,

    -- * Response Lenses
    enableDirectoryResponse_httpStatus,
    enableDirectoryResponse_directoryArn,
  )
where

import Network.AWS.CloudDirectory.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newEnableDirectory' smart constructor.
data EnableDirectory = EnableDirectory'
  { -- | The ARN of the directory to enable.
    directoryArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryArn', 'enableDirectory_directoryArn' - The ARN of the directory to enable.
newEnableDirectory ::
  -- | 'directoryArn'
  Prelude.Text ->
  EnableDirectory
newEnableDirectory pDirectoryArn_ =
  EnableDirectory' {directoryArn = pDirectoryArn_}

-- | The ARN of the directory to enable.
enableDirectory_directoryArn :: Lens.Lens' EnableDirectory Prelude.Text
enableDirectory_directoryArn = Lens.lens (\EnableDirectory' {directoryArn} -> directoryArn) (\s@EnableDirectory' {} a -> s {directoryArn = a} :: EnableDirectory)

instance Prelude.AWSRequest EnableDirectory where
  type Rs EnableDirectory = EnableDirectoryResponse
  request = Request.putJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          EnableDirectoryResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Prelude..:> "DirectoryArn")
      )

instance Prelude.Hashable EnableDirectory

instance Prelude.NFData EnableDirectory

instance Prelude.ToHeaders EnableDirectory where
  toHeaders EnableDirectory' {..} =
    Prelude.mconcat
      ["x-amz-data-partition" Prelude.=# directoryArn]

instance Prelude.ToJSON EnableDirectory where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath EnableDirectory where
  toPath =
    Prelude.const
      "/amazonclouddirectory/2017-01-11/directory/enable"

instance Prelude.ToQuery EnableDirectory where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newEnableDirectoryResponse' smart constructor.
data EnableDirectoryResponse = EnableDirectoryResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the enabled directory.
    directoryArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'EnableDirectoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'enableDirectoryResponse_httpStatus' - The response's http status code.
--
-- 'directoryArn', 'enableDirectoryResponse_directoryArn' - The ARN of the enabled directory.
newEnableDirectoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'directoryArn'
  Prelude.Text ->
  EnableDirectoryResponse
newEnableDirectoryResponse
  pHttpStatus_
  pDirectoryArn_ =
    EnableDirectoryResponse'
      { httpStatus = pHttpStatus_,
        directoryArn = pDirectoryArn_
      }

-- | The response's http status code.
enableDirectoryResponse_httpStatus :: Lens.Lens' EnableDirectoryResponse Prelude.Int
enableDirectoryResponse_httpStatus = Lens.lens (\EnableDirectoryResponse' {httpStatus} -> httpStatus) (\s@EnableDirectoryResponse' {} a -> s {httpStatus = a} :: EnableDirectoryResponse)

-- | The ARN of the enabled directory.
enableDirectoryResponse_directoryArn :: Lens.Lens' EnableDirectoryResponse Prelude.Text
enableDirectoryResponse_directoryArn = Lens.lens (\EnableDirectoryResponse' {directoryArn} -> directoryArn) (\s@EnableDirectoryResponse' {} a -> s {directoryArn = a} :: EnableDirectoryResponse)

instance Prelude.NFData EnableDirectoryResponse
