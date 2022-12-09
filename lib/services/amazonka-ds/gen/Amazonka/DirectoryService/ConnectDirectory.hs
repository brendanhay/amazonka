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
-- Module      : Amazonka.DirectoryService.ConnectDirectory
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AD Connector to connect to a self-managed directory.
--
-- Before you call @ConnectDirectory@, ensure that all of the required
-- permissions have been explicitly granted through a policy. For details
-- about what permissions are required to run the @ConnectDirectory@
-- operation, see
-- <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html Directory Service API Permissions: Actions, Resources, and Conditions Reference>.
module Amazonka.DirectoryService.ConnectDirectory
  ( -- * Creating a Request
    ConnectDirectory (..),
    newConnectDirectory,

    -- * Request Lenses
    connectDirectory_description,
    connectDirectory_shortName,
    connectDirectory_tags,
    connectDirectory_name,
    connectDirectory_password,
    connectDirectory_size,
    connectDirectory_connectSettings,

    -- * Destructuring the Response
    ConnectDirectoryResponse (..),
    newConnectDirectoryResponse,

    -- * Response Lenses
    connectDirectoryResponse_directoryId,
    connectDirectoryResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DirectoryService.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Contains the inputs for the ConnectDirectory operation.
--
-- /See:/ 'newConnectDirectory' smart constructor.
data ConnectDirectory = ConnectDirectory'
  { -- | A description for the directory.
    description :: Prelude.Maybe Prelude.Text,
    -- | The NetBIOS name of your self-managed directory, such as @CORP@.
    shortName :: Prelude.Maybe Prelude.Text,
    -- | The tags to be assigned to AD Connector.
    tags :: Prelude.Maybe [Tag],
    -- | The fully qualified name of your self-managed directory, such as
    -- @corp.example.com@.
    name :: Prelude.Text,
    -- | The password for your self-managed user account.
    password :: Data.Sensitive Prelude.Text,
    -- | The size of the directory.
    size :: DirectorySize,
    -- | A DirectoryConnectSettings object that contains additional information
    -- for the operation.
    connectSettings :: DirectoryConnectSettings
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'connectDirectory_description' - A description for the directory.
--
-- 'shortName', 'connectDirectory_shortName' - The NetBIOS name of your self-managed directory, such as @CORP@.
--
-- 'tags', 'connectDirectory_tags' - The tags to be assigned to AD Connector.
--
-- 'name', 'connectDirectory_name' - The fully qualified name of your self-managed directory, such as
-- @corp.example.com@.
--
-- 'password', 'connectDirectory_password' - The password for your self-managed user account.
--
-- 'size', 'connectDirectory_size' - The size of the directory.
--
-- 'connectSettings', 'connectDirectory_connectSettings' - A DirectoryConnectSettings object that contains additional information
-- for the operation.
newConnectDirectory ::
  -- | 'name'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  -- | 'size'
  DirectorySize ->
  -- | 'connectSettings'
  DirectoryConnectSettings ->
  ConnectDirectory
newConnectDirectory
  pName_
  pPassword_
  pSize_
  pConnectSettings_ =
    ConnectDirectory'
      { description = Prelude.Nothing,
        shortName = Prelude.Nothing,
        tags = Prelude.Nothing,
        name = pName_,
        password = Data._Sensitive Lens.# pPassword_,
        size = pSize_,
        connectSettings = pConnectSettings_
      }

-- | A description for the directory.
connectDirectory_description :: Lens.Lens' ConnectDirectory (Prelude.Maybe Prelude.Text)
connectDirectory_description = Lens.lens (\ConnectDirectory' {description} -> description) (\s@ConnectDirectory' {} a -> s {description = a} :: ConnectDirectory)

-- | The NetBIOS name of your self-managed directory, such as @CORP@.
connectDirectory_shortName :: Lens.Lens' ConnectDirectory (Prelude.Maybe Prelude.Text)
connectDirectory_shortName = Lens.lens (\ConnectDirectory' {shortName} -> shortName) (\s@ConnectDirectory' {} a -> s {shortName = a} :: ConnectDirectory)

-- | The tags to be assigned to AD Connector.
connectDirectory_tags :: Lens.Lens' ConnectDirectory (Prelude.Maybe [Tag])
connectDirectory_tags = Lens.lens (\ConnectDirectory' {tags} -> tags) (\s@ConnectDirectory' {} a -> s {tags = a} :: ConnectDirectory) Prelude.. Lens.mapping Lens.coerced

-- | The fully qualified name of your self-managed directory, such as
-- @corp.example.com@.
connectDirectory_name :: Lens.Lens' ConnectDirectory Prelude.Text
connectDirectory_name = Lens.lens (\ConnectDirectory' {name} -> name) (\s@ConnectDirectory' {} a -> s {name = a} :: ConnectDirectory)

-- | The password for your self-managed user account.
connectDirectory_password :: Lens.Lens' ConnectDirectory Prelude.Text
connectDirectory_password = Lens.lens (\ConnectDirectory' {password} -> password) (\s@ConnectDirectory' {} a -> s {password = a} :: ConnectDirectory) Prelude.. Data._Sensitive

-- | The size of the directory.
connectDirectory_size :: Lens.Lens' ConnectDirectory DirectorySize
connectDirectory_size = Lens.lens (\ConnectDirectory' {size} -> size) (\s@ConnectDirectory' {} a -> s {size = a} :: ConnectDirectory)

-- | A DirectoryConnectSettings object that contains additional information
-- for the operation.
connectDirectory_connectSettings :: Lens.Lens' ConnectDirectory DirectoryConnectSettings
connectDirectory_connectSettings = Lens.lens (\ConnectDirectory' {connectSettings} -> connectSettings) (\s@ConnectDirectory' {} a -> s {connectSettings = a} :: ConnectDirectory)

instance Core.AWSRequest ConnectDirectory where
  type
    AWSResponse ConnectDirectory =
      ConnectDirectoryResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ConnectDirectoryResponse'
            Prelude.<$> (x Data..?> "DirectoryId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ConnectDirectory where
  hashWithSalt _salt ConnectDirectory' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` shortName
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` connectSettings

instance Prelude.NFData ConnectDirectory where
  rnf ConnectDirectory' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf shortName
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf size
      `Prelude.seq` Prelude.rnf connectSettings

instance Data.ToHeaders ConnectDirectory where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "DirectoryService_20150416.ConnectDirectory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ConnectDirectory where
  toJSON ConnectDirectory' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("ShortName" Data..=) Prelude.<$> shortName,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Password" Data..= password),
            Prelude.Just ("Size" Data..= size),
            Prelude.Just
              ("ConnectSettings" Data..= connectSettings)
          ]
      )

instance Data.ToPath ConnectDirectory where
  toPath = Prelude.const "/"

instance Data.ToQuery ConnectDirectory where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the results of the ConnectDirectory operation.
--
-- /See:/ 'newConnectDirectoryResponse' smart constructor.
data ConnectDirectoryResponse = ConnectDirectoryResponse'
  { -- | The identifier of the new directory.
    directoryId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ConnectDirectoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directoryId', 'connectDirectoryResponse_directoryId' - The identifier of the new directory.
--
-- 'httpStatus', 'connectDirectoryResponse_httpStatus' - The response's http status code.
newConnectDirectoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ConnectDirectoryResponse
newConnectDirectoryResponse pHttpStatus_ =
  ConnectDirectoryResponse'
    { directoryId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the new directory.
connectDirectoryResponse_directoryId :: Lens.Lens' ConnectDirectoryResponse (Prelude.Maybe Prelude.Text)
connectDirectoryResponse_directoryId = Lens.lens (\ConnectDirectoryResponse' {directoryId} -> directoryId) (\s@ConnectDirectoryResponse' {} a -> s {directoryId = a} :: ConnectDirectoryResponse)

-- | The response's http status code.
connectDirectoryResponse_httpStatus :: Lens.Lens' ConnectDirectoryResponse Prelude.Int
connectDirectoryResponse_httpStatus = Lens.lens (\ConnectDirectoryResponse' {httpStatus} -> httpStatus) (\s@ConnectDirectoryResponse' {} a -> s {httpStatus = a} :: ConnectDirectoryResponse)

instance Prelude.NFData ConnectDirectoryResponse where
  rnf ConnectDirectoryResponse' {..} =
    Prelude.rnf directoryId
      `Prelude.seq` Prelude.rnf httpStatus
