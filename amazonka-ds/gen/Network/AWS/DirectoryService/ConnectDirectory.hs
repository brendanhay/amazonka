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
-- Module      : Network.AWS.DirectoryService.ConnectDirectory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an AD Connector to connect to an on-premises directory.
--
-- Before you call @ConnectDirectory@, ensure that all of the required
-- permissions have been explicitly granted through a policy. For details
-- about what permissions are required to run the @ConnectDirectory@
-- operation, see
-- <http://docs.aws.amazon.com/directoryservice/latest/admin-guide/UsingWithDS_IAM_ResourcePermissions.html AWS Directory Service API Permissions: Actions, Resources, and Conditions Reference>.
module Network.AWS.DirectoryService.ConnectDirectory
  ( -- * Creating a Request
    ConnectDirectory (..),
    newConnectDirectory,

    -- * Request Lenses
    connectDirectory_shortName,
    connectDirectory_tags,
    connectDirectory_description,
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

import qualified Network.AWS.Core as Core
import Network.AWS.DirectoryService.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the inputs for the ConnectDirectory operation.
--
-- /See:/ 'newConnectDirectory' smart constructor.
data ConnectDirectory = ConnectDirectory'
  { -- | The NetBIOS name of the on-premises directory, such as @CORP@.
    shortName :: Core.Maybe Core.Text,
    -- | The tags to be assigned to AD Connector.
    tags :: Core.Maybe [Tag],
    -- | A description for the directory.
    description :: Core.Maybe Core.Text,
    -- | The fully qualified name of the on-premises directory, such as
    -- @corp.example.com@.
    name :: Core.Text,
    -- | The password for the on-premises user account.
    password :: Core.Sensitive Core.Text,
    -- | The size of the directory.
    size :: DirectorySize,
    -- | A DirectoryConnectSettings object that contains additional information
    -- for the operation.
    connectSettings :: DirectoryConnectSettings
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ConnectDirectory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'shortName', 'connectDirectory_shortName' - The NetBIOS name of the on-premises directory, such as @CORP@.
--
-- 'tags', 'connectDirectory_tags' - The tags to be assigned to AD Connector.
--
-- 'description', 'connectDirectory_description' - A description for the directory.
--
-- 'name', 'connectDirectory_name' - The fully qualified name of the on-premises directory, such as
-- @corp.example.com@.
--
-- 'password', 'connectDirectory_password' - The password for the on-premises user account.
--
-- 'size', 'connectDirectory_size' - The size of the directory.
--
-- 'connectSettings', 'connectDirectory_connectSettings' - A DirectoryConnectSettings object that contains additional information
-- for the operation.
newConnectDirectory ::
  -- | 'name'
  Core.Text ->
  -- | 'password'
  Core.Text ->
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
      { shortName = Core.Nothing,
        tags = Core.Nothing,
        description = Core.Nothing,
        name = pName_,
        password = Core._Sensitive Lens.# pPassword_,
        size = pSize_,
        connectSettings = pConnectSettings_
      }

-- | The NetBIOS name of the on-premises directory, such as @CORP@.
connectDirectory_shortName :: Lens.Lens' ConnectDirectory (Core.Maybe Core.Text)
connectDirectory_shortName = Lens.lens (\ConnectDirectory' {shortName} -> shortName) (\s@ConnectDirectory' {} a -> s {shortName = a} :: ConnectDirectory)

-- | The tags to be assigned to AD Connector.
connectDirectory_tags :: Lens.Lens' ConnectDirectory (Core.Maybe [Tag])
connectDirectory_tags = Lens.lens (\ConnectDirectory' {tags} -> tags) (\s@ConnectDirectory' {} a -> s {tags = a} :: ConnectDirectory) Core.. Lens.mapping Lens._Coerce

-- | A description for the directory.
connectDirectory_description :: Lens.Lens' ConnectDirectory (Core.Maybe Core.Text)
connectDirectory_description = Lens.lens (\ConnectDirectory' {description} -> description) (\s@ConnectDirectory' {} a -> s {description = a} :: ConnectDirectory)

-- | The fully qualified name of the on-premises directory, such as
-- @corp.example.com@.
connectDirectory_name :: Lens.Lens' ConnectDirectory Core.Text
connectDirectory_name = Lens.lens (\ConnectDirectory' {name} -> name) (\s@ConnectDirectory' {} a -> s {name = a} :: ConnectDirectory)

-- | The password for the on-premises user account.
connectDirectory_password :: Lens.Lens' ConnectDirectory Core.Text
connectDirectory_password = Lens.lens (\ConnectDirectory' {password} -> password) (\s@ConnectDirectory' {} a -> s {password = a} :: ConnectDirectory) Core.. Core._Sensitive

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ConnectDirectoryResponse'
            Core.<$> (x Core..?> "DirectoryId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ConnectDirectory

instance Core.NFData ConnectDirectory

instance Core.ToHeaders ConnectDirectory where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DirectoryService_20150416.ConnectDirectory" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ConnectDirectory where
  toJSON ConnectDirectory' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ShortName" Core..=) Core.<$> shortName,
            ("Tags" Core..=) Core.<$> tags,
            ("Description" Core..=) Core.<$> description,
            Core.Just ("Name" Core..= name),
            Core.Just ("Password" Core..= password),
            Core.Just ("Size" Core..= size),
            Core.Just
              ("ConnectSettings" Core..= connectSettings)
          ]
      )

instance Core.ToPath ConnectDirectory where
  toPath = Core.const "/"

instance Core.ToQuery ConnectDirectory where
  toQuery = Core.const Core.mempty

-- | Contains the results of the ConnectDirectory operation.
--
-- /See:/ 'newConnectDirectoryResponse' smart constructor.
data ConnectDirectoryResponse = ConnectDirectoryResponse'
  { -- | The identifier of the new directory.
    directoryId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ConnectDirectoryResponse
newConnectDirectoryResponse pHttpStatus_ =
  ConnectDirectoryResponse'
    { directoryId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The identifier of the new directory.
connectDirectoryResponse_directoryId :: Lens.Lens' ConnectDirectoryResponse (Core.Maybe Core.Text)
connectDirectoryResponse_directoryId = Lens.lens (\ConnectDirectoryResponse' {directoryId} -> directoryId) (\s@ConnectDirectoryResponse' {} a -> s {directoryId = a} :: ConnectDirectoryResponse)

-- | The response's http status code.
connectDirectoryResponse_httpStatus :: Lens.Lens' ConnectDirectoryResponse Core.Int
connectDirectoryResponse_httpStatus = Lens.lens (\ConnectDirectoryResponse' {httpStatus} -> httpStatus) (\s@ConnectDirectoryResponse' {} a -> s {httpStatus = a} :: ConnectDirectoryResponse)

instance Core.NFData ConnectDirectoryResponse
