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
-- Module      : Amazonka.StorageGateway.AssociateFileSystem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associate an Amazon FSx file system with the FSx File Gateway. After the
-- association process is complete, the file shares on the Amazon FSx file
-- system are available for access through the gateway. This operation only
-- supports the FSx File Gateway type.
module Amazonka.StorageGateway.AssociateFileSystem
  ( -- * Creating a Request
    AssociateFileSystem (..),
    newAssociateFileSystem,

    -- * Request Lenses
    associateFileSystem_auditDestinationARN,
    associateFileSystem_cacheAttributes,
    associateFileSystem_endpointNetworkConfiguration,
    associateFileSystem_tags,
    associateFileSystem_userName,
    associateFileSystem_password,
    associateFileSystem_clientToken,
    associateFileSystem_gatewayARN,
    associateFileSystem_locationARN,

    -- * Destructuring the Response
    AssociateFileSystemResponse (..),
    newAssociateFileSystemResponse,

    -- * Response Lenses
    associateFileSystemResponse_fileSystemAssociationARN,
    associateFileSystemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | /See:/ 'newAssociateFileSystem' smart constructor.
data AssociateFileSystem = AssociateFileSystem'
  { -- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
    auditDestinationARN :: Prelude.Maybe Prelude.Text,
    cacheAttributes :: Prelude.Maybe CacheAttributes,
    -- | Specifies the network configuration information for the gateway
    -- associated with the Amazon FSx file system.
    --
    -- If multiple file systems are associated with this gateway, this
    -- parameter\'s @IpAddresses@ field is required.
    endpointNetworkConfiguration :: Prelude.Maybe EndpointNetworkConfiguration,
    -- | A list of up to 50 tags that can be assigned to the file system
    -- association. Each tag is a key-value pair.
    tags :: Prelude.Maybe [Tag],
    -- | The user name of the user credential that has permission to access the
    -- root share D$ of the Amazon FSx file system. The user account must
    -- belong to the Amazon FSx delegated admin user group.
    userName :: Prelude.Text,
    -- | The password of the user credential.
    password :: Data.Sensitive Prelude.Text,
    -- | A unique string value that you supply that is used by the FSx File
    -- Gateway to ensure idempotent file system association creation.
    clientToken :: Prelude.Text,
    gatewayARN :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Amazon FSx file system to
    -- associate with the FSx File Gateway.
    locationARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateFileSystem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auditDestinationARN', 'associateFileSystem_auditDestinationARN' - The Amazon Resource Name (ARN) of the storage used for the audit logs.
--
-- 'cacheAttributes', 'associateFileSystem_cacheAttributes' - Undocumented member.
--
-- 'endpointNetworkConfiguration', 'associateFileSystem_endpointNetworkConfiguration' - Specifies the network configuration information for the gateway
-- associated with the Amazon FSx file system.
--
-- If multiple file systems are associated with this gateway, this
-- parameter\'s @IpAddresses@ field is required.
--
-- 'tags', 'associateFileSystem_tags' - A list of up to 50 tags that can be assigned to the file system
-- association. Each tag is a key-value pair.
--
-- 'userName', 'associateFileSystem_userName' - The user name of the user credential that has permission to access the
-- root share D$ of the Amazon FSx file system. The user account must
-- belong to the Amazon FSx delegated admin user group.
--
-- 'password', 'associateFileSystem_password' - The password of the user credential.
--
-- 'clientToken', 'associateFileSystem_clientToken' - A unique string value that you supply that is used by the FSx File
-- Gateway to ensure idempotent file system association creation.
--
-- 'gatewayARN', 'associateFileSystem_gatewayARN' - Undocumented member.
--
-- 'locationARN', 'associateFileSystem_locationARN' - The Amazon Resource Name (ARN) of the Amazon FSx file system to
-- associate with the FSx File Gateway.
newAssociateFileSystem ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'password'
  Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  -- | 'gatewayARN'
  Prelude.Text ->
  -- | 'locationARN'
  Prelude.Text ->
  AssociateFileSystem
newAssociateFileSystem
  pUserName_
  pPassword_
  pClientToken_
  pGatewayARN_
  pLocationARN_ =
    AssociateFileSystem'
      { auditDestinationARN =
          Prelude.Nothing,
        cacheAttributes = Prelude.Nothing,
        endpointNetworkConfiguration = Prelude.Nothing,
        tags = Prelude.Nothing,
        userName = pUserName_,
        password = Data._Sensitive Lens.# pPassword_,
        clientToken = pClientToken_,
        gatewayARN = pGatewayARN_,
        locationARN = pLocationARN_
      }

-- | The Amazon Resource Name (ARN) of the storage used for the audit logs.
associateFileSystem_auditDestinationARN :: Lens.Lens' AssociateFileSystem (Prelude.Maybe Prelude.Text)
associateFileSystem_auditDestinationARN = Lens.lens (\AssociateFileSystem' {auditDestinationARN} -> auditDestinationARN) (\s@AssociateFileSystem' {} a -> s {auditDestinationARN = a} :: AssociateFileSystem)

-- | Undocumented member.
associateFileSystem_cacheAttributes :: Lens.Lens' AssociateFileSystem (Prelude.Maybe CacheAttributes)
associateFileSystem_cacheAttributes = Lens.lens (\AssociateFileSystem' {cacheAttributes} -> cacheAttributes) (\s@AssociateFileSystem' {} a -> s {cacheAttributes = a} :: AssociateFileSystem)

-- | Specifies the network configuration information for the gateway
-- associated with the Amazon FSx file system.
--
-- If multiple file systems are associated with this gateway, this
-- parameter\'s @IpAddresses@ field is required.
associateFileSystem_endpointNetworkConfiguration :: Lens.Lens' AssociateFileSystem (Prelude.Maybe EndpointNetworkConfiguration)
associateFileSystem_endpointNetworkConfiguration = Lens.lens (\AssociateFileSystem' {endpointNetworkConfiguration} -> endpointNetworkConfiguration) (\s@AssociateFileSystem' {} a -> s {endpointNetworkConfiguration = a} :: AssociateFileSystem)

-- | A list of up to 50 tags that can be assigned to the file system
-- association. Each tag is a key-value pair.
associateFileSystem_tags :: Lens.Lens' AssociateFileSystem (Prelude.Maybe [Tag])
associateFileSystem_tags = Lens.lens (\AssociateFileSystem' {tags} -> tags) (\s@AssociateFileSystem' {} a -> s {tags = a} :: AssociateFileSystem) Prelude.. Lens.mapping Lens.coerced

-- | The user name of the user credential that has permission to access the
-- root share D$ of the Amazon FSx file system. The user account must
-- belong to the Amazon FSx delegated admin user group.
associateFileSystem_userName :: Lens.Lens' AssociateFileSystem Prelude.Text
associateFileSystem_userName = Lens.lens (\AssociateFileSystem' {userName} -> userName) (\s@AssociateFileSystem' {} a -> s {userName = a} :: AssociateFileSystem)

-- | The password of the user credential.
associateFileSystem_password :: Lens.Lens' AssociateFileSystem Prelude.Text
associateFileSystem_password = Lens.lens (\AssociateFileSystem' {password} -> password) (\s@AssociateFileSystem' {} a -> s {password = a} :: AssociateFileSystem) Prelude.. Data._Sensitive

-- | A unique string value that you supply that is used by the FSx File
-- Gateway to ensure idempotent file system association creation.
associateFileSystem_clientToken :: Lens.Lens' AssociateFileSystem Prelude.Text
associateFileSystem_clientToken = Lens.lens (\AssociateFileSystem' {clientToken} -> clientToken) (\s@AssociateFileSystem' {} a -> s {clientToken = a} :: AssociateFileSystem)

-- | Undocumented member.
associateFileSystem_gatewayARN :: Lens.Lens' AssociateFileSystem Prelude.Text
associateFileSystem_gatewayARN = Lens.lens (\AssociateFileSystem' {gatewayARN} -> gatewayARN) (\s@AssociateFileSystem' {} a -> s {gatewayARN = a} :: AssociateFileSystem)

-- | The Amazon Resource Name (ARN) of the Amazon FSx file system to
-- associate with the FSx File Gateway.
associateFileSystem_locationARN :: Lens.Lens' AssociateFileSystem Prelude.Text
associateFileSystem_locationARN = Lens.lens (\AssociateFileSystem' {locationARN} -> locationARN) (\s@AssociateFileSystem' {} a -> s {locationARN = a} :: AssociateFileSystem)

instance Core.AWSRequest AssociateFileSystem where
  type
    AWSResponse AssociateFileSystem =
      AssociateFileSystemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AssociateFileSystemResponse'
            Prelude.<$> (x Data..?> "FileSystemAssociationARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable AssociateFileSystem where
  hashWithSalt _salt AssociateFileSystem' {..} =
    _salt `Prelude.hashWithSalt` auditDestinationARN
      `Prelude.hashWithSalt` cacheAttributes
      `Prelude.hashWithSalt` endpointNetworkConfiguration
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` locationARN

instance Prelude.NFData AssociateFileSystem where
  rnf AssociateFileSystem' {..} =
    Prelude.rnf auditDestinationARN
      `Prelude.seq` Prelude.rnf cacheAttributes
      `Prelude.seq` Prelude.rnf endpointNetworkConfiguration
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf locationARN

instance Data.ToHeaders AssociateFileSystem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.AssociateFileSystem" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AssociateFileSystem where
  toJSON AssociateFileSystem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AuditDestinationARN" Data..=)
              Prelude.<$> auditDestinationARN,
            ("CacheAttributes" Data..=)
              Prelude.<$> cacheAttributes,
            ("EndpointNetworkConfiguration" Data..=)
              Prelude.<$> endpointNetworkConfiguration,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("UserName" Data..= userName),
            Prelude.Just ("Password" Data..= password),
            Prelude.Just ("ClientToken" Data..= clientToken),
            Prelude.Just ("GatewayARN" Data..= gatewayARN),
            Prelude.Just ("LocationARN" Data..= locationARN)
          ]
      )

instance Data.ToPath AssociateFileSystem where
  toPath = Prelude.const "/"

instance Data.ToQuery AssociateFileSystem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAssociateFileSystemResponse' smart constructor.
data AssociateFileSystemResponse = AssociateFileSystemResponse'
  { -- | The ARN of the newly created file system association.
    fileSystemAssociationARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociateFileSystemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileSystemAssociationARN', 'associateFileSystemResponse_fileSystemAssociationARN' - The ARN of the newly created file system association.
--
-- 'httpStatus', 'associateFileSystemResponse_httpStatus' - The response's http status code.
newAssociateFileSystemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AssociateFileSystemResponse
newAssociateFileSystemResponse pHttpStatus_ =
  AssociateFileSystemResponse'
    { fileSystemAssociationARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the newly created file system association.
associateFileSystemResponse_fileSystemAssociationARN :: Lens.Lens' AssociateFileSystemResponse (Prelude.Maybe Prelude.Text)
associateFileSystemResponse_fileSystemAssociationARN = Lens.lens (\AssociateFileSystemResponse' {fileSystemAssociationARN} -> fileSystemAssociationARN) (\s@AssociateFileSystemResponse' {} a -> s {fileSystemAssociationARN = a} :: AssociateFileSystemResponse)

-- | The response's http status code.
associateFileSystemResponse_httpStatus :: Lens.Lens' AssociateFileSystemResponse Prelude.Int
associateFileSystemResponse_httpStatus = Lens.lens (\AssociateFileSystemResponse' {httpStatus} -> httpStatus) (\s@AssociateFileSystemResponse' {} a -> s {httpStatus = a} :: AssociateFileSystemResponse)

instance Prelude.NFData AssociateFileSystemResponse where
  rnf AssociateFileSystemResponse' {..} =
    Prelude.rnf fileSystemAssociationARN
      `Prelude.seq` Prelude.rnf httpStatus
