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
-- Module      : Amazonka.DataSync.UpdateLocationObjectStorage
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates some parameters of an existing object storage location that
-- DataSync accesses for a transfer. For information about creating a
-- self-managed object storage location, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-object-location.html Creating a location for object storage>.
module Amazonka.DataSync.UpdateLocationObjectStorage
  ( -- * Creating a Request
    UpdateLocationObjectStorage (..),
    newUpdateLocationObjectStorage,

    -- * Request Lenses
    updateLocationObjectStorage_serverProtocol,
    updateLocationObjectStorage_serverCertificate,
    updateLocationObjectStorage_serverPort,
    updateLocationObjectStorage_accessKey,
    updateLocationObjectStorage_secretKey,
    updateLocationObjectStorage_subdirectory,
    updateLocationObjectStorage_agentArns,
    updateLocationObjectStorage_locationArn,

    -- * Destructuring the Response
    UpdateLocationObjectStorageResponse (..),
    newUpdateLocationObjectStorageResponse,

    -- * Response Lenses
    updateLocationObjectStorageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLocationObjectStorage' smart constructor.
data UpdateLocationObjectStorage = UpdateLocationObjectStorage'
  { -- | Specifies the protocol that your object storage server uses to
    -- communicate.
    serverProtocol :: Prelude.Maybe ObjectStorageServerProtocol,
    -- | Specifies a certificate to authenticate with an object storage system
    -- that uses a private or self-signed certificate authority (CA). You must
    -- specify a Base64-encoded @.pem@ file (for example,
    -- @file:\/\/\/home\/user\/.ssh\/storage_sys_certificate.pem@). The
    -- certificate can be up to 32768 bytes (before Base64 encoding).
    --
    -- To use this parameter, configure @ServerProtocol@ to @HTTPS@.
    --
    -- Updating the certificate doesn\'t interfere with tasks that you have in
    -- progress.
    serverCertificate :: Prelude.Maybe Data.Base64,
    -- | Specifies the port that your object storage server accepts inbound
    -- network traffic on (for example, port 443).
    serverPort :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the access key (for example, a user name) if credentials are
    -- required to authenticate with the object storage server.
    accessKey :: Prelude.Maybe Prelude.Text,
    -- | Specifies the secret key (for example, a password) if credentials are
    -- required to authenticate with the object storage server.
    secretKey :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Specifies the object prefix for your object storage server. If this is a
    -- source location, DataSync only copies objects with this prefix. If this
    -- is a destination location, DataSync writes all objects with this prefix.
    subdirectory :: Prelude.Maybe Prelude.Text,
    -- | Specifies the Amazon Resource Names (ARNs) of the DataSync agents that
    -- can securely connect with your location.
    agentArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Specifies the ARN of the object storage system location that you\'re
    -- updating.
    locationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLocationObjectStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverProtocol', 'updateLocationObjectStorage_serverProtocol' - Specifies the protocol that your object storage server uses to
-- communicate.
--
-- 'serverCertificate', 'updateLocationObjectStorage_serverCertificate' - Specifies a certificate to authenticate with an object storage system
-- that uses a private or self-signed certificate authority (CA). You must
-- specify a Base64-encoded @.pem@ file (for example,
-- @file:\/\/\/home\/user\/.ssh\/storage_sys_certificate.pem@). The
-- certificate can be up to 32768 bytes (before Base64 encoding).
--
-- To use this parameter, configure @ServerProtocol@ to @HTTPS@.
--
-- Updating the certificate doesn\'t interfere with tasks that you have in
-- progress.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'serverPort', 'updateLocationObjectStorage_serverPort' - Specifies the port that your object storage server accepts inbound
-- network traffic on (for example, port 443).
--
-- 'accessKey', 'updateLocationObjectStorage_accessKey' - Specifies the access key (for example, a user name) if credentials are
-- required to authenticate with the object storage server.
--
-- 'secretKey', 'updateLocationObjectStorage_secretKey' - Specifies the secret key (for example, a password) if credentials are
-- required to authenticate with the object storage server.
--
-- 'subdirectory', 'updateLocationObjectStorage_subdirectory' - Specifies the object prefix for your object storage server. If this is a
-- source location, DataSync only copies objects with this prefix. If this
-- is a destination location, DataSync writes all objects with this prefix.
--
-- 'agentArns', 'updateLocationObjectStorage_agentArns' - Specifies the Amazon Resource Names (ARNs) of the DataSync agents that
-- can securely connect with your location.
--
-- 'locationArn', 'updateLocationObjectStorage_locationArn' - Specifies the ARN of the object storage system location that you\'re
-- updating.
newUpdateLocationObjectStorage ::
  -- | 'locationArn'
  Prelude.Text ->
  UpdateLocationObjectStorage
newUpdateLocationObjectStorage pLocationArn_ =
  UpdateLocationObjectStorage'
    { serverProtocol =
        Prelude.Nothing,
      serverCertificate = Prelude.Nothing,
      serverPort = Prelude.Nothing,
      accessKey = Prelude.Nothing,
      secretKey = Prelude.Nothing,
      subdirectory = Prelude.Nothing,
      agentArns = Prelude.Nothing,
      locationArn = pLocationArn_
    }

-- | Specifies the protocol that your object storage server uses to
-- communicate.
updateLocationObjectStorage_serverProtocol :: Lens.Lens' UpdateLocationObjectStorage (Prelude.Maybe ObjectStorageServerProtocol)
updateLocationObjectStorage_serverProtocol = Lens.lens (\UpdateLocationObjectStorage' {serverProtocol} -> serverProtocol) (\s@UpdateLocationObjectStorage' {} a -> s {serverProtocol = a} :: UpdateLocationObjectStorage)

-- | Specifies a certificate to authenticate with an object storage system
-- that uses a private or self-signed certificate authority (CA). You must
-- specify a Base64-encoded @.pem@ file (for example,
-- @file:\/\/\/home\/user\/.ssh\/storage_sys_certificate.pem@). The
-- certificate can be up to 32768 bytes (before Base64 encoding).
--
-- To use this parameter, configure @ServerProtocol@ to @HTTPS@.
--
-- Updating the certificate doesn\'t interfere with tasks that you have in
-- progress.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
updateLocationObjectStorage_serverCertificate :: Lens.Lens' UpdateLocationObjectStorage (Prelude.Maybe Prelude.ByteString)
updateLocationObjectStorage_serverCertificate = Lens.lens (\UpdateLocationObjectStorage' {serverCertificate} -> serverCertificate) (\s@UpdateLocationObjectStorage' {} a -> s {serverCertificate = a} :: UpdateLocationObjectStorage) Prelude.. Lens.mapping Data._Base64

-- | Specifies the port that your object storage server accepts inbound
-- network traffic on (for example, port 443).
updateLocationObjectStorage_serverPort :: Lens.Lens' UpdateLocationObjectStorage (Prelude.Maybe Prelude.Natural)
updateLocationObjectStorage_serverPort = Lens.lens (\UpdateLocationObjectStorage' {serverPort} -> serverPort) (\s@UpdateLocationObjectStorage' {} a -> s {serverPort = a} :: UpdateLocationObjectStorage)

-- | Specifies the access key (for example, a user name) if credentials are
-- required to authenticate with the object storage server.
updateLocationObjectStorage_accessKey :: Lens.Lens' UpdateLocationObjectStorage (Prelude.Maybe Prelude.Text)
updateLocationObjectStorage_accessKey = Lens.lens (\UpdateLocationObjectStorage' {accessKey} -> accessKey) (\s@UpdateLocationObjectStorage' {} a -> s {accessKey = a} :: UpdateLocationObjectStorage)

-- | Specifies the secret key (for example, a password) if credentials are
-- required to authenticate with the object storage server.
updateLocationObjectStorage_secretKey :: Lens.Lens' UpdateLocationObjectStorage (Prelude.Maybe Prelude.Text)
updateLocationObjectStorage_secretKey = Lens.lens (\UpdateLocationObjectStorage' {secretKey} -> secretKey) (\s@UpdateLocationObjectStorage' {} a -> s {secretKey = a} :: UpdateLocationObjectStorage) Prelude.. Lens.mapping Data._Sensitive

-- | Specifies the object prefix for your object storage server. If this is a
-- source location, DataSync only copies objects with this prefix. If this
-- is a destination location, DataSync writes all objects with this prefix.
updateLocationObjectStorage_subdirectory :: Lens.Lens' UpdateLocationObjectStorage (Prelude.Maybe Prelude.Text)
updateLocationObjectStorage_subdirectory = Lens.lens (\UpdateLocationObjectStorage' {subdirectory} -> subdirectory) (\s@UpdateLocationObjectStorage' {} a -> s {subdirectory = a} :: UpdateLocationObjectStorage)

-- | Specifies the Amazon Resource Names (ARNs) of the DataSync agents that
-- can securely connect with your location.
updateLocationObjectStorage_agentArns :: Lens.Lens' UpdateLocationObjectStorage (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateLocationObjectStorage_agentArns = Lens.lens (\UpdateLocationObjectStorage' {agentArns} -> agentArns) (\s@UpdateLocationObjectStorage' {} a -> s {agentArns = a} :: UpdateLocationObjectStorage) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the ARN of the object storage system location that you\'re
-- updating.
updateLocationObjectStorage_locationArn :: Lens.Lens' UpdateLocationObjectStorage Prelude.Text
updateLocationObjectStorage_locationArn = Lens.lens (\UpdateLocationObjectStorage' {locationArn} -> locationArn) (\s@UpdateLocationObjectStorage' {} a -> s {locationArn = a} :: UpdateLocationObjectStorage)

instance Core.AWSRequest UpdateLocationObjectStorage where
  type
    AWSResponse UpdateLocationObjectStorage =
      UpdateLocationObjectStorageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateLocationObjectStorageResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateLocationObjectStorage where
  hashWithSalt _salt UpdateLocationObjectStorage' {..} =
    _salt `Prelude.hashWithSalt` serverProtocol
      `Prelude.hashWithSalt` serverCertificate
      `Prelude.hashWithSalt` serverPort
      `Prelude.hashWithSalt` accessKey
      `Prelude.hashWithSalt` secretKey
      `Prelude.hashWithSalt` subdirectory
      `Prelude.hashWithSalt` agentArns
      `Prelude.hashWithSalt` locationArn

instance Prelude.NFData UpdateLocationObjectStorage where
  rnf UpdateLocationObjectStorage' {..} =
    Prelude.rnf serverProtocol
      `Prelude.seq` Prelude.rnf serverCertificate
      `Prelude.seq` Prelude.rnf serverPort
      `Prelude.seq` Prelude.rnf accessKey
      `Prelude.seq` Prelude.rnf secretKey
      `Prelude.seq` Prelude.rnf subdirectory
      `Prelude.seq` Prelude.rnf agentArns
      `Prelude.seq` Prelude.rnf locationArn

instance Data.ToHeaders UpdateLocationObjectStorage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.UpdateLocationObjectStorage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateLocationObjectStorage where
  toJSON UpdateLocationObjectStorage' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ServerProtocol" Data..=)
              Prelude.<$> serverProtocol,
            ("ServerCertificate" Data..=)
              Prelude.<$> serverCertificate,
            ("ServerPort" Data..=) Prelude.<$> serverPort,
            ("AccessKey" Data..=) Prelude.<$> accessKey,
            ("SecretKey" Data..=) Prelude.<$> secretKey,
            ("Subdirectory" Data..=) Prelude.<$> subdirectory,
            ("AgentArns" Data..=) Prelude.<$> agentArns,
            Prelude.Just ("LocationArn" Data..= locationArn)
          ]
      )

instance Data.ToPath UpdateLocationObjectStorage where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateLocationObjectStorage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLocationObjectStorageResponse' smart constructor.
data UpdateLocationObjectStorageResponse = UpdateLocationObjectStorageResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLocationObjectStorageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateLocationObjectStorageResponse_httpStatus' - The response's http status code.
newUpdateLocationObjectStorageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateLocationObjectStorageResponse
newUpdateLocationObjectStorageResponse pHttpStatus_ =
  UpdateLocationObjectStorageResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateLocationObjectStorageResponse_httpStatus :: Lens.Lens' UpdateLocationObjectStorageResponse Prelude.Int
updateLocationObjectStorageResponse_httpStatus = Lens.lens (\UpdateLocationObjectStorageResponse' {httpStatus} -> httpStatus) (\s@UpdateLocationObjectStorageResponse' {} a -> s {httpStatus = a} :: UpdateLocationObjectStorageResponse)

instance
  Prelude.NFData
    UpdateLocationObjectStorageResponse
  where
  rnf UpdateLocationObjectStorageResponse' {..} =
    Prelude.rnf httpStatus
