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
-- Module      : Amazonka.DataSync.CreateLocationObjectStorage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an endpoint for a self-managed object storage bucket. For more
-- information about self-managed object storage locations, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-object-location.html Creating a location for object storage>.
module Amazonka.DataSync.CreateLocationObjectStorage
  ( -- * Creating a Request
    CreateLocationObjectStorage (..),
    newCreateLocationObjectStorage,

    -- * Request Lenses
    createLocationObjectStorage_serverPort,
    createLocationObjectStorage_serverProtocol,
    createLocationObjectStorage_secretKey,
    createLocationObjectStorage_subdirectory,
    createLocationObjectStorage_accessKey,
    createLocationObjectStorage_tags,
    createLocationObjectStorage_serverHostname,
    createLocationObjectStorage_bucketName,
    createLocationObjectStorage_agentArns,

    -- * Destructuring the Response
    CreateLocationObjectStorageResponse (..),
    newCreateLocationObjectStorageResponse,

    -- * Response Lenses
    createLocationObjectStorageResponse_locationArn,
    createLocationObjectStorageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DataSync.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | CreateLocationObjectStorageRequest
--
-- /See:/ 'newCreateLocationObjectStorage' smart constructor.
data CreateLocationObjectStorage = CreateLocationObjectStorage'
  { -- | The port that your self-managed object storage server accepts inbound
    -- network traffic on. The server port is set by default to TCP 80 (HTTP)
    -- or TCP 443 (HTTPS). You can specify a custom port if your self-managed
    -- object storage server requires one.
    serverPort :: Prelude.Maybe Prelude.Natural,
    -- | The protocol that the object storage server uses to communicate. Valid
    -- values are HTTP or HTTPS.
    serverProtocol :: Prelude.Maybe ObjectStorageServerProtocol,
    -- | Optional. The secret key is used if credentials are required to access
    -- the self-managed object storage server. If your object storage requires
    -- a user name and password to authenticate, use @AccessKey@ and
    -- @SecretKey@ to provide the user name and password, respectively.
    secretKey :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The subdirectory in the self-managed object storage server that is used
    -- to read data from.
    subdirectory :: Prelude.Maybe Prelude.Text,
    -- | Optional. The access key is used if credentials are required to access
    -- the self-managed object storage server. If your object storage requires
    -- a user name and password to authenticate, use @AccessKey@ and
    -- @SecretKey@ to provide the user name and password, respectively.
    accessKey :: Prelude.Maybe Prelude.Text,
    -- | The key-value pair that represents the tag that you want to add to the
    -- location. The value can be an empty string. We recommend using tags to
    -- name your resources.
    tags :: Prelude.Maybe [TagListEntry],
    -- | The name of the self-managed object storage server. This value is the IP
    -- address or Domain Name Service (DNS) name of the object storage server.
    -- An agent uses this host name to mount the object storage server in a
    -- network.
    serverHostname :: Prelude.Text,
    -- | The bucket on the self-managed object storage server that is used to
    -- read data from.
    bucketName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the agents associated with the
    -- self-managed object storage server location.
    agentArns :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocationObjectStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverPort', 'createLocationObjectStorage_serverPort' - The port that your self-managed object storage server accepts inbound
-- network traffic on. The server port is set by default to TCP 80 (HTTP)
-- or TCP 443 (HTTPS). You can specify a custom port if your self-managed
-- object storage server requires one.
--
-- 'serverProtocol', 'createLocationObjectStorage_serverProtocol' - The protocol that the object storage server uses to communicate. Valid
-- values are HTTP or HTTPS.
--
-- 'secretKey', 'createLocationObjectStorage_secretKey' - Optional. The secret key is used if credentials are required to access
-- the self-managed object storage server. If your object storage requires
-- a user name and password to authenticate, use @AccessKey@ and
-- @SecretKey@ to provide the user name and password, respectively.
--
-- 'subdirectory', 'createLocationObjectStorage_subdirectory' - The subdirectory in the self-managed object storage server that is used
-- to read data from.
--
-- 'accessKey', 'createLocationObjectStorage_accessKey' - Optional. The access key is used if credentials are required to access
-- the self-managed object storage server. If your object storage requires
-- a user name and password to authenticate, use @AccessKey@ and
-- @SecretKey@ to provide the user name and password, respectively.
--
-- 'tags', 'createLocationObjectStorage_tags' - The key-value pair that represents the tag that you want to add to the
-- location. The value can be an empty string. We recommend using tags to
-- name your resources.
--
-- 'serverHostname', 'createLocationObjectStorage_serverHostname' - The name of the self-managed object storage server. This value is the IP
-- address or Domain Name Service (DNS) name of the object storage server.
-- An agent uses this host name to mount the object storage server in a
-- network.
--
-- 'bucketName', 'createLocationObjectStorage_bucketName' - The bucket on the self-managed object storage server that is used to
-- read data from.
--
-- 'agentArns', 'createLocationObjectStorage_agentArns' - The Amazon Resource Name (ARN) of the agents associated with the
-- self-managed object storage server location.
newCreateLocationObjectStorage ::
  -- | 'serverHostname'
  Prelude.Text ->
  -- | 'bucketName'
  Prelude.Text ->
  -- | 'agentArns'
  Prelude.NonEmpty Prelude.Text ->
  CreateLocationObjectStorage
newCreateLocationObjectStorage
  pServerHostname_
  pBucketName_
  pAgentArns_ =
    CreateLocationObjectStorage'
      { serverPort =
          Prelude.Nothing,
        serverProtocol = Prelude.Nothing,
        secretKey = Prelude.Nothing,
        subdirectory = Prelude.Nothing,
        accessKey = Prelude.Nothing,
        tags = Prelude.Nothing,
        serverHostname = pServerHostname_,
        bucketName = pBucketName_,
        agentArns = Lens.coerced Lens.# pAgentArns_
      }

-- | The port that your self-managed object storage server accepts inbound
-- network traffic on. The server port is set by default to TCP 80 (HTTP)
-- or TCP 443 (HTTPS). You can specify a custom port if your self-managed
-- object storage server requires one.
createLocationObjectStorage_serverPort :: Lens.Lens' CreateLocationObjectStorage (Prelude.Maybe Prelude.Natural)
createLocationObjectStorage_serverPort = Lens.lens (\CreateLocationObjectStorage' {serverPort} -> serverPort) (\s@CreateLocationObjectStorage' {} a -> s {serverPort = a} :: CreateLocationObjectStorage)

-- | The protocol that the object storage server uses to communicate. Valid
-- values are HTTP or HTTPS.
createLocationObjectStorage_serverProtocol :: Lens.Lens' CreateLocationObjectStorage (Prelude.Maybe ObjectStorageServerProtocol)
createLocationObjectStorage_serverProtocol = Lens.lens (\CreateLocationObjectStorage' {serverProtocol} -> serverProtocol) (\s@CreateLocationObjectStorage' {} a -> s {serverProtocol = a} :: CreateLocationObjectStorage)

-- | Optional. The secret key is used if credentials are required to access
-- the self-managed object storage server. If your object storage requires
-- a user name and password to authenticate, use @AccessKey@ and
-- @SecretKey@ to provide the user name and password, respectively.
createLocationObjectStorage_secretKey :: Lens.Lens' CreateLocationObjectStorage (Prelude.Maybe Prelude.Text)
createLocationObjectStorage_secretKey = Lens.lens (\CreateLocationObjectStorage' {secretKey} -> secretKey) (\s@CreateLocationObjectStorage' {} a -> s {secretKey = a} :: CreateLocationObjectStorage) Prelude.. Lens.mapping Core._Sensitive

-- | The subdirectory in the self-managed object storage server that is used
-- to read data from.
createLocationObjectStorage_subdirectory :: Lens.Lens' CreateLocationObjectStorage (Prelude.Maybe Prelude.Text)
createLocationObjectStorage_subdirectory = Lens.lens (\CreateLocationObjectStorage' {subdirectory} -> subdirectory) (\s@CreateLocationObjectStorage' {} a -> s {subdirectory = a} :: CreateLocationObjectStorage)

-- | Optional. The access key is used if credentials are required to access
-- the self-managed object storage server. If your object storage requires
-- a user name and password to authenticate, use @AccessKey@ and
-- @SecretKey@ to provide the user name and password, respectively.
createLocationObjectStorage_accessKey :: Lens.Lens' CreateLocationObjectStorage (Prelude.Maybe Prelude.Text)
createLocationObjectStorage_accessKey = Lens.lens (\CreateLocationObjectStorage' {accessKey} -> accessKey) (\s@CreateLocationObjectStorage' {} a -> s {accessKey = a} :: CreateLocationObjectStorage)

-- | The key-value pair that represents the tag that you want to add to the
-- location. The value can be an empty string. We recommend using tags to
-- name your resources.
createLocationObjectStorage_tags :: Lens.Lens' CreateLocationObjectStorage (Prelude.Maybe [TagListEntry])
createLocationObjectStorage_tags = Lens.lens (\CreateLocationObjectStorage' {tags} -> tags) (\s@CreateLocationObjectStorage' {} a -> s {tags = a} :: CreateLocationObjectStorage) Prelude.. Lens.mapping Lens.coerced

-- | The name of the self-managed object storage server. This value is the IP
-- address or Domain Name Service (DNS) name of the object storage server.
-- An agent uses this host name to mount the object storage server in a
-- network.
createLocationObjectStorage_serverHostname :: Lens.Lens' CreateLocationObjectStorage Prelude.Text
createLocationObjectStorage_serverHostname = Lens.lens (\CreateLocationObjectStorage' {serverHostname} -> serverHostname) (\s@CreateLocationObjectStorage' {} a -> s {serverHostname = a} :: CreateLocationObjectStorage)

-- | The bucket on the self-managed object storage server that is used to
-- read data from.
createLocationObjectStorage_bucketName :: Lens.Lens' CreateLocationObjectStorage Prelude.Text
createLocationObjectStorage_bucketName = Lens.lens (\CreateLocationObjectStorage' {bucketName} -> bucketName) (\s@CreateLocationObjectStorage' {} a -> s {bucketName = a} :: CreateLocationObjectStorage)

-- | The Amazon Resource Name (ARN) of the agents associated with the
-- self-managed object storage server location.
createLocationObjectStorage_agentArns :: Lens.Lens' CreateLocationObjectStorage (Prelude.NonEmpty Prelude.Text)
createLocationObjectStorage_agentArns = Lens.lens (\CreateLocationObjectStorage' {agentArns} -> agentArns) (\s@CreateLocationObjectStorage' {} a -> s {agentArns = a} :: CreateLocationObjectStorage) Prelude.. Lens.coerced

instance Core.AWSRequest CreateLocationObjectStorage where
  type
    AWSResponse CreateLocationObjectStorage =
      CreateLocationObjectStorageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateLocationObjectStorageResponse'
            Prelude.<$> (x Core..?> "LocationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateLocationObjectStorage where
  hashWithSalt _salt CreateLocationObjectStorage' {..} =
    _salt `Prelude.hashWithSalt` serverPort
      `Prelude.hashWithSalt` serverProtocol
      `Prelude.hashWithSalt` secretKey
      `Prelude.hashWithSalt` subdirectory
      `Prelude.hashWithSalt` accessKey
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` serverHostname
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` agentArns

instance Prelude.NFData CreateLocationObjectStorage where
  rnf CreateLocationObjectStorage' {..} =
    Prelude.rnf serverPort
      `Prelude.seq` Prelude.rnf serverProtocol
      `Prelude.seq` Prelude.rnf secretKey
      `Prelude.seq` Prelude.rnf subdirectory
      `Prelude.seq` Prelude.rnf accessKey
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf serverHostname
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf agentArns

instance Core.ToHeaders CreateLocationObjectStorage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "FmrsService.CreateLocationObjectStorage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateLocationObjectStorage where
  toJSON CreateLocationObjectStorage' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ServerPort" Core..=) Prelude.<$> serverPort,
            ("ServerProtocol" Core..=)
              Prelude.<$> serverProtocol,
            ("SecretKey" Core..=) Prelude.<$> secretKey,
            ("Subdirectory" Core..=) Prelude.<$> subdirectory,
            ("AccessKey" Core..=) Prelude.<$> accessKey,
            ("Tags" Core..=) Prelude.<$> tags,
            Prelude.Just
              ("ServerHostname" Core..= serverHostname),
            Prelude.Just ("BucketName" Core..= bucketName),
            Prelude.Just ("AgentArns" Core..= agentArns)
          ]
      )

instance Core.ToPath CreateLocationObjectStorage where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateLocationObjectStorage where
  toQuery = Prelude.const Prelude.mempty

-- | CreateLocationObjectStorageResponse
--
-- /See:/ 'newCreateLocationObjectStorageResponse' smart constructor.
data CreateLocationObjectStorageResponse = CreateLocationObjectStorageResponse'
  { -- | The Amazon Resource Name (ARN) of the agents associated with the
    -- self-managed object storage server location.
    locationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateLocationObjectStorageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'createLocationObjectStorageResponse_locationArn' - The Amazon Resource Name (ARN) of the agents associated with the
-- self-managed object storage server location.
--
-- 'httpStatus', 'createLocationObjectStorageResponse_httpStatus' - The response's http status code.
newCreateLocationObjectStorageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateLocationObjectStorageResponse
newCreateLocationObjectStorageResponse pHttpStatus_ =
  CreateLocationObjectStorageResponse'
    { locationArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the agents associated with the
-- self-managed object storage server location.
createLocationObjectStorageResponse_locationArn :: Lens.Lens' CreateLocationObjectStorageResponse (Prelude.Maybe Prelude.Text)
createLocationObjectStorageResponse_locationArn = Lens.lens (\CreateLocationObjectStorageResponse' {locationArn} -> locationArn) (\s@CreateLocationObjectStorageResponse' {} a -> s {locationArn = a} :: CreateLocationObjectStorageResponse)

-- | The response's http status code.
createLocationObjectStorageResponse_httpStatus :: Lens.Lens' CreateLocationObjectStorageResponse Prelude.Int
createLocationObjectStorageResponse_httpStatus = Lens.lens (\CreateLocationObjectStorageResponse' {httpStatus} -> httpStatus) (\s@CreateLocationObjectStorageResponse' {} a -> s {httpStatus = a} :: CreateLocationObjectStorageResponse)

instance
  Prelude.NFData
    CreateLocationObjectStorageResponse
  where
  rnf CreateLocationObjectStorageResponse' {..} =
    Prelude.rnf locationArn
      `Prelude.seq` Prelude.rnf httpStatus
