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
-- Module      : Amazonka.DataSync.DescribeLocationObjectStorage
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about a self-managed object storage server location.
-- For more information about self-managed object storage locations, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/create-object-location.html Creating a location for object storage>.
module Amazonka.DataSync.DescribeLocationObjectStorage
  ( -- * Creating a Request
    DescribeLocationObjectStorage (..),
    newDescribeLocationObjectStorage,

    -- * Request Lenses
    describeLocationObjectStorage_locationArn,

    -- * Destructuring the Response
    DescribeLocationObjectStorageResponse (..),
    newDescribeLocationObjectStorageResponse,

    -- * Response Lenses
    describeLocationObjectStorageResponse_serverPort,
    describeLocationObjectStorageResponse_creationTime,
    describeLocationObjectStorageResponse_agentArns,
    describeLocationObjectStorageResponse_locationUri,
    describeLocationObjectStorageResponse_serverProtocol,
    describeLocationObjectStorageResponse_locationArn,
    describeLocationObjectStorageResponse_accessKey,
    describeLocationObjectStorageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.DataSync.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | DescribeLocationObjectStorageRequest
--
-- /See:/ 'newDescribeLocationObjectStorage' smart constructor.
data DescribeLocationObjectStorage = DescribeLocationObjectStorage'
  { -- | The Amazon Resource Name (ARN) of the self-managed object storage server
    -- location that was described.
    locationArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocationObjectStorage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'locationArn', 'describeLocationObjectStorage_locationArn' - The Amazon Resource Name (ARN) of the self-managed object storage server
-- location that was described.
newDescribeLocationObjectStorage ::
  -- | 'locationArn'
  Prelude.Text ->
  DescribeLocationObjectStorage
newDescribeLocationObjectStorage pLocationArn_ =
  DescribeLocationObjectStorage'
    { locationArn =
        pLocationArn_
    }

-- | The Amazon Resource Name (ARN) of the self-managed object storage server
-- location that was described.
describeLocationObjectStorage_locationArn :: Lens.Lens' DescribeLocationObjectStorage Prelude.Text
describeLocationObjectStorage_locationArn = Lens.lens (\DescribeLocationObjectStorage' {locationArn} -> locationArn) (\s@DescribeLocationObjectStorage' {} a -> s {locationArn = a} :: DescribeLocationObjectStorage)

instance
  Core.AWSRequest
    DescribeLocationObjectStorage
  where
  type
    AWSResponse DescribeLocationObjectStorage =
      DescribeLocationObjectStorageResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLocationObjectStorageResponse'
            Prelude.<$> (x Core..?> "ServerPort")
            Prelude.<*> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "AgentArns")
            Prelude.<*> (x Core..?> "LocationUri")
            Prelude.<*> (x Core..?> "ServerProtocol")
            Prelude.<*> (x Core..?> "LocationArn")
            Prelude.<*> (x Core..?> "AccessKey")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeLocationObjectStorage
  where
  hashWithSalt salt' DescribeLocationObjectStorage' {..} =
    salt' `Prelude.hashWithSalt` locationArn

instance Prelude.NFData DescribeLocationObjectStorage where
  rnf DescribeLocationObjectStorage' {..} =
    Prelude.rnf locationArn

instance Core.ToHeaders DescribeLocationObjectStorage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "FmrsService.DescribeLocationObjectStorage" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeLocationObjectStorage where
  toJSON DescribeLocationObjectStorage' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("LocationArn" Core..= locationArn)]
      )

instance Core.ToPath DescribeLocationObjectStorage where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeLocationObjectStorage where
  toQuery = Prelude.const Prelude.mempty

-- | DescribeLocationObjectStorageResponse
--
-- /See:/ 'newDescribeLocationObjectStorageResponse' smart constructor.
data DescribeLocationObjectStorageResponse = DescribeLocationObjectStorageResponse'
  { -- | The port that your self-managed object storage server accepts inbound
    -- network traffic on. The server port is set by default to TCP 80 (HTTP)
    -- or TCP 443 (HTTPS).
    serverPort :: Prelude.Maybe Prelude.Natural,
    -- | The time that the self-managed object storage server agent was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the agents associated with the
    -- self-managed object storage server location.
    agentArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The URL of the source self-managed object storage server location that
    -- was described.
    locationUri :: Prelude.Maybe Prelude.Text,
    -- | The protocol that the object storage server uses to communicate. Valid
    -- values are HTTP or HTTPS.
    serverProtocol :: Prelude.Maybe ObjectStorageServerProtocol,
    -- | The Amazon Resource Name (ARN) of the self-managed object storage server
    -- location to describe.
    locationArn :: Prelude.Maybe Prelude.Text,
    -- | Optional. The access key is used if credentials are required to access
    -- the self-managed object storage server. If your object storage requires
    -- a user name and password to authenticate, use @AccessKey@ and
    -- @SecretKey@ to provide the user name and password, respectively.
    accessKey :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeLocationObjectStorageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serverPort', 'describeLocationObjectStorageResponse_serverPort' - The port that your self-managed object storage server accepts inbound
-- network traffic on. The server port is set by default to TCP 80 (HTTP)
-- or TCP 443 (HTTPS).
--
-- 'creationTime', 'describeLocationObjectStorageResponse_creationTime' - The time that the self-managed object storage server agent was created.
--
-- 'agentArns', 'describeLocationObjectStorageResponse_agentArns' - The Amazon Resource Name (ARN) of the agents associated with the
-- self-managed object storage server location.
--
-- 'locationUri', 'describeLocationObjectStorageResponse_locationUri' - The URL of the source self-managed object storage server location that
-- was described.
--
-- 'serverProtocol', 'describeLocationObjectStorageResponse_serverProtocol' - The protocol that the object storage server uses to communicate. Valid
-- values are HTTP or HTTPS.
--
-- 'locationArn', 'describeLocationObjectStorageResponse_locationArn' - The Amazon Resource Name (ARN) of the self-managed object storage server
-- location to describe.
--
-- 'accessKey', 'describeLocationObjectStorageResponse_accessKey' - Optional. The access key is used if credentials are required to access
-- the self-managed object storage server. If your object storage requires
-- a user name and password to authenticate, use @AccessKey@ and
-- @SecretKey@ to provide the user name and password, respectively.
--
-- 'httpStatus', 'describeLocationObjectStorageResponse_httpStatus' - The response's http status code.
newDescribeLocationObjectStorageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLocationObjectStorageResponse
newDescribeLocationObjectStorageResponse pHttpStatus_ =
  DescribeLocationObjectStorageResponse'
    { serverPort =
        Prelude.Nothing,
      creationTime = Prelude.Nothing,
      agentArns = Prelude.Nothing,
      locationUri = Prelude.Nothing,
      serverProtocol = Prelude.Nothing,
      locationArn = Prelude.Nothing,
      accessKey = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The port that your self-managed object storage server accepts inbound
-- network traffic on. The server port is set by default to TCP 80 (HTTP)
-- or TCP 443 (HTTPS).
describeLocationObjectStorageResponse_serverPort :: Lens.Lens' DescribeLocationObjectStorageResponse (Prelude.Maybe Prelude.Natural)
describeLocationObjectStorageResponse_serverPort = Lens.lens (\DescribeLocationObjectStorageResponse' {serverPort} -> serverPort) (\s@DescribeLocationObjectStorageResponse' {} a -> s {serverPort = a} :: DescribeLocationObjectStorageResponse)

-- | The time that the self-managed object storage server agent was created.
describeLocationObjectStorageResponse_creationTime :: Lens.Lens' DescribeLocationObjectStorageResponse (Prelude.Maybe Prelude.UTCTime)
describeLocationObjectStorageResponse_creationTime = Lens.lens (\DescribeLocationObjectStorageResponse' {creationTime} -> creationTime) (\s@DescribeLocationObjectStorageResponse' {} a -> s {creationTime = a} :: DescribeLocationObjectStorageResponse) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the agents associated with the
-- self-managed object storage server location.
describeLocationObjectStorageResponse_agentArns :: Lens.Lens' DescribeLocationObjectStorageResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeLocationObjectStorageResponse_agentArns = Lens.lens (\DescribeLocationObjectStorageResponse' {agentArns} -> agentArns) (\s@DescribeLocationObjectStorageResponse' {} a -> s {agentArns = a} :: DescribeLocationObjectStorageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The URL of the source self-managed object storage server location that
-- was described.
describeLocationObjectStorageResponse_locationUri :: Lens.Lens' DescribeLocationObjectStorageResponse (Prelude.Maybe Prelude.Text)
describeLocationObjectStorageResponse_locationUri = Lens.lens (\DescribeLocationObjectStorageResponse' {locationUri} -> locationUri) (\s@DescribeLocationObjectStorageResponse' {} a -> s {locationUri = a} :: DescribeLocationObjectStorageResponse)

-- | The protocol that the object storage server uses to communicate. Valid
-- values are HTTP or HTTPS.
describeLocationObjectStorageResponse_serverProtocol :: Lens.Lens' DescribeLocationObjectStorageResponse (Prelude.Maybe ObjectStorageServerProtocol)
describeLocationObjectStorageResponse_serverProtocol = Lens.lens (\DescribeLocationObjectStorageResponse' {serverProtocol} -> serverProtocol) (\s@DescribeLocationObjectStorageResponse' {} a -> s {serverProtocol = a} :: DescribeLocationObjectStorageResponse)

-- | The Amazon Resource Name (ARN) of the self-managed object storage server
-- location to describe.
describeLocationObjectStorageResponse_locationArn :: Lens.Lens' DescribeLocationObjectStorageResponse (Prelude.Maybe Prelude.Text)
describeLocationObjectStorageResponse_locationArn = Lens.lens (\DescribeLocationObjectStorageResponse' {locationArn} -> locationArn) (\s@DescribeLocationObjectStorageResponse' {} a -> s {locationArn = a} :: DescribeLocationObjectStorageResponse)

-- | Optional. The access key is used if credentials are required to access
-- the self-managed object storage server. If your object storage requires
-- a user name and password to authenticate, use @AccessKey@ and
-- @SecretKey@ to provide the user name and password, respectively.
describeLocationObjectStorageResponse_accessKey :: Lens.Lens' DescribeLocationObjectStorageResponse (Prelude.Maybe Prelude.Text)
describeLocationObjectStorageResponse_accessKey = Lens.lens (\DescribeLocationObjectStorageResponse' {accessKey} -> accessKey) (\s@DescribeLocationObjectStorageResponse' {} a -> s {accessKey = a} :: DescribeLocationObjectStorageResponse)

-- | The response's http status code.
describeLocationObjectStorageResponse_httpStatus :: Lens.Lens' DescribeLocationObjectStorageResponse Prelude.Int
describeLocationObjectStorageResponse_httpStatus = Lens.lens (\DescribeLocationObjectStorageResponse' {httpStatus} -> httpStatus) (\s@DescribeLocationObjectStorageResponse' {} a -> s {httpStatus = a} :: DescribeLocationObjectStorageResponse)

instance
  Prelude.NFData
    DescribeLocationObjectStorageResponse
  where
  rnf DescribeLocationObjectStorageResponse' {..} =
    Prelude.rnf serverPort
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf accessKey
      `Prelude.seq` Prelude.rnf locationArn
      `Prelude.seq` Prelude.rnf serverProtocol
      `Prelude.seq` Prelude.rnf locationUri
      `Prelude.seq` Prelude.rnf agentArns
      `Prelude.seq` Prelude.rnf creationTime
