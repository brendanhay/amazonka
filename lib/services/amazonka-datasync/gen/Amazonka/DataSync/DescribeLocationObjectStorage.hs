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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns metadata about your DataSync location for an object storage
-- system.
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
    describeLocationObjectStorageResponse_serverProtocol,
    describeLocationObjectStorageResponse_serverCertificate,
    describeLocationObjectStorageResponse_locationArn,
    describeLocationObjectStorageResponse_serverPort,
    describeLocationObjectStorageResponse_accessKey,
    describeLocationObjectStorageResponse_locationUri,
    describeLocationObjectStorageResponse_creationTime,
    describeLocationObjectStorageResponse_agentArns,
    describeLocationObjectStorageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | DescribeLocationObjectStorageRequest
--
-- /See:/ 'newDescribeLocationObjectStorage' smart constructor.
data DescribeLocationObjectStorage = DescribeLocationObjectStorage'
  { -- | The Amazon Resource Name (ARN) of the object storage system location
    -- that you want information about.
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
-- 'locationArn', 'describeLocationObjectStorage_locationArn' - The Amazon Resource Name (ARN) of the object storage system location
-- that you want information about.
newDescribeLocationObjectStorage ::
  -- | 'locationArn'
  Prelude.Text ->
  DescribeLocationObjectStorage
newDescribeLocationObjectStorage pLocationArn_ =
  DescribeLocationObjectStorage'
    { locationArn =
        pLocationArn_
    }

-- | The Amazon Resource Name (ARN) of the object storage system location
-- that you want information about.
describeLocationObjectStorage_locationArn :: Lens.Lens' DescribeLocationObjectStorage Prelude.Text
describeLocationObjectStorage_locationArn = Lens.lens (\DescribeLocationObjectStorage' {locationArn} -> locationArn) (\s@DescribeLocationObjectStorage' {} a -> s {locationArn = a} :: DescribeLocationObjectStorage)

instance
  Core.AWSRequest
    DescribeLocationObjectStorage
  where
  type
    AWSResponse DescribeLocationObjectStorage =
      DescribeLocationObjectStorageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeLocationObjectStorageResponse'
            Prelude.<$> (x Core..?> "ServerProtocol")
            Prelude.<*> (x Core..?> "ServerCertificate")
            Prelude.<*> (x Core..?> "LocationArn")
            Prelude.<*> (x Core..?> "ServerPort")
            Prelude.<*> (x Core..?> "AccessKey")
            Prelude.<*> (x Core..?> "LocationUri")
            Prelude.<*> (x Core..?> "CreationTime")
            Prelude.<*> (x Core..?> "AgentArns")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeLocationObjectStorage
  where
  hashWithSalt _salt DescribeLocationObjectStorage' {..} =
    _salt `Prelude.hashWithSalt` locationArn

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
  { -- | The protocol that your object storage system uses to communicate.
    serverProtocol :: Prelude.Maybe ObjectStorageServerProtocol,
    -- | The self-signed certificate that DataSync uses to securely authenticate
    -- with your object storage system.
    serverCertificate :: Prelude.Maybe Core.Base64,
    -- | The ARN of the object storage system location.
    locationArn :: Prelude.Maybe Prelude.Text,
    -- | The port that your object storage server accepts inbound network traffic
    -- on (for example, port 443).
    serverPort :: Prelude.Maybe Prelude.Natural,
    -- | The access key (for example, a user name) required to authenticate with
    -- the object storage system.
    accessKey :: Prelude.Maybe Prelude.Text,
    -- | The URL of the object storage system location.
    locationUri :: Prelude.Maybe Prelude.Text,
    -- | The time that the location was created.
    creationTime :: Prelude.Maybe Core.POSIX,
    -- | The ARNs of the DataSync agents that can securely connect with your
    -- location.
    agentArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
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
-- 'serverProtocol', 'describeLocationObjectStorageResponse_serverProtocol' - The protocol that your object storage system uses to communicate.
--
-- 'serverCertificate', 'describeLocationObjectStorageResponse_serverCertificate' - The self-signed certificate that DataSync uses to securely authenticate
-- with your object storage system.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'locationArn', 'describeLocationObjectStorageResponse_locationArn' - The ARN of the object storage system location.
--
-- 'serverPort', 'describeLocationObjectStorageResponse_serverPort' - The port that your object storage server accepts inbound network traffic
-- on (for example, port 443).
--
-- 'accessKey', 'describeLocationObjectStorageResponse_accessKey' - The access key (for example, a user name) required to authenticate with
-- the object storage system.
--
-- 'locationUri', 'describeLocationObjectStorageResponse_locationUri' - The URL of the object storage system location.
--
-- 'creationTime', 'describeLocationObjectStorageResponse_creationTime' - The time that the location was created.
--
-- 'agentArns', 'describeLocationObjectStorageResponse_agentArns' - The ARNs of the DataSync agents that can securely connect with your
-- location.
--
-- 'httpStatus', 'describeLocationObjectStorageResponse_httpStatus' - The response's http status code.
newDescribeLocationObjectStorageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeLocationObjectStorageResponse
newDescribeLocationObjectStorageResponse pHttpStatus_ =
  DescribeLocationObjectStorageResponse'
    { serverProtocol =
        Prelude.Nothing,
      serverCertificate = Prelude.Nothing,
      locationArn = Prelude.Nothing,
      serverPort = Prelude.Nothing,
      accessKey = Prelude.Nothing,
      locationUri = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      agentArns = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The protocol that your object storage system uses to communicate.
describeLocationObjectStorageResponse_serverProtocol :: Lens.Lens' DescribeLocationObjectStorageResponse (Prelude.Maybe ObjectStorageServerProtocol)
describeLocationObjectStorageResponse_serverProtocol = Lens.lens (\DescribeLocationObjectStorageResponse' {serverProtocol} -> serverProtocol) (\s@DescribeLocationObjectStorageResponse' {} a -> s {serverProtocol = a} :: DescribeLocationObjectStorageResponse)

-- | The self-signed certificate that DataSync uses to securely authenticate
-- with your object storage system.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
describeLocationObjectStorageResponse_serverCertificate :: Lens.Lens' DescribeLocationObjectStorageResponse (Prelude.Maybe Prelude.ByteString)
describeLocationObjectStorageResponse_serverCertificate = Lens.lens (\DescribeLocationObjectStorageResponse' {serverCertificate} -> serverCertificate) (\s@DescribeLocationObjectStorageResponse' {} a -> s {serverCertificate = a} :: DescribeLocationObjectStorageResponse) Prelude.. Lens.mapping Core._Base64

-- | The ARN of the object storage system location.
describeLocationObjectStorageResponse_locationArn :: Lens.Lens' DescribeLocationObjectStorageResponse (Prelude.Maybe Prelude.Text)
describeLocationObjectStorageResponse_locationArn = Lens.lens (\DescribeLocationObjectStorageResponse' {locationArn} -> locationArn) (\s@DescribeLocationObjectStorageResponse' {} a -> s {locationArn = a} :: DescribeLocationObjectStorageResponse)

-- | The port that your object storage server accepts inbound network traffic
-- on (for example, port 443).
describeLocationObjectStorageResponse_serverPort :: Lens.Lens' DescribeLocationObjectStorageResponse (Prelude.Maybe Prelude.Natural)
describeLocationObjectStorageResponse_serverPort = Lens.lens (\DescribeLocationObjectStorageResponse' {serverPort} -> serverPort) (\s@DescribeLocationObjectStorageResponse' {} a -> s {serverPort = a} :: DescribeLocationObjectStorageResponse)

-- | The access key (for example, a user name) required to authenticate with
-- the object storage system.
describeLocationObjectStorageResponse_accessKey :: Lens.Lens' DescribeLocationObjectStorageResponse (Prelude.Maybe Prelude.Text)
describeLocationObjectStorageResponse_accessKey = Lens.lens (\DescribeLocationObjectStorageResponse' {accessKey} -> accessKey) (\s@DescribeLocationObjectStorageResponse' {} a -> s {accessKey = a} :: DescribeLocationObjectStorageResponse)

-- | The URL of the object storage system location.
describeLocationObjectStorageResponse_locationUri :: Lens.Lens' DescribeLocationObjectStorageResponse (Prelude.Maybe Prelude.Text)
describeLocationObjectStorageResponse_locationUri = Lens.lens (\DescribeLocationObjectStorageResponse' {locationUri} -> locationUri) (\s@DescribeLocationObjectStorageResponse' {} a -> s {locationUri = a} :: DescribeLocationObjectStorageResponse)

-- | The time that the location was created.
describeLocationObjectStorageResponse_creationTime :: Lens.Lens' DescribeLocationObjectStorageResponse (Prelude.Maybe Prelude.UTCTime)
describeLocationObjectStorageResponse_creationTime = Lens.lens (\DescribeLocationObjectStorageResponse' {creationTime} -> creationTime) (\s@DescribeLocationObjectStorageResponse' {} a -> s {creationTime = a} :: DescribeLocationObjectStorageResponse) Prelude.. Lens.mapping Core._Time

-- | The ARNs of the DataSync agents that can securely connect with your
-- location.
describeLocationObjectStorageResponse_agentArns :: Lens.Lens' DescribeLocationObjectStorageResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeLocationObjectStorageResponse_agentArns = Lens.lens (\DescribeLocationObjectStorageResponse' {agentArns} -> agentArns) (\s@DescribeLocationObjectStorageResponse' {} a -> s {agentArns = a} :: DescribeLocationObjectStorageResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeLocationObjectStorageResponse_httpStatus :: Lens.Lens' DescribeLocationObjectStorageResponse Prelude.Int
describeLocationObjectStorageResponse_httpStatus = Lens.lens (\DescribeLocationObjectStorageResponse' {httpStatus} -> httpStatus) (\s@DescribeLocationObjectStorageResponse' {} a -> s {httpStatus = a} :: DescribeLocationObjectStorageResponse)

instance
  Prelude.NFData
    DescribeLocationObjectStorageResponse
  where
  rnf DescribeLocationObjectStorageResponse' {..} =
    Prelude.rnf serverProtocol
      `Prelude.seq` Prelude.rnf serverCertificate
      `Prelude.seq` Prelude.rnf locationArn
      `Prelude.seq` Prelude.rnf serverPort
      `Prelude.seq` Prelude.rnf accessKey
      `Prelude.seq` Prelude.rnf locationUri
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf agentArns
      `Prelude.seq` Prelude.rnf httpStatus
