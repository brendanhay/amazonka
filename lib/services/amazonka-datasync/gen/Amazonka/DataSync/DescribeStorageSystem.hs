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
-- Module      : Amazonka.DataSync.DescribeStorageSystem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about an on-premises storage system that you\'re
-- using with DataSync Discovery.
module Amazonka.DataSync.DescribeStorageSystem
  ( -- * Creating a Request
    DescribeStorageSystem (..),
    newDescribeStorageSystem,

    -- * Request Lenses
    describeStorageSystem_storageSystemArn,

    -- * Destructuring the Response
    DescribeStorageSystemResponse (..),
    newDescribeStorageSystemResponse,

    -- * Response Lenses
    describeStorageSystemResponse_agentArns,
    describeStorageSystemResponse_cloudWatchLogGroupArn,
    describeStorageSystemResponse_connectivityStatus,
    describeStorageSystemResponse_creationTime,
    describeStorageSystemResponse_errorMessage,
    describeStorageSystemResponse_name,
    describeStorageSystemResponse_secretsManagerArn,
    describeStorageSystemResponse_serverConfiguration,
    describeStorageSystemResponse_storageSystemArn,
    describeStorageSystemResponse_systemType,
    describeStorageSystemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeStorageSystem' smart constructor.
data DescribeStorageSystem = DescribeStorageSystem'
  { -- | Specifies the Amazon Resource Name (ARN) of an on-premises storage
    -- system that you\'re using with DataSync Discovery.
    storageSystemArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStorageSystem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageSystemArn', 'describeStorageSystem_storageSystemArn' - Specifies the Amazon Resource Name (ARN) of an on-premises storage
-- system that you\'re using with DataSync Discovery.
newDescribeStorageSystem ::
  -- | 'storageSystemArn'
  Prelude.Text ->
  DescribeStorageSystem
newDescribeStorageSystem pStorageSystemArn_ =
  DescribeStorageSystem'
    { storageSystemArn =
        pStorageSystemArn_
    }

-- | Specifies the Amazon Resource Name (ARN) of an on-premises storage
-- system that you\'re using with DataSync Discovery.
describeStorageSystem_storageSystemArn :: Lens.Lens' DescribeStorageSystem Prelude.Text
describeStorageSystem_storageSystemArn = Lens.lens (\DescribeStorageSystem' {storageSystemArn} -> storageSystemArn) (\s@DescribeStorageSystem' {} a -> s {storageSystemArn = a} :: DescribeStorageSystem)

instance Core.AWSRequest DescribeStorageSystem where
  type
    AWSResponse DescribeStorageSystem =
      DescribeStorageSystemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeStorageSystemResponse'
            Prelude.<$> (x Data..?> "AgentArns")
            Prelude.<*> (x Data..?> "CloudWatchLogGroupArn")
            Prelude.<*> (x Data..?> "ConnectivityStatus")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "ErrorMessage")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "SecretsManagerArn")
            Prelude.<*> (x Data..?> "ServerConfiguration")
            Prelude.<*> (x Data..?> "StorageSystemArn")
            Prelude.<*> (x Data..?> "SystemType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeStorageSystem where
  hashWithSalt _salt DescribeStorageSystem' {..} =
    _salt `Prelude.hashWithSalt` storageSystemArn

instance Prelude.NFData DescribeStorageSystem where
  rnf DescribeStorageSystem' {..} =
    Prelude.rnf storageSystemArn

instance Data.ToHeaders DescribeStorageSystem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.DescribeStorageSystem" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeStorageSystem where
  toJSON DescribeStorageSystem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("StorageSystemArn" Data..= storageSystemArn)
          ]
      )

instance Data.ToPath DescribeStorageSystem where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeStorageSystem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeStorageSystemResponse' smart constructor.
data DescribeStorageSystemResponse = DescribeStorageSystemResponse'
  { -- | The ARN of the DataSync agent that connects to and reads from your
    -- on-premises storage system.
    agentArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The ARN of the Amazon CloudWatch log group that\'s used to monitor and
    -- log discovery job events.
    cloudWatchLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether your DataSync agent can connect to your on-premises
    -- storage system.
    connectivityStatus :: Prelude.Maybe StorageSystemConnectivityStatus,
    -- | The time when you added the on-premises storage system to DataSync
    -- Discovery.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Describes the connectivity error that the DataSync agent is encountering
    -- with your on-premises storage system.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | The name that you gave your on-premises storage system when adding it to
    -- DataSync Discovery.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the secret that stores your on-premises storage system\'s
    -- credentials. DataSync Discovery stores these credentials in
    -- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-configure-storage.html#discovery-add-storage Secrets Manager>.
    secretsManagerArn :: Prelude.Maybe Prelude.Text,
    -- | The server name and network port required to connect with your
    -- on-premises storage system\'s management interface.
    serverConfiguration :: Prelude.Maybe DiscoveryServerConfiguration,
    -- | The ARN of the on-premises storage system that the discovery job looked
    -- at.
    storageSystemArn :: Prelude.Maybe Prelude.Text,
    -- | The type of on-premises storage system.
    --
    -- DataSync Discovery currently only supports NetApp Fabric-Attached
    -- Storage (FAS) and All Flash FAS (AFF) systems running ONTAP 9.7 or
    -- later.
    systemType :: Prelude.Maybe DiscoverySystemType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeStorageSystemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentArns', 'describeStorageSystemResponse_agentArns' - The ARN of the DataSync agent that connects to and reads from your
-- on-premises storage system.
--
-- 'cloudWatchLogGroupArn', 'describeStorageSystemResponse_cloudWatchLogGroupArn' - The ARN of the Amazon CloudWatch log group that\'s used to monitor and
-- log discovery job events.
--
-- 'connectivityStatus', 'describeStorageSystemResponse_connectivityStatus' - Indicates whether your DataSync agent can connect to your on-premises
-- storage system.
--
-- 'creationTime', 'describeStorageSystemResponse_creationTime' - The time when you added the on-premises storage system to DataSync
-- Discovery.
--
-- 'errorMessage', 'describeStorageSystemResponse_errorMessage' - Describes the connectivity error that the DataSync agent is encountering
-- with your on-premises storage system.
--
-- 'name', 'describeStorageSystemResponse_name' - The name that you gave your on-premises storage system when adding it to
-- DataSync Discovery.
--
-- 'secretsManagerArn', 'describeStorageSystemResponse_secretsManagerArn' - The ARN of the secret that stores your on-premises storage system\'s
-- credentials. DataSync Discovery stores these credentials in
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-configure-storage.html#discovery-add-storage Secrets Manager>.
--
-- 'serverConfiguration', 'describeStorageSystemResponse_serverConfiguration' - The server name and network port required to connect with your
-- on-premises storage system\'s management interface.
--
-- 'storageSystemArn', 'describeStorageSystemResponse_storageSystemArn' - The ARN of the on-premises storage system that the discovery job looked
-- at.
--
-- 'systemType', 'describeStorageSystemResponse_systemType' - The type of on-premises storage system.
--
-- DataSync Discovery currently only supports NetApp Fabric-Attached
-- Storage (FAS) and All Flash FAS (AFF) systems running ONTAP 9.7 or
-- later.
--
-- 'httpStatus', 'describeStorageSystemResponse_httpStatus' - The response's http status code.
newDescribeStorageSystemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeStorageSystemResponse
newDescribeStorageSystemResponse pHttpStatus_ =
  DescribeStorageSystemResponse'
    { agentArns =
        Prelude.Nothing,
      cloudWatchLogGroupArn = Prelude.Nothing,
      connectivityStatus = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      name = Prelude.Nothing,
      secretsManagerArn = Prelude.Nothing,
      serverConfiguration = Prelude.Nothing,
      storageSystemArn = Prelude.Nothing,
      systemType = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the DataSync agent that connects to and reads from your
-- on-premises storage system.
describeStorageSystemResponse_agentArns :: Lens.Lens' DescribeStorageSystemResponse (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
describeStorageSystemResponse_agentArns = Lens.lens (\DescribeStorageSystemResponse' {agentArns} -> agentArns) (\s@DescribeStorageSystemResponse' {} a -> s {agentArns = a} :: DescribeStorageSystemResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the Amazon CloudWatch log group that\'s used to monitor and
-- log discovery job events.
describeStorageSystemResponse_cloudWatchLogGroupArn :: Lens.Lens' DescribeStorageSystemResponse (Prelude.Maybe Prelude.Text)
describeStorageSystemResponse_cloudWatchLogGroupArn = Lens.lens (\DescribeStorageSystemResponse' {cloudWatchLogGroupArn} -> cloudWatchLogGroupArn) (\s@DescribeStorageSystemResponse' {} a -> s {cloudWatchLogGroupArn = a} :: DescribeStorageSystemResponse)

-- | Indicates whether your DataSync agent can connect to your on-premises
-- storage system.
describeStorageSystemResponse_connectivityStatus :: Lens.Lens' DescribeStorageSystemResponse (Prelude.Maybe StorageSystemConnectivityStatus)
describeStorageSystemResponse_connectivityStatus = Lens.lens (\DescribeStorageSystemResponse' {connectivityStatus} -> connectivityStatus) (\s@DescribeStorageSystemResponse' {} a -> s {connectivityStatus = a} :: DescribeStorageSystemResponse)

-- | The time when you added the on-premises storage system to DataSync
-- Discovery.
describeStorageSystemResponse_creationTime :: Lens.Lens' DescribeStorageSystemResponse (Prelude.Maybe Prelude.UTCTime)
describeStorageSystemResponse_creationTime = Lens.lens (\DescribeStorageSystemResponse' {creationTime} -> creationTime) (\s@DescribeStorageSystemResponse' {} a -> s {creationTime = a} :: DescribeStorageSystemResponse) Prelude.. Lens.mapping Data._Time

-- | Describes the connectivity error that the DataSync agent is encountering
-- with your on-premises storage system.
describeStorageSystemResponse_errorMessage :: Lens.Lens' DescribeStorageSystemResponse (Prelude.Maybe Prelude.Text)
describeStorageSystemResponse_errorMessage = Lens.lens (\DescribeStorageSystemResponse' {errorMessage} -> errorMessage) (\s@DescribeStorageSystemResponse' {} a -> s {errorMessage = a} :: DescribeStorageSystemResponse)

-- | The name that you gave your on-premises storage system when adding it to
-- DataSync Discovery.
describeStorageSystemResponse_name :: Lens.Lens' DescribeStorageSystemResponse (Prelude.Maybe Prelude.Text)
describeStorageSystemResponse_name = Lens.lens (\DescribeStorageSystemResponse' {name} -> name) (\s@DescribeStorageSystemResponse' {} a -> s {name = a} :: DescribeStorageSystemResponse)

-- | The ARN of the secret that stores your on-premises storage system\'s
-- credentials. DataSync Discovery stores these credentials in
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-configure-storage.html#discovery-add-storage Secrets Manager>.
describeStorageSystemResponse_secretsManagerArn :: Lens.Lens' DescribeStorageSystemResponse (Prelude.Maybe Prelude.Text)
describeStorageSystemResponse_secretsManagerArn = Lens.lens (\DescribeStorageSystemResponse' {secretsManagerArn} -> secretsManagerArn) (\s@DescribeStorageSystemResponse' {} a -> s {secretsManagerArn = a} :: DescribeStorageSystemResponse)

-- | The server name and network port required to connect with your
-- on-premises storage system\'s management interface.
describeStorageSystemResponse_serverConfiguration :: Lens.Lens' DescribeStorageSystemResponse (Prelude.Maybe DiscoveryServerConfiguration)
describeStorageSystemResponse_serverConfiguration = Lens.lens (\DescribeStorageSystemResponse' {serverConfiguration} -> serverConfiguration) (\s@DescribeStorageSystemResponse' {} a -> s {serverConfiguration = a} :: DescribeStorageSystemResponse)

-- | The ARN of the on-premises storage system that the discovery job looked
-- at.
describeStorageSystemResponse_storageSystemArn :: Lens.Lens' DescribeStorageSystemResponse (Prelude.Maybe Prelude.Text)
describeStorageSystemResponse_storageSystemArn = Lens.lens (\DescribeStorageSystemResponse' {storageSystemArn} -> storageSystemArn) (\s@DescribeStorageSystemResponse' {} a -> s {storageSystemArn = a} :: DescribeStorageSystemResponse)

-- | The type of on-premises storage system.
--
-- DataSync Discovery currently only supports NetApp Fabric-Attached
-- Storage (FAS) and All Flash FAS (AFF) systems running ONTAP 9.7 or
-- later.
describeStorageSystemResponse_systemType :: Lens.Lens' DescribeStorageSystemResponse (Prelude.Maybe DiscoverySystemType)
describeStorageSystemResponse_systemType = Lens.lens (\DescribeStorageSystemResponse' {systemType} -> systemType) (\s@DescribeStorageSystemResponse' {} a -> s {systemType = a} :: DescribeStorageSystemResponse)

-- | The response's http status code.
describeStorageSystemResponse_httpStatus :: Lens.Lens' DescribeStorageSystemResponse Prelude.Int
describeStorageSystemResponse_httpStatus = Lens.lens (\DescribeStorageSystemResponse' {httpStatus} -> httpStatus) (\s@DescribeStorageSystemResponse' {} a -> s {httpStatus = a} :: DescribeStorageSystemResponse)

instance Prelude.NFData DescribeStorageSystemResponse where
  rnf DescribeStorageSystemResponse' {..} =
    Prelude.rnf agentArns
      `Prelude.seq` Prelude.rnf cloudWatchLogGroupArn
      `Prelude.seq` Prelude.rnf connectivityStatus
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf secretsManagerArn
      `Prelude.seq` Prelude.rnf serverConfiguration
      `Prelude.seq` Prelude.rnf storageSystemArn
      `Prelude.seq` Prelude.rnf systemType
      `Prelude.seq` Prelude.rnf httpStatus
