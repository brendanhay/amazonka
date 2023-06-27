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
-- Module      : Amazonka.DataSync.AddStorageSystem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Web Services resource for an on-premises storage
-- system that you want DataSync Discovery to collect information about.
module Amazonka.DataSync.AddStorageSystem
  ( -- * Creating a Request
    AddStorageSystem (..),
    newAddStorageSystem,

    -- * Request Lenses
    addStorageSystem_cloudWatchLogGroupArn,
    addStorageSystem_name,
    addStorageSystem_tags,
    addStorageSystem_serverConfiguration,
    addStorageSystem_systemType,
    addStorageSystem_agentArns,
    addStorageSystem_clientToken,
    addStorageSystem_credentials,

    -- * Destructuring the Response
    AddStorageSystemResponse (..),
    newAddStorageSystemResponse,

    -- * Response Lenses
    addStorageSystemResponse_httpStatus,
    addStorageSystemResponse_storageSystemArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAddStorageSystem' smart constructor.
data AddStorageSystem = AddStorageSystem'
  { -- | Specifies the ARN of the Amazon CloudWatch log group for monitoring and
    -- logging discovery job events.
    cloudWatchLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies a familiar name for your on-premises storage system.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies labels that help you categorize, filter, and search for your
    -- Amazon Web Services resources. We recommend creating at least a name tag
    -- for your on-premises storage system.
    tags :: Prelude.Maybe [TagListEntry],
    -- | Specifies the server name and network port required to connect with the
    -- management interface of your on-premises storage system.
    serverConfiguration :: DiscoveryServerConfiguration,
    -- | Specifies the type of on-premises storage system that you want DataSync
    -- Discovery to collect information about.
    --
    -- DataSync Discovery currently supports NetApp Fabric-Attached Storage
    -- (FAS) and All Flash FAS (AFF) systems running ONTAP 9.7 or later.
    systemType :: DiscoverySystemType,
    -- | Specifies the Amazon Resource Name (ARN) of the DataSync agent that
    -- connects to and reads from your on-premises storage system\'s management
    -- interface.
    agentArns :: Prelude.NonEmpty Prelude.Text,
    -- | Specifies a client token to make sure requests with this API operation
    -- are idempotent. If you don\'t specify a client token, DataSync generates
    -- one for you automatically.
    clientToken :: Prelude.Text,
    -- | Specifies the user name and password for accessing your on-premises
    -- storage system\'s management interface.
    credentials :: Credentials
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddStorageSystem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudWatchLogGroupArn', 'addStorageSystem_cloudWatchLogGroupArn' - Specifies the ARN of the Amazon CloudWatch log group for monitoring and
-- logging discovery job events.
--
-- 'name', 'addStorageSystem_name' - Specifies a familiar name for your on-premises storage system.
--
-- 'tags', 'addStorageSystem_tags' - Specifies labels that help you categorize, filter, and search for your
-- Amazon Web Services resources. We recommend creating at least a name tag
-- for your on-premises storage system.
--
-- 'serverConfiguration', 'addStorageSystem_serverConfiguration' - Specifies the server name and network port required to connect with the
-- management interface of your on-premises storage system.
--
-- 'systemType', 'addStorageSystem_systemType' - Specifies the type of on-premises storage system that you want DataSync
-- Discovery to collect information about.
--
-- DataSync Discovery currently supports NetApp Fabric-Attached Storage
-- (FAS) and All Flash FAS (AFF) systems running ONTAP 9.7 or later.
--
-- 'agentArns', 'addStorageSystem_agentArns' - Specifies the Amazon Resource Name (ARN) of the DataSync agent that
-- connects to and reads from your on-premises storage system\'s management
-- interface.
--
-- 'clientToken', 'addStorageSystem_clientToken' - Specifies a client token to make sure requests with this API operation
-- are idempotent. If you don\'t specify a client token, DataSync generates
-- one for you automatically.
--
-- 'credentials', 'addStorageSystem_credentials' - Specifies the user name and password for accessing your on-premises
-- storage system\'s management interface.
newAddStorageSystem ::
  -- | 'serverConfiguration'
  DiscoveryServerConfiguration ->
  -- | 'systemType'
  DiscoverySystemType ->
  -- | 'agentArns'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'clientToken'
  Prelude.Text ->
  -- | 'credentials'
  Credentials ->
  AddStorageSystem
newAddStorageSystem
  pServerConfiguration_
  pSystemType_
  pAgentArns_
  pClientToken_
  pCredentials_ =
    AddStorageSystem'
      { cloudWatchLogGroupArn =
          Prelude.Nothing,
        name = Prelude.Nothing,
        tags = Prelude.Nothing,
        serverConfiguration = pServerConfiguration_,
        systemType = pSystemType_,
        agentArns = Lens.coerced Lens.# pAgentArns_,
        clientToken = pClientToken_,
        credentials = pCredentials_
      }

-- | Specifies the ARN of the Amazon CloudWatch log group for monitoring and
-- logging discovery job events.
addStorageSystem_cloudWatchLogGroupArn :: Lens.Lens' AddStorageSystem (Prelude.Maybe Prelude.Text)
addStorageSystem_cloudWatchLogGroupArn = Lens.lens (\AddStorageSystem' {cloudWatchLogGroupArn} -> cloudWatchLogGroupArn) (\s@AddStorageSystem' {} a -> s {cloudWatchLogGroupArn = a} :: AddStorageSystem)

-- | Specifies a familiar name for your on-premises storage system.
addStorageSystem_name :: Lens.Lens' AddStorageSystem (Prelude.Maybe Prelude.Text)
addStorageSystem_name = Lens.lens (\AddStorageSystem' {name} -> name) (\s@AddStorageSystem' {} a -> s {name = a} :: AddStorageSystem)

-- | Specifies labels that help you categorize, filter, and search for your
-- Amazon Web Services resources. We recommend creating at least a name tag
-- for your on-premises storage system.
addStorageSystem_tags :: Lens.Lens' AddStorageSystem (Prelude.Maybe [TagListEntry])
addStorageSystem_tags = Lens.lens (\AddStorageSystem' {tags} -> tags) (\s@AddStorageSystem' {} a -> s {tags = a} :: AddStorageSystem) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the server name and network port required to connect with the
-- management interface of your on-premises storage system.
addStorageSystem_serverConfiguration :: Lens.Lens' AddStorageSystem DiscoveryServerConfiguration
addStorageSystem_serverConfiguration = Lens.lens (\AddStorageSystem' {serverConfiguration} -> serverConfiguration) (\s@AddStorageSystem' {} a -> s {serverConfiguration = a} :: AddStorageSystem)

-- | Specifies the type of on-premises storage system that you want DataSync
-- Discovery to collect information about.
--
-- DataSync Discovery currently supports NetApp Fabric-Attached Storage
-- (FAS) and All Flash FAS (AFF) systems running ONTAP 9.7 or later.
addStorageSystem_systemType :: Lens.Lens' AddStorageSystem DiscoverySystemType
addStorageSystem_systemType = Lens.lens (\AddStorageSystem' {systemType} -> systemType) (\s@AddStorageSystem' {} a -> s {systemType = a} :: AddStorageSystem)

-- | Specifies the Amazon Resource Name (ARN) of the DataSync agent that
-- connects to and reads from your on-premises storage system\'s management
-- interface.
addStorageSystem_agentArns :: Lens.Lens' AddStorageSystem (Prelude.NonEmpty Prelude.Text)
addStorageSystem_agentArns = Lens.lens (\AddStorageSystem' {agentArns} -> agentArns) (\s@AddStorageSystem' {} a -> s {agentArns = a} :: AddStorageSystem) Prelude.. Lens.coerced

-- | Specifies a client token to make sure requests with this API operation
-- are idempotent. If you don\'t specify a client token, DataSync generates
-- one for you automatically.
addStorageSystem_clientToken :: Lens.Lens' AddStorageSystem Prelude.Text
addStorageSystem_clientToken = Lens.lens (\AddStorageSystem' {clientToken} -> clientToken) (\s@AddStorageSystem' {} a -> s {clientToken = a} :: AddStorageSystem)

-- | Specifies the user name and password for accessing your on-premises
-- storage system\'s management interface.
addStorageSystem_credentials :: Lens.Lens' AddStorageSystem Credentials
addStorageSystem_credentials = Lens.lens (\AddStorageSystem' {credentials} -> credentials) (\s@AddStorageSystem' {} a -> s {credentials = a} :: AddStorageSystem)

instance Core.AWSRequest AddStorageSystem where
  type
    AWSResponse AddStorageSystem =
      AddStorageSystemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AddStorageSystemResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "StorageSystemArn")
      )

instance Prelude.Hashable AddStorageSystem where
  hashWithSalt _salt AddStorageSystem' {..} =
    _salt
      `Prelude.hashWithSalt` cloudWatchLogGroupArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` serverConfiguration
      `Prelude.hashWithSalt` systemType
      `Prelude.hashWithSalt` agentArns
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` credentials

instance Prelude.NFData AddStorageSystem where
  rnf AddStorageSystem' {..} =
    Prelude.rnf cloudWatchLogGroupArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf serverConfiguration
      `Prelude.seq` Prelude.rnf systemType
      `Prelude.seq` Prelude.rnf agentArns
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf credentials

instance Data.ToHeaders AddStorageSystem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.AddStorageSystem" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AddStorageSystem where
  toJSON AddStorageSystem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CloudWatchLogGroupArn" Data..=)
              Prelude.<$> cloudWatchLogGroupArn,
            ("Name" Data..=) Prelude.<$> name,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just
              ("ServerConfiguration" Data..= serverConfiguration),
            Prelude.Just ("SystemType" Data..= systemType),
            Prelude.Just ("AgentArns" Data..= agentArns),
            Prelude.Just ("ClientToken" Data..= clientToken),
            Prelude.Just ("Credentials" Data..= credentials)
          ]
      )

instance Data.ToPath AddStorageSystem where
  toPath = Prelude.const "/"

instance Data.ToQuery AddStorageSystem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAddStorageSystemResponse' smart constructor.
data AddStorageSystemResponse = AddStorageSystemResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the on-premises storage system that you can use with DataSync
    -- Discovery.
    storageSystemArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AddStorageSystemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'addStorageSystemResponse_httpStatus' - The response's http status code.
--
-- 'storageSystemArn', 'addStorageSystemResponse_storageSystemArn' - The ARN of the on-premises storage system that you can use with DataSync
-- Discovery.
newAddStorageSystemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'storageSystemArn'
  Prelude.Text ->
  AddStorageSystemResponse
newAddStorageSystemResponse
  pHttpStatus_
  pStorageSystemArn_ =
    AddStorageSystemResponse'
      { httpStatus =
          pHttpStatus_,
        storageSystemArn = pStorageSystemArn_
      }

-- | The response's http status code.
addStorageSystemResponse_httpStatus :: Lens.Lens' AddStorageSystemResponse Prelude.Int
addStorageSystemResponse_httpStatus = Lens.lens (\AddStorageSystemResponse' {httpStatus} -> httpStatus) (\s@AddStorageSystemResponse' {} a -> s {httpStatus = a} :: AddStorageSystemResponse)

-- | The ARN of the on-premises storage system that you can use with DataSync
-- Discovery.
addStorageSystemResponse_storageSystemArn :: Lens.Lens' AddStorageSystemResponse Prelude.Text
addStorageSystemResponse_storageSystemArn = Lens.lens (\AddStorageSystemResponse' {storageSystemArn} -> storageSystemArn) (\s@AddStorageSystemResponse' {} a -> s {storageSystemArn = a} :: AddStorageSystemResponse)

instance Prelude.NFData AddStorageSystemResponse where
  rnf AddStorageSystemResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf storageSystemArn
