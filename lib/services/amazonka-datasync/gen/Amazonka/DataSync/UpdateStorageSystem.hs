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
-- Module      : Amazonka.DataSync.UpdateStorageSystem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies some configurations of an on-premises storage system resource
-- that you\'re using with DataSync Discovery.
module Amazonka.DataSync.UpdateStorageSystem
  ( -- * Creating a Request
    UpdateStorageSystem (..),
    newUpdateStorageSystem,

    -- * Request Lenses
    updateStorageSystem_agentArns,
    updateStorageSystem_cloudWatchLogGroupArn,
    updateStorageSystem_credentials,
    updateStorageSystem_name,
    updateStorageSystem_serverConfiguration,
    updateStorageSystem_storageSystemArn,

    -- * Destructuring the Response
    UpdateStorageSystemResponse (..),
    newUpdateStorageSystemResponse,

    -- * Response Lenses
    updateStorageSystemResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataSync.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateStorageSystem' smart constructor.
data UpdateStorageSystem = UpdateStorageSystem'
  { -- | Specifies the Amazon Resource Name (ARN) of the DataSync agent that
    -- connects to and reads your on-premises storage system.
    agentArns :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | Specifies the ARN of the Amazon CloudWatch log group for monitoring and
    -- logging discovery job events.
    cloudWatchLogGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Specifies the user name and password for accessing your on-premises
    -- storage system\'s management interface.
    credentials :: Prelude.Maybe Credentials,
    -- | Specifies a familiar name for your on-premises storage system.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies the server name and network port required to connect with your
    -- on-premises storage system\'s management interface.
    serverConfiguration :: Prelude.Maybe DiscoveryServerConfiguration,
    -- | Specifies the ARN of the on-premises storage system that you want
    -- reconfigure.
    storageSystemArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStorageSystem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'agentArns', 'updateStorageSystem_agentArns' - Specifies the Amazon Resource Name (ARN) of the DataSync agent that
-- connects to and reads your on-premises storage system.
--
-- 'cloudWatchLogGroupArn', 'updateStorageSystem_cloudWatchLogGroupArn' - Specifies the ARN of the Amazon CloudWatch log group for monitoring and
-- logging discovery job events.
--
-- 'credentials', 'updateStorageSystem_credentials' - Specifies the user name and password for accessing your on-premises
-- storage system\'s management interface.
--
-- 'name', 'updateStorageSystem_name' - Specifies a familiar name for your on-premises storage system.
--
-- 'serverConfiguration', 'updateStorageSystem_serverConfiguration' - Specifies the server name and network port required to connect with your
-- on-premises storage system\'s management interface.
--
-- 'storageSystemArn', 'updateStorageSystem_storageSystemArn' - Specifies the ARN of the on-premises storage system that you want
-- reconfigure.
newUpdateStorageSystem ::
  -- | 'storageSystemArn'
  Prelude.Text ->
  UpdateStorageSystem
newUpdateStorageSystem pStorageSystemArn_ =
  UpdateStorageSystem'
    { agentArns = Prelude.Nothing,
      cloudWatchLogGroupArn = Prelude.Nothing,
      credentials = Prelude.Nothing,
      name = Prelude.Nothing,
      serverConfiguration = Prelude.Nothing,
      storageSystemArn = pStorageSystemArn_
    }

-- | Specifies the Amazon Resource Name (ARN) of the DataSync agent that
-- connects to and reads your on-premises storage system.
updateStorageSystem_agentArns :: Lens.Lens' UpdateStorageSystem (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
updateStorageSystem_agentArns = Lens.lens (\UpdateStorageSystem' {agentArns} -> agentArns) (\s@UpdateStorageSystem' {} a -> s {agentArns = a} :: UpdateStorageSystem) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the ARN of the Amazon CloudWatch log group for monitoring and
-- logging discovery job events.
updateStorageSystem_cloudWatchLogGroupArn :: Lens.Lens' UpdateStorageSystem (Prelude.Maybe Prelude.Text)
updateStorageSystem_cloudWatchLogGroupArn = Lens.lens (\UpdateStorageSystem' {cloudWatchLogGroupArn} -> cloudWatchLogGroupArn) (\s@UpdateStorageSystem' {} a -> s {cloudWatchLogGroupArn = a} :: UpdateStorageSystem)

-- | Specifies the user name and password for accessing your on-premises
-- storage system\'s management interface.
updateStorageSystem_credentials :: Lens.Lens' UpdateStorageSystem (Prelude.Maybe Credentials)
updateStorageSystem_credentials = Lens.lens (\UpdateStorageSystem' {credentials} -> credentials) (\s@UpdateStorageSystem' {} a -> s {credentials = a} :: UpdateStorageSystem)

-- | Specifies a familiar name for your on-premises storage system.
updateStorageSystem_name :: Lens.Lens' UpdateStorageSystem (Prelude.Maybe Prelude.Text)
updateStorageSystem_name = Lens.lens (\UpdateStorageSystem' {name} -> name) (\s@UpdateStorageSystem' {} a -> s {name = a} :: UpdateStorageSystem)

-- | Specifies the server name and network port required to connect with your
-- on-premises storage system\'s management interface.
updateStorageSystem_serverConfiguration :: Lens.Lens' UpdateStorageSystem (Prelude.Maybe DiscoveryServerConfiguration)
updateStorageSystem_serverConfiguration = Lens.lens (\UpdateStorageSystem' {serverConfiguration} -> serverConfiguration) (\s@UpdateStorageSystem' {} a -> s {serverConfiguration = a} :: UpdateStorageSystem)

-- | Specifies the ARN of the on-premises storage system that you want
-- reconfigure.
updateStorageSystem_storageSystemArn :: Lens.Lens' UpdateStorageSystem Prelude.Text
updateStorageSystem_storageSystemArn = Lens.lens (\UpdateStorageSystem' {storageSystemArn} -> storageSystemArn) (\s@UpdateStorageSystem' {} a -> s {storageSystemArn = a} :: UpdateStorageSystem)

instance Core.AWSRequest UpdateStorageSystem where
  type
    AWSResponse UpdateStorageSystem =
      UpdateStorageSystemResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateStorageSystemResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateStorageSystem where
  hashWithSalt _salt UpdateStorageSystem' {..} =
    _salt
      `Prelude.hashWithSalt` agentArns
      `Prelude.hashWithSalt` cloudWatchLogGroupArn
      `Prelude.hashWithSalt` credentials
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` serverConfiguration
      `Prelude.hashWithSalt` storageSystemArn

instance Prelude.NFData UpdateStorageSystem where
  rnf UpdateStorageSystem' {..} =
    Prelude.rnf agentArns
      `Prelude.seq` Prelude.rnf cloudWatchLogGroupArn
      `Prelude.seq` Prelude.rnf credentials
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf serverConfiguration
      `Prelude.seq` Prelude.rnf storageSystemArn

instance Data.ToHeaders UpdateStorageSystem where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "FmrsService.UpdateStorageSystem" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateStorageSystem where
  toJSON UpdateStorageSystem' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AgentArns" Data..=) Prelude.<$> agentArns,
            ("CloudWatchLogGroupArn" Data..=)
              Prelude.<$> cloudWatchLogGroupArn,
            ("Credentials" Data..=) Prelude.<$> credentials,
            ("Name" Data..=) Prelude.<$> name,
            ("ServerConfiguration" Data..=)
              Prelude.<$> serverConfiguration,
            Prelude.Just
              ("StorageSystemArn" Data..= storageSystemArn)
          ]
      )

instance Data.ToPath UpdateStorageSystem where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateStorageSystem where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStorageSystemResponse' smart constructor.
data UpdateStorageSystemResponse = UpdateStorageSystemResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStorageSystemResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateStorageSystemResponse_httpStatus' - The response's http status code.
newUpdateStorageSystemResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateStorageSystemResponse
newUpdateStorageSystemResponse pHttpStatus_ =
  UpdateStorageSystemResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateStorageSystemResponse_httpStatus :: Lens.Lens' UpdateStorageSystemResponse Prelude.Int
updateStorageSystemResponse_httpStatus = Lens.lens (\UpdateStorageSystemResponse' {httpStatus} -> httpStatus) (\s@UpdateStorageSystemResponse' {} a -> s {httpStatus = a} :: UpdateStorageSystemResponse)

instance Prelude.NFData UpdateStorageSystemResponse where
  rnf UpdateStorageSystemResponse' {..} =
    Prelude.rnf httpStatus
