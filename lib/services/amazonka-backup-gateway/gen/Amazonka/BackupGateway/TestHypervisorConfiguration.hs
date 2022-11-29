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
-- Module      : Amazonka.BackupGateway.TestHypervisorConfiguration
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Tests your hypervisor configuration to validate that backup gateway can
-- connect with the hypervisor and its resources.
module Amazonka.BackupGateway.TestHypervisorConfiguration
  ( -- * Creating a Request
    TestHypervisorConfiguration (..),
    newTestHypervisorConfiguration,

    -- * Request Lenses
    testHypervisorConfiguration_password,
    testHypervisorConfiguration_username,
    testHypervisorConfiguration_gatewayArn,
    testHypervisorConfiguration_host,

    -- * Destructuring the Response
    TestHypervisorConfigurationResponse (..),
    newTestHypervisorConfigurationResponse,

    -- * Response Lenses
    testHypervisorConfigurationResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newTestHypervisorConfiguration' smart constructor.
data TestHypervisorConfiguration = TestHypervisorConfiguration'
  { -- | The password for the hypervisor.
    password :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The username for the hypervisor.
    username :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the gateway to the hypervisor to test.
    gatewayArn :: Prelude.Text,
    -- | The server host of the hypervisor. This can be either an IP address or a
    -- fully-qualified domain name (FQDN).
    host :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestHypervisorConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'password', 'testHypervisorConfiguration_password' - The password for the hypervisor.
--
-- 'username', 'testHypervisorConfiguration_username' - The username for the hypervisor.
--
-- 'gatewayArn', 'testHypervisorConfiguration_gatewayArn' - The Amazon Resource Name (ARN) of the gateway to the hypervisor to test.
--
-- 'host', 'testHypervisorConfiguration_host' - The server host of the hypervisor. This can be either an IP address or a
-- fully-qualified domain name (FQDN).
newTestHypervisorConfiguration ::
  -- | 'gatewayArn'
  Prelude.Text ->
  -- | 'host'
  Prelude.Text ->
  TestHypervisorConfiguration
newTestHypervisorConfiguration pGatewayArn_ pHost_ =
  TestHypervisorConfiguration'
    { password =
        Prelude.Nothing,
      username = Prelude.Nothing,
      gatewayArn = pGatewayArn_,
      host = pHost_
    }

-- | The password for the hypervisor.
testHypervisorConfiguration_password :: Lens.Lens' TestHypervisorConfiguration (Prelude.Maybe Prelude.Text)
testHypervisorConfiguration_password = Lens.lens (\TestHypervisorConfiguration' {password} -> password) (\s@TestHypervisorConfiguration' {} a -> s {password = a} :: TestHypervisorConfiguration) Prelude.. Lens.mapping Core._Sensitive

-- | The username for the hypervisor.
testHypervisorConfiguration_username :: Lens.Lens' TestHypervisorConfiguration (Prelude.Maybe Prelude.Text)
testHypervisorConfiguration_username = Lens.lens (\TestHypervisorConfiguration' {username} -> username) (\s@TestHypervisorConfiguration' {} a -> s {username = a} :: TestHypervisorConfiguration) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Resource Name (ARN) of the gateway to the hypervisor to test.
testHypervisorConfiguration_gatewayArn :: Lens.Lens' TestHypervisorConfiguration Prelude.Text
testHypervisorConfiguration_gatewayArn = Lens.lens (\TestHypervisorConfiguration' {gatewayArn} -> gatewayArn) (\s@TestHypervisorConfiguration' {} a -> s {gatewayArn = a} :: TestHypervisorConfiguration)

-- | The server host of the hypervisor. This can be either an IP address or a
-- fully-qualified domain name (FQDN).
testHypervisorConfiguration_host :: Lens.Lens' TestHypervisorConfiguration Prelude.Text
testHypervisorConfiguration_host = Lens.lens (\TestHypervisorConfiguration' {host} -> host) (\s@TestHypervisorConfiguration' {} a -> s {host = a} :: TestHypervisorConfiguration)

instance Core.AWSRequest TestHypervisorConfiguration where
  type
    AWSResponse TestHypervisorConfiguration =
      TestHypervisorConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          TestHypervisorConfigurationResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable TestHypervisorConfiguration where
  hashWithSalt _salt TestHypervisorConfiguration' {..} =
    _salt `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` gatewayArn
      `Prelude.hashWithSalt` host

instance Prelude.NFData TestHypervisorConfiguration where
  rnf TestHypervisorConfiguration' {..} =
    Prelude.rnf password
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf gatewayArn
      `Prelude.seq` Prelude.rnf host

instance Core.ToHeaders TestHypervisorConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "BackupOnPremises_v20210101.TestHypervisorConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON TestHypervisorConfiguration where
  toJSON TestHypervisorConfiguration' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Password" Core..=) Prelude.<$> password,
            ("Username" Core..=) Prelude.<$> username,
            Prelude.Just ("GatewayArn" Core..= gatewayArn),
            Prelude.Just ("Host" Core..= host)
          ]
      )

instance Core.ToPath TestHypervisorConfiguration where
  toPath = Prelude.const "/"

instance Core.ToQuery TestHypervisorConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newTestHypervisorConfigurationResponse' smart constructor.
data TestHypervisorConfigurationResponse = TestHypervisorConfigurationResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TestHypervisorConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'testHypervisorConfigurationResponse_httpStatus' - The response's http status code.
newTestHypervisorConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  TestHypervisorConfigurationResponse
newTestHypervisorConfigurationResponse pHttpStatus_ =
  TestHypervisorConfigurationResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
testHypervisorConfigurationResponse_httpStatus :: Lens.Lens' TestHypervisorConfigurationResponse Prelude.Int
testHypervisorConfigurationResponse_httpStatus = Lens.lens (\TestHypervisorConfigurationResponse' {httpStatus} -> httpStatus) (\s@TestHypervisorConfigurationResponse' {} a -> s {httpStatus = a} :: TestHypervisorConfigurationResponse)

instance
  Prelude.NFData
    TestHypervisorConfigurationResponse
  where
  rnf TestHypervisorConfigurationResponse' {..} =
    Prelude.rnf httpStatus
