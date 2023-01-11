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
-- Module      : Amazonka.BackupGateway.ImportHypervisorConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Connect to a hypervisor by importing its configuration.
module Amazonka.BackupGateway.ImportHypervisorConfiguration
  ( -- * Creating a Request
    ImportHypervisorConfiguration (..),
    newImportHypervisorConfiguration,

    -- * Request Lenses
    importHypervisorConfiguration_kmsKeyArn,
    importHypervisorConfiguration_password,
    importHypervisorConfiguration_tags,
    importHypervisorConfiguration_username,
    importHypervisorConfiguration_host,
    importHypervisorConfiguration_name,

    -- * Destructuring the Response
    ImportHypervisorConfigurationResponse (..),
    newImportHypervisorConfigurationResponse,

    -- * Response Lenses
    importHypervisorConfigurationResponse_hypervisorArn,
    importHypervisorConfigurationResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newImportHypervisorConfiguration' smart constructor.
data ImportHypervisorConfiguration = ImportHypervisorConfiguration'
  { -- | The Key Management Service for the hypervisor.
    kmsKeyArn :: Prelude.Maybe Prelude.Text,
    -- | The password for the hypervisor.
    password :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The tags of the hypervisor configuration to import.
    tags :: Prelude.Maybe [Tag],
    -- | The username for the hypervisor.
    username :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The server host of the hypervisor. This can be either an IP address or a
    -- fully-qualified domain name (FQDN).
    host :: Prelude.Text,
    -- | The name of the hypervisor.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportHypervisorConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'kmsKeyArn', 'importHypervisorConfiguration_kmsKeyArn' - The Key Management Service for the hypervisor.
--
-- 'password', 'importHypervisorConfiguration_password' - The password for the hypervisor.
--
-- 'tags', 'importHypervisorConfiguration_tags' - The tags of the hypervisor configuration to import.
--
-- 'username', 'importHypervisorConfiguration_username' - The username for the hypervisor.
--
-- 'host', 'importHypervisorConfiguration_host' - The server host of the hypervisor. This can be either an IP address or a
-- fully-qualified domain name (FQDN).
--
-- 'name', 'importHypervisorConfiguration_name' - The name of the hypervisor.
newImportHypervisorConfiguration ::
  -- | 'host'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  ImportHypervisorConfiguration
newImportHypervisorConfiguration pHost_ pName_ =
  ImportHypervisorConfiguration'
    { kmsKeyArn =
        Prelude.Nothing,
      password = Prelude.Nothing,
      tags = Prelude.Nothing,
      username = Prelude.Nothing,
      host = pHost_,
      name = pName_
    }

-- | The Key Management Service for the hypervisor.
importHypervisorConfiguration_kmsKeyArn :: Lens.Lens' ImportHypervisorConfiguration (Prelude.Maybe Prelude.Text)
importHypervisorConfiguration_kmsKeyArn = Lens.lens (\ImportHypervisorConfiguration' {kmsKeyArn} -> kmsKeyArn) (\s@ImportHypervisorConfiguration' {} a -> s {kmsKeyArn = a} :: ImportHypervisorConfiguration)

-- | The password for the hypervisor.
importHypervisorConfiguration_password :: Lens.Lens' ImportHypervisorConfiguration (Prelude.Maybe Prelude.Text)
importHypervisorConfiguration_password = Lens.lens (\ImportHypervisorConfiguration' {password} -> password) (\s@ImportHypervisorConfiguration' {} a -> s {password = a} :: ImportHypervisorConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The tags of the hypervisor configuration to import.
importHypervisorConfiguration_tags :: Lens.Lens' ImportHypervisorConfiguration (Prelude.Maybe [Tag])
importHypervisorConfiguration_tags = Lens.lens (\ImportHypervisorConfiguration' {tags} -> tags) (\s@ImportHypervisorConfiguration' {} a -> s {tags = a} :: ImportHypervisorConfiguration) Prelude.. Lens.mapping Lens.coerced

-- | The username for the hypervisor.
importHypervisorConfiguration_username :: Lens.Lens' ImportHypervisorConfiguration (Prelude.Maybe Prelude.Text)
importHypervisorConfiguration_username = Lens.lens (\ImportHypervisorConfiguration' {username} -> username) (\s@ImportHypervisorConfiguration' {} a -> s {username = a} :: ImportHypervisorConfiguration) Prelude.. Lens.mapping Data._Sensitive

-- | The server host of the hypervisor. This can be either an IP address or a
-- fully-qualified domain name (FQDN).
importHypervisorConfiguration_host :: Lens.Lens' ImportHypervisorConfiguration Prelude.Text
importHypervisorConfiguration_host = Lens.lens (\ImportHypervisorConfiguration' {host} -> host) (\s@ImportHypervisorConfiguration' {} a -> s {host = a} :: ImportHypervisorConfiguration)

-- | The name of the hypervisor.
importHypervisorConfiguration_name :: Lens.Lens' ImportHypervisorConfiguration Prelude.Text
importHypervisorConfiguration_name = Lens.lens (\ImportHypervisorConfiguration' {name} -> name) (\s@ImportHypervisorConfiguration' {} a -> s {name = a} :: ImportHypervisorConfiguration)

instance
  Core.AWSRequest
    ImportHypervisorConfiguration
  where
  type
    AWSResponse ImportHypervisorConfiguration =
      ImportHypervisorConfigurationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ImportHypervisorConfigurationResponse'
            Prelude.<$> (x Data..?> "HypervisorArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ImportHypervisorConfiguration
  where
  hashWithSalt _salt ImportHypervisorConfiguration' {..} =
    _salt `Prelude.hashWithSalt` kmsKeyArn
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` name

instance Prelude.NFData ImportHypervisorConfiguration where
  rnf ImportHypervisorConfiguration' {..} =
    Prelude.rnf kmsKeyArn
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf host
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders ImportHypervisorConfiguration where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BackupOnPremises_v20210101.ImportHypervisorConfiguration" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ImportHypervisorConfiguration where
  toJSON ImportHypervisorConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("KmsKeyArn" Data..=) Prelude.<$> kmsKeyArn,
            ("Password" Data..=) Prelude.<$> password,
            ("Tags" Data..=) Prelude.<$> tags,
            ("Username" Data..=) Prelude.<$> username,
            Prelude.Just ("Host" Data..= host),
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath ImportHypervisorConfiguration where
  toPath = Prelude.const "/"

instance Data.ToQuery ImportHypervisorConfiguration where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newImportHypervisorConfigurationResponse' smart constructor.
data ImportHypervisorConfigurationResponse = ImportHypervisorConfigurationResponse'
  { -- | The Amazon Resource Name (ARN) of the hypervisor you disassociated.
    hypervisorArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImportHypervisorConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hypervisorArn', 'importHypervisorConfigurationResponse_hypervisorArn' - The Amazon Resource Name (ARN) of the hypervisor you disassociated.
--
-- 'httpStatus', 'importHypervisorConfigurationResponse_httpStatus' - The response's http status code.
newImportHypervisorConfigurationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ImportHypervisorConfigurationResponse
newImportHypervisorConfigurationResponse pHttpStatus_ =
  ImportHypervisorConfigurationResponse'
    { hypervisorArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the hypervisor you disassociated.
importHypervisorConfigurationResponse_hypervisorArn :: Lens.Lens' ImportHypervisorConfigurationResponse (Prelude.Maybe Prelude.Text)
importHypervisorConfigurationResponse_hypervisorArn = Lens.lens (\ImportHypervisorConfigurationResponse' {hypervisorArn} -> hypervisorArn) (\s@ImportHypervisorConfigurationResponse' {} a -> s {hypervisorArn = a} :: ImportHypervisorConfigurationResponse)

-- | The response's http status code.
importHypervisorConfigurationResponse_httpStatus :: Lens.Lens' ImportHypervisorConfigurationResponse Prelude.Int
importHypervisorConfigurationResponse_httpStatus = Lens.lens (\ImportHypervisorConfigurationResponse' {httpStatus} -> httpStatus) (\s@ImportHypervisorConfigurationResponse' {} a -> s {httpStatus = a} :: ImportHypervisorConfigurationResponse)

instance
  Prelude.NFData
    ImportHypervisorConfigurationResponse
  where
  rnf ImportHypervisorConfigurationResponse' {..} =
    Prelude.rnf hypervisorArn
      `Prelude.seq` Prelude.rnf httpStatus
