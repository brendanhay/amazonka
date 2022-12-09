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
-- Module      : Amazonka.BackupGateway.UpdateHypervisor
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a hypervisor metadata, including its host, username, and
-- password. Specify which hypervisor to update using the Amazon Resource
-- Name (ARN) of the hypervisor in your request.
module Amazonka.BackupGateway.UpdateHypervisor
  ( -- * Creating a Request
    UpdateHypervisor (..),
    newUpdateHypervisor,

    -- * Request Lenses
    updateHypervisor_host,
    updateHypervisor_name,
    updateHypervisor_password,
    updateHypervisor_username,
    updateHypervisor_hypervisorArn,

    -- * Destructuring the Response
    UpdateHypervisorResponse (..),
    newUpdateHypervisorResponse,

    -- * Response Lenses
    updateHypervisorResponse_hypervisorArn,
    updateHypervisorResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateHypervisor' smart constructor.
data UpdateHypervisor = UpdateHypervisor'
  { -- | The updated host of the hypervisor. This can be either an IP address or
    -- a fully-qualified domain name (FQDN).
    host :: Prelude.Maybe Prelude.Text,
    -- | The updated name for the hypervisor
    name :: Prelude.Maybe Prelude.Text,
    -- | The updated password for the hypervisor.
    password :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The updated username for the hypervisor.
    username :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the hypervisor to update.
    hypervisorArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateHypervisor' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'host', 'updateHypervisor_host' - The updated host of the hypervisor. This can be either an IP address or
-- a fully-qualified domain name (FQDN).
--
-- 'name', 'updateHypervisor_name' - The updated name for the hypervisor
--
-- 'password', 'updateHypervisor_password' - The updated password for the hypervisor.
--
-- 'username', 'updateHypervisor_username' - The updated username for the hypervisor.
--
-- 'hypervisorArn', 'updateHypervisor_hypervisorArn' - The Amazon Resource Name (ARN) of the hypervisor to update.
newUpdateHypervisor ::
  -- | 'hypervisorArn'
  Prelude.Text ->
  UpdateHypervisor
newUpdateHypervisor pHypervisorArn_ =
  UpdateHypervisor'
    { host = Prelude.Nothing,
      name = Prelude.Nothing,
      password = Prelude.Nothing,
      username = Prelude.Nothing,
      hypervisorArn = pHypervisorArn_
    }

-- | The updated host of the hypervisor. This can be either an IP address or
-- a fully-qualified domain name (FQDN).
updateHypervisor_host :: Lens.Lens' UpdateHypervisor (Prelude.Maybe Prelude.Text)
updateHypervisor_host = Lens.lens (\UpdateHypervisor' {host} -> host) (\s@UpdateHypervisor' {} a -> s {host = a} :: UpdateHypervisor)

-- | The updated name for the hypervisor
updateHypervisor_name :: Lens.Lens' UpdateHypervisor (Prelude.Maybe Prelude.Text)
updateHypervisor_name = Lens.lens (\UpdateHypervisor' {name} -> name) (\s@UpdateHypervisor' {} a -> s {name = a} :: UpdateHypervisor)

-- | The updated password for the hypervisor.
updateHypervisor_password :: Lens.Lens' UpdateHypervisor (Prelude.Maybe Prelude.Text)
updateHypervisor_password = Lens.lens (\UpdateHypervisor' {password} -> password) (\s@UpdateHypervisor' {} a -> s {password = a} :: UpdateHypervisor) Prelude.. Lens.mapping Data._Sensitive

-- | The updated username for the hypervisor.
updateHypervisor_username :: Lens.Lens' UpdateHypervisor (Prelude.Maybe Prelude.Text)
updateHypervisor_username = Lens.lens (\UpdateHypervisor' {username} -> username) (\s@UpdateHypervisor' {} a -> s {username = a} :: UpdateHypervisor) Prelude.. Lens.mapping Data._Sensitive

-- | The Amazon Resource Name (ARN) of the hypervisor to update.
updateHypervisor_hypervisorArn :: Lens.Lens' UpdateHypervisor Prelude.Text
updateHypervisor_hypervisorArn = Lens.lens (\UpdateHypervisor' {hypervisorArn} -> hypervisorArn) (\s@UpdateHypervisor' {} a -> s {hypervisorArn = a} :: UpdateHypervisor)

instance Core.AWSRequest UpdateHypervisor where
  type
    AWSResponse UpdateHypervisor =
      UpdateHypervisorResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateHypervisorResponse'
            Prelude.<$> (x Data..?> "HypervisorArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateHypervisor where
  hashWithSalt _salt UpdateHypervisor' {..} =
    _salt `Prelude.hashWithSalt` host
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` password
      `Prelude.hashWithSalt` username
      `Prelude.hashWithSalt` hypervisorArn

instance Prelude.NFData UpdateHypervisor where
  rnf UpdateHypervisor' {..} =
    Prelude.rnf host
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf password
      `Prelude.seq` Prelude.rnf username
      `Prelude.seq` Prelude.rnf hypervisorArn

instance Data.ToHeaders UpdateHypervisor where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BackupOnPremises_v20210101.UpdateHypervisor" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateHypervisor where
  toJSON UpdateHypervisor' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Host" Data..=) Prelude.<$> host,
            ("Name" Data..=) Prelude.<$> name,
            ("Password" Data..=) Prelude.<$> password,
            ("Username" Data..=) Prelude.<$> username,
            Prelude.Just
              ("HypervisorArn" Data..= hypervisorArn)
          ]
      )

instance Data.ToPath UpdateHypervisor where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateHypervisor where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateHypervisorResponse' smart constructor.
data UpdateHypervisorResponse = UpdateHypervisorResponse'
  { -- | The Amazon Resource Name (ARN) of the hypervisor you updated.
    hypervisorArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateHypervisorResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hypervisorArn', 'updateHypervisorResponse_hypervisorArn' - The Amazon Resource Name (ARN) of the hypervisor you updated.
--
-- 'httpStatus', 'updateHypervisorResponse_httpStatus' - The response's http status code.
newUpdateHypervisorResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateHypervisorResponse
newUpdateHypervisorResponse pHttpStatus_ =
  UpdateHypervisorResponse'
    { hypervisorArn =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the hypervisor you updated.
updateHypervisorResponse_hypervisorArn :: Lens.Lens' UpdateHypervisorResponse (Prelude.Maybe Prelude.Text)
updateHypervisorResponse_hypervisorArn = Lens.lens (\UpdateHypervisorResponse' {hypervisorArn} -> hypervisorArn) (\s@UpdateHypervisorResponse' {} a -> s {hypervisorArn = a} :: UpdateHypervisorResponse)

-- | The response's http status code.
updateHypervisorResponse_httpStatus :: Lens.Lens' UpdateHypervisorResponse Prelude.Int
updateHypervisorResponse_httpStatus = Lens.lens (\UpdateHypervisorResponse' {httpStatus} -> httpStatus) (\s@UpdateHypervisorResponse' {} a -> s {httpStatus = a} :: UpdateHypervisorResponse)

instance Prelude.NFData UpdateHypervisorResponse where
  rnf UpdateHypervisorResponse' {..} =
    Prelude.rnf hypervisorArn
      `Prelude.seq` Prelude.rnf httpStatus
