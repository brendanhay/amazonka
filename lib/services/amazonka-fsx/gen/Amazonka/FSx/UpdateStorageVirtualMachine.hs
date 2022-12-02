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
-- Module      : Amazonka.FSx.UpdateStorageVirtualMachine
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an Amazon FSx for ONTAP storage virtual machine (SVM).
module Amazonka.FSx.UpdateStorageVirtualMachine
  ( -- * Creating a Request
    UpdateStorageVirtualMachine (..),
    newUpdateStorageVirtualMachine,

    -- * Request Lenses
    updateStorageVirtualMachine_activeDirectoryConfiguration,
    updateStorageVirtualMachine_clientRequestToken,
    updateStorageVirtualMachine_svmAdminPassword,
    updateStorageVirtualMachine_storageVirtualMachineId,

    -- * Destructuring the Response
    UpdateStorageVirtualMachineResponse (..),
    newUpdateStorageVirtualMachineResponse,

    -- * Response Lenses
    updateStorageVirtualMachineResponse_storageVirtualMachine,
    updateStorageVirtualMachineResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FSx.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateStorageVirtualMachine' smart constructor.
data UpdateStorageVirtualMachine = UpdateStorageVirtualMachine'
  { -- | Updates the Microsoft Active Directory (AD) configuration for an SVM
    -- that is joined to an AD.
    activeDirectoryConfiguration :: Prelude.Maybe UpdateSvmActiveDirectoryConfiguration,
    clientRequestToken :: Prelude.Maybe Prelude.Text,
    -- | Enter a new SvmAdminPassword if you are updating it.
    svmAdminPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The ID of the SVM that you want to update, in the format
    -- @svm-0123456789abcdef0@.
    storageVirtualMachineId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStorageVirtualMachine' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activeDirectoryConfiguration', 'updateStorageVirtualMachine_activeDirectoryConfiguration' - Updates the Microsoft Active Directory (AD) configuration for an SVM
-- that is joined to an AD.
--
-- 'clientRequestToken', 'updateStorageVirtualMachine_clientRequestToken' - Undocumented member.
--
-- 'svmAdminPassword', 'updateStorageVirtualMachine_svmAdminPassword' - Enter a new SvmAdminPassword if you are updating it.
--
-- 'storageVirtualMachineId', 'updateStorageVirtualMachine_storageVirtualMachineId' - The ID of the SVM that you want to update, in the format
-- @svm-0123456789abcdef0@.
newUpdateStorageVirtualMachine ::
  -- | 'storageVirtualMachineId'
  Prelude.Text ->
  UpdateStorageVirtualMachine
newUpdateStorageVirtualMachine
  pStorageVirtualMachineId_ =
    UpdateStorageVirtualMachine'
      { activeDirectoryConfiguration =
          Prelude.Nothing,
        clientRequestToken = Prelude.Nothing,
        svmAdminPassword = Prelude.Nothing,
        storageVirtualMachineId =
          pStorageVirtualMachineId_
      }

-- | Updates the Microsoft Active Directory (AD) configuration for an SVM
-- that is joined to an AD.
updateStorageVirtualMachine_activeDirectoryConfiguration :: Lens.Lens' UpdateStorageVirtualMachine (Prelude.Maybe UpdateSvmActiveDirectoryConfiguration)
updateStorageVirtualMachine_activeDirectoryConfiguration = Lens.lens (\UpdateStorageVirtualMachine' {activeDirectoryConfiguration} -> activeDirectoryConfiguration) (\s@UpdateStorageVirtualMachine' {} a -> s {activeDirectoryConfiguration = a} :: UpdateStorageVirtualMachine)

-- | Undocumented member.
updateStorageVirtualMachine_clientRequestToken :: Lens.Lens' UpdateStorageVirtualMachine (Prelude.Maybe Prelude.Text)
updateStorageVirtualMachine_clientRequestToken = Lens.lens (\UpdateStorageVirtualMachine' {clientRequestToken} -> clientRequestToken) (\s@UpdateStorageVirtualMachine' {} a -> s {clientRequestToken = a} :: UpdateStorageVirtualMachine)

-- | Enter a new SvmAdminPassword if you are updating it.
updateStorageVirtualMachine_svmAdminPassword :: Lens.Lens' UpdateStorageVirtualMachine (Prelude.Maybe Prelude.Text)
updateStorageVirtualMachine_svmAdminPassword = Lens.lens (\UpdateStorageVirtualMachine' {svmAdminPassword} -> svmAdminPassword) (\s@UpdateStorageVirtualMachine' {} a -> s {svmAdminPassword = a} :: UpdateStorageVirtualMachine) Prelude.. Lens.mapping Data._Sensitive

-- | The ID of the SVM that you want to update, in the format
-- @svm-0123456789abcdef0@.
updateStorageVirtualMachine_storageVirtualMachineId :: Lens.Lens' UpdateStorageVirtualMachine Prelude.Text
updateStorageVirtualMachine_storageVirtualMachineId = Lens.lens (\UpdateStorageVirtualMachine' {storageVirtualMachineId} -> storageVirtualMachineId) (\s@UpdateStorageVirtualMachine' {} a -> s {storageVirtualMachineId = a} :: UpdateStorageVirtualMachine)

instance Core.AWSRequest UpdateStorageVirtualMachine where
  type
    AWSResponse UpdateStorageVirtualMachine =
      UpdateStorageVirtualMachineResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateStorageVirtualMachineResponse'
            Prelude.<$> (x Data..?> "StorageVirtualMachine")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateStorageVirtualMachine where
  hashWithSalt _salt UpdateStorageVirtualMachine' {..} =
    _salt
      `Prelude.hashWithSalt` activeDirectoryConfiguration
      `Prelude.hashWithSalt` clientRequestToken
      `Prelude.hashWithSalt` svmAdminPassword
      `Prelude.hashWithSalt` storageVirtualMachineId

instance Prelude.NFData UpdateStorageVirtualMachine where
  rnf UpdateStorageVirtualMachine' {..} =
    Prelude.rnf activeDirectoryConfiguration
      `Prelude.seq` Prelude.rnf clientRequestToken
      `Prelude.seq` Prelude.rnf svmAdminPassword
      `Prelude.seq` Prelude.rnf storageVirtualMachineId

instance Data.ToHeaders UpdateStorageVirtualMachine where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSSimbaAPIService_v20180301.UpdateStorageVirtualMachine" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateStorageVirtualMachine where
  toJSON UpdateStorageVirtualMachine' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ActiveDirectoryConfiguration" Data..=)
              Prelude.<$> activeDirectoryConfiguration,
            ("ClientRequestToken" Data..=)
              Prelude.<$> clientRequestToken,
            ("SvmAdminPassword" Data..=)
              Prelude.<$> svmAdminPassword,
            Prelude.Just
              ( "StorageVirtualMachineId"
                  Data..= storageVirtualMachineId
              )
          ]
      )

instance Data.ToPath UpdateStorageVirtualMachine where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateStorageVirtualMachine where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateStorageVirtualMachineResponse' smart constructor.
data UpdateStorageVirtualMachineResponse = UpdateStorageVirtualMachineResponse'
  { storageVirtualMachine :: Prelude.Maybe StorageVirtualMachine,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateStorageVirtualMachineResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storageVirtualMachine', 'updateStorageVirtualMachineResponse_storageVirtualMachine' - Undocumented member.
--
-- 'httpStatus', 'updateStorageVirtualMachineResponse_httpStatus' - The response's http status code.
newUpdateStorageVirtualMachineResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateStorageVirtualMachineResponse
newUpdateStorageVirtualMachineResponse pHttpStatus_ =
  UpdateStorageVirtualMachineResponse'
    { storageVirtualMachine =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateStorageVirtualMachineResponse_storageVirtualMachine :: Lens.Lens' UpdateStorageVirtualMachineResponse (Prelude.Maybe StorageVirtualMachine)
updateStorageVirtualMachineResponse_storageVirtualMachine = Lens.lens (\UpdateStorageVirtualMachineResponse' {storageVirtualMachine} -> storageVirtualMachine) (\s@UpdateStorageVirtualMachineResponse' {} a -> s {storageVirtualMachine = a} :: UpdateStorageVirtualMachineResponse)

-- | The response's http status code.
updateStorageVirtualMachineResponse_httpStatus :: Lens.Lens' UpdateStorageVirtualMachineResponse Prelude.Int
updateStorageVirtualMachineResponse_httpStatus = Lens.lens (\UpdateStorageVirtualMachineResponse' {httpStatus} -> httpStatus) (\s@UpdateStorageVirtualMachineResponse' {} a -> s {httpStatus = a} :: UpdateStorageVirtualMachineResponse)

instance
  Prelude.NFData
    UpdateStorageVirtualMachineResponse
  where
  rnf UpdateStorageVirtualMachineResponse' {..} =
    Prelude.rnf storageVirtualMachine
      `Prelude.seq` Prelude.rnf httpStatus
