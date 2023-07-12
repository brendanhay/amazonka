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
-- Module      : Amazonka.Kafka.UpdateSecurity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the security settings for the cluster. You can use this
-- operation to specify encryption and authentication on existing clusters.
module Amazonka.Kafka.UpdateSecurity
  ( -- * Creating a Request
    UpdateSecurity (..),
    newUpdateSecurity,

    -- * Request Lenses
    updateSecurity_clientAuthentication,
    updateSecurity_encryptionInfo,
    updateSecurity_clusterArn,
    updateSecurity_currentVersion,

    -- * Destructuring the Response
    UpdateSecurityResponse (..),
    newUpdateSecurityResponse,

    -- * Response Lenses
    updateSecurityResponse_clusterArn,
    updateSecurityResponse_clusterOperationArn,
    updateSecurityResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateSecurity' smart constructor.
data UpdateSecurity = UpdateSecurity'
  { -- | Includes all client authentication related information.
    clientAuthentication :: Prelude.Maybe ClientAuthentication,
    -- | Includes all encryption-related information.
    encryptionInfo :: Prelude.Maybe EncryptionInfo,
    -- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
    clusterArn :: Prelude.Text,
    -- | The version of the MSK cluster to update. Cluster versions aren\'t
    -- simple numbers. You can describe an MSK cluster to find its version.
    -- When this update operation is successful, it generates a new cluster
    -- version.
    currentVersion :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecurity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientAuthentication', 'updateSecurity_clientAuthentication' - Includes all client authentication related information.
--
-- 'encryptionInfo', 'updateSecurity_encryptionInfo' - Includes all encryption-related information.
--
-- 'clusterArn', 'updateSecurity_clusterArn' - The Amazon Resource Name (ARN) that uniquely identifies the cluster.
--
-- 'currentVersion', 'updateSecurity_currentVersion' - The version of the MSK cluster to update. Cluster versions aren\'t
-- simple numbers. You can describe an MSK cluster to find its version.
-- When this update operation is successful, it generates a new cluster
-- version.
newUpdateSecurity ::
  -- | 'clusterArn'
  Prelude.Text ->
  -- | 'currentVersion'
  Prelude.Text ->
  UpdateSecurity
newUpdateSecurity pClusterArn_ pCurrentVersion_ =
  UpdateSecurity'
    { clientAuthentication =
        Prelude.Nothing,
      encryptionInfo = Prelude.Nothing,
      clusterArn = pClusterArn_,
      currentVersion = pCurrentVersion_
    }

-- | Includes all client authentication related information.
updateSecurity_clientAuthentication :: Lens.Lens' UpdateSecurity (Prelude.Maybe ClientAuthentication)
updateSecurity_clientAuthentication = Lens.lens (\UpdateSecurity' {clientAuthentication} -> clientAuthentication) (\s@UpdateSecurity' {} a -> s {clientAuthentication = a} :: UpdateSecurity)

-- | Includes all encryption-related information.
updateSecurity_encryptionInfo :: Lens.Lens' UpdateSecurity (Prelude.Maybe EncryptionInfo)
updateSecurity_encryptionInfo = Lens.lens (\UpdateSecurity' {encryptionInfo} -> encryptionInfo) (\s@UpdateSecurity' {} a -> s {encryptionInfo = a} :: UpdateSecurity)

-- | The Amazon Resource Name (ARN) that uniquely identifies the cluster.
updateSecurity_clusterArn :: Lens.Lens' UpdateSecurity Prelude.Text
updateSecurity_clusterArn = Lens.lens (\UpdateSecurity' {clusterArn} -> clusterArn) (\s@UpdateSecurity' {} a -> s {clusterArn = a} :: UpdateSecurity)

-- | The version of the MSK cluster to update. Cluster versions aren\'t
-- simple numbers. You can describe an MSK cluster to find its version.
-- When this update operation is successful, it generates a new cluster
-- version.
updateSecurity_currentVersion :: Lens.Lens' UpdateSecurity Prelude.Text
updateSecurity_currentVersion = Lens.lens (\UpdateSecurity' {currentVersion} -> currentVersion) (\s@UpdateSecurity' {} a -> s {currentVersion = a} :: UpdateSecurity)

instance Core.AWSRequest UpdateSecurity where
  type
    AWSResponse UpdateSecurity =
      UpdateSecurityResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSecurityResponse'
            Prelude.<$> (x Data..?> "clusterArn")
            Prelude.<*> (x Data..?> "clusterOperationArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateSecurity where
  hashWithSalt _salt UpdateSecurity' {..} =
    _salt
      `Prelude.hashWithSalt` clientAuthentication
      `Prelude.hashWithSalt` encryptionInfo
      `Prelude.hashWithSalt` clusterArn
      `Prelude.hashWithSalt` currentVersion

instance Prelude.NFData UpdateSecurity where
  rnf UpdateSecurity' {..} =
    Prelude.rnf clientAuthentication
      `Prelude.seq` Prelude.rnf encryptionInfo
      `Prelude.seq` Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf currentVersion

instance Data.ToHeaders UpdateSecurity where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateSecurity where
  toJSON UpdateSecurity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientAuthentication" Data..=)
              Prelude.<$> clientAuthentication,
            ("encryptionInfo" Data..=)
              Prelude.<$> encryptionInfo,
            Prelude.Just
              ("currentVersion" Data..= currentVersion)
          ]
      )

instance Data.ToPath UpdateSecurity where
  toPath UpdateSecurity' {..} =
    Prelude.mconcat
      ["/v1/clusters/", Data.toBS clusterArn, "/security"]

instance Data.ToQuery UpdateSecurity where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateSecurityResponse' smart constructor.
data UpdateSecurityResponse = UpdateSecurityResponse'
  { -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the cluster operation.
    clusterOperationArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateSecurityResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clusterArn', 'updateSecurityResponse_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
--
-- 'clusterOperationArn', 'updateSecurityResponse_clusterOperationArn' - The Amazon Resource Name (ARN) of the cluster operation.
--
-- 'httpStatus', 'updateSecurityResponse_httpStatus' - The response's http status code.
newUpdateSecurityResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateSecurityResponse
newUpdateSecurityResponse pHttpStatus_ =
  UpdateSecurityResponse'
    { clusterArn =
        Prelude.Nothing,
      clusterOperationArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the cluster.
updateSecurityResponse_clusterArn :: Lens.Lens' UpdateSecurityResponse (Prelude.Maybe Prelude.Text)
updateSecurityResponse_clusterArn = Lens.lens (\UpdateSecurityResponse' {clusterArn} -> clusterArn) (\s@UpdateSecurityResponse' {} a -> s {clusterArn = a} :: UpdateSecurityResponse)

-- | The Amazon Resource Name (ARN) of the cluster operation.
updateSecurityResponse_clusterOperationArn :: Lens.Lens' UpdateSecurityResponse (Prelude.Maybe Prelude.Text)
updateSecurityResponse_clusterOperationArn = Lens.lens (\UpdateSecurityResponse' {clusterOperationArn} -> clusterOperationArn) (\s@UpdateSecurityResponse' {} a -> s {clusterOperationArn = a} :: UpdateSecurityResponse)

-- | The response's http status code.
updateSecurityResponse_httpStatus :: Lens.Lens' UpdateSecurityResponse Prelude.Int
updateSecurityResponse_httpStatus = Lens.lens (\UpdateSecurityResponse' {httpStatus} -> httpStatus) (\s@UpdateSecurityResponse' {} a -> s {httpStatus = a} :: UpdateSecurityResponse)

instance Prelude.NFData UpdateSecurityResponse where
  rnf UpdateSecurityResponse' {..} =
    Prelude.rnf clusterArn
      `Prelude.seq` Prelude.rnf clusterOperationArn
      `Prelude.seq` Prelude.rnf httpStatus
