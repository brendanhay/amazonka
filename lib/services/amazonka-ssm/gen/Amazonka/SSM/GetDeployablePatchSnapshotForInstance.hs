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
-- Module      : Amazonka.SSM.GetDeployablePatchSnapshotForInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current snapshot for the patch baseline the managed node
-- uses. This API is primarily used by the @AWS-RunPatchBaseline@ Systems
-- Manager document (SSM document).
--
-- If you run the command locally, such as with the Command Line Interface
-- (CLI), the system attempts to use your local Amazon Web Services
-- credentials and the operation fails. To avoid this, you can run the
-- command in the Amazon Web Services Systems Manager console. Use Run
-- Command, a capability of Amazon Web Services Systems Manager, with an
-- SSM document that enables you to target a managed node with a script or
-- command. For example, run the command using the @AWS-RunShellScript@
-- document or the @AWS-RunPowerShellScript@ document.
module Amazonka.SSM.GetDeployablePatchSnapshotForInstance
  ( -- * Creating a Request
    GetDeployablePatchSnapshotForInstance (..),
    newGetDeployablePatchSnapshotForInstance,

    -- * Request Lenses
    getDeployablePatchSnapshotForInstance_baselineOverride,
    getDeployablePatchSnapshotForInstance_instanceId,
    getDeployablePatchSnapshotForInstance_snapshotId,

    -- * Destructuring the Response
    GetDeployablePatchSnapshotForInstanceResponse (..),
    newGetDeployablePatchSnapshotForInstanceResponse,

    -- * Response Lenses
    getDeployablePatchSnapshotForInstanceResponse_instanceId,
    getDeployablePatchSnapshotForInstanceResponse_product,
    getDeployablePatchSnapshotForInstanceResponse_snapshotDownloadUrl,
    getDeployablePatchSnapshotForInstanceResponse_snapshotId,
    getDeployablePatchSnapshotForInstanceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSM.Types

-- | /See:/ 'newGetDeployablePatchSnapshotForInstance' smart constructor.
data GetDeployablePatchSnapshotForInstance = GetDeployablePatchSnapshotForInstance'
  { -- | Defines the basic information about a patch baseline override.
    baselineOverride :: Prelude.Maybe BaselineOverride,
    -- | The ID of the managed node for which the appropriate patch snapshot
    -- should be retrieved.
    instanceId :: Prelude.Text,
    -- | The snapshot ID provided by the user when running
    -- @AWS-RunPatchBaseline@.
    snapshotId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeployablePatchSnapshotForInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'baselineOverride', 'getDeployablePatchSnapshotForInstance_baselineOverride' - Defines the basic information about a patch baseline override.
--
-- 'instanceId', 'getDeployablePatchSnapshotForInstance_instanceId' - The ID of the managed node for which the appropriate patch snapshot
-- should be retrieved.
--
-- 'snapshotId', 'getDeployablePatchSnapshotForInstance_snapshotId' - The snapshot ID provided by the user when running
-- @AWS-RunPatchBaseline@.
newGetDeployablePatchSnapshotForInstance ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'snapshotId'
  Prelude.Text ->
  GetDeployablePatchSnapshotForInstance
newGetDeployablePatchSnapshotForInstance
  pInstanceId_
  pSnapshotId_ =
    GetDeployablePatchSnapshotForInstance'
      { baselineOverride =
          Prelude.Nothing,
        instanceId = pInstanceId_,
        snapshotId = pSnapshotId_
      }

-- | Defines the basic information about a patch baseline override.
getDeployablePatchSnapshotForInstance_baselineOverride :: Lens.Lens' GetDeployablePatchSnapshotForInstance (Prelude.Maybe BaselineOverride)
getDeployablePatchSnapshotForInstance_baselineOverride = Lens.lens (\GetDeployablePatchSnapshotForInstance' {baselineOverride} -> baselineOverride) (\s@GetDeployablePatchSnapshotForInstance' {} a -> s {baselineOverride = a} :: GetDeployablePatchSnapshotForInstance)

-- | The ID of the managed node for which the appropriate patch snapshot
-- should be retrieved.
getDeployablePatchSnapshotForInstance_instanceId :: Lens.Lens' GetDeployablePatchSnapshotForInstance Prelude.Text
getDeployablePatchSnapshotForInstance_instanceId = Lens.lens (\GetDeployablePatchSnapshotForInstance' {instanceId} -> instanceId) (\s@GetDeployablePatchSnapshotForInstance' {} a -> s {instanceId = a} :: GetDeployablePatchSnapshotForInstance)

-- | The snapshot ID provided by the user when running
-- @AWS-RunPatchBaseline@.
getDeployablePatchSnapshotForInstance_snapshotId :: Lens.Lens' GetDeployablePatchSnapshotForInstance Prelude.Text
getDeployablePatchSnapshotForInstance_snapshotId = Lens.lens (\GetDeployablePatchSnapshotForInstance' {snapshotId} -> snapshotId) (\s@GetDeployablePatchSnapshotForInstance' {} a -> s {snapshotId = a} :: GetDeployablePatchSnapshotForInstance)

instance
  Core.AWSRequest
    GetDeployablePatchSnapshotForInstance
  where
  type
    AWSResponse
      GetDeployablePatchSnapshotForInstance =
      GetDeployablePatchSnapshotForInstanceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeployablePatchSnapshotForInstanceResponse'
            Prelude.<$> (x Data..?> "InstanceId")
            Prelude.<*> (x Data..?> "Product")
            Prelude.<*> (x Data..?> "SnapshotDownloadUrl")
            Prelude.<*> (x Data..?> "SnapshotId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetDeployablePatchSnapshotForInstance
  where
  hashWithSalt
    _salt
    GetDeployablePatchSnapshotForInstance' {..} =
      _salt
        `Prelude.hashWithSalt` baselineOverride
        `Prelude.hashWithSalt` instanceId
        `Prelude.hashWithSalt` snapshotId

instance
  Prelude.NFData
    GetDeployablePatchSnapshotForInstance
  where
  rnf GetDeployablePatchSnapshotForInstance' {..} =
    Prelude.rnf baselineOverride
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf snapshotId

instance
  Data.ToHeaders
    GetDeployablePatchSnapshotForInstance
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonSSM.GetDeployablePatchSnapshotForInstance" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    GetDeployablePatchSnapshotForInstance
  where
  toJSON GetDeployablePatchSnapshotForInstance' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("BaselineOverride" Data..=)
              Prelude.<$> baselineOverride,
            Prelude.Just ("InstanceId" Data..= instanceId),
            Prelude.Just ("SnapshotId" Data..= snapshotId)
          ]
      )

instance
  Data.ToPath
    GetDeployablePatchSnapshotForInstance
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    GetDeployablePatchSnapshotForInstance
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDeployablePatchSnapshotForInstanceResponse' smart constructor.
data GetDeployablePatchSnapshotForInstanceResponse = GetDeployablePatchSnapshotForInstanceResponse'
  { -- | The managed node ID.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | Returns the specific operating system (for example Windows Server 2012
    -- or Amazon Linux 2015.09) on the managed node for the specified patch
    -- snapshot.
    product :: Prelude.Maybe Prelude.Text,
    -- | A pre-signed Amazon Simple Storage Service (Amazon S3) URL that can be
    -- used to download the patch snapshot.
    snapshotDownloadUrl :: Prelude.Maybe Prelude.Text,
    -- | The user-defined snapshot ID.
    snapshotId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDeployablePatchSnapshotForInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'getDeployablePatchSnapshotForInstanceResponse_instanceId' - The managed node ID.
--
-- 'product', 'getDeployablePatchSnapshotForInstanceResponse_product' - Returns the specific operating system (for example Windows Server 2012
-- or Amazon Linux 2015.09) on the managed node for the specified patch
-- snapshot.
--
-- 'snapshotDownloadUrl', 'getDeployablePatchSnapshotForInstanceResponse_snapshotDownloadUrl' - A pre-signed Amazon Simple Storage Service (Amazon S3) URL that can be
-- used to download the patch snapshot.
--
-- 'snapshotId', 'getDeployablePatchSnapshotForInstanceResponse_snapshotId' - The user-defined snapshot ID.
--
-- 'httpStatus', 'getDeployablePatchSnapshotForInstanceResponse_httpStatus' - The response's http status code.
newGetDeployablePatchSnapshotForInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDeployablePatchSnapshotForInstanceResponse
newGetDeployablePatchSnapshotForInstanceResponse
  pHttpStatus_ =
    GetDeployablePatchSnapshotForInstanceResponse'
      { instanceId =
          Prelude.Nothing,
        product = Prelude.Nothing,
        snapshotDownloadUrl =
          Prelude.Nothing,
        snapshotId = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The managed node ID.
getDeployablePatchSnapshotForInstanceResponse_instanceId :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse (Prelude.Maybe Prelude.Text)
getDeployablePatchSnapshotForInstanceResponse_instanceId = Lens.lens (\GetDeployablePatchSnapshotForInstanceResponse' {instanceId} -> instanceId) (\s@GetDeployablePatchSnapshotForInstanceResponse' {} a -> s {instanceId = a} :: GetDeployablePatchSnapshotForInstanceResponse)

-- | Returns the specific operating system (for example Windows Server 2012
-- or Amazon Linux 2015.09) on the managed node for the specified patch
-- snapshot.
getDeployablePatchSnapshotForInstanceResponse_product :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse (Prelude.Maybe Prelude.Text)
getDeployablePatchSnapshotForInstanceResponse_product = Lens.lens (\GetDeployablePatchSnapshotForInstanceResponse' {product} -> product) (\s@GetDeployablePatchSnapshotForInstanceResponse' {} a -> s {product = a} :: GetDeployablePatchSnapshotForInstanceResponse)

-- | A pre-signed Amazon Simple Storage Service (Amazon S3) URL that can be
-- used to download the patch snapshot.
getDeployablePatchSnapshotForInstanceResponse_snapshotDownloadUrl :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse (Prelude.Maybe Prelude.Text)
getDeployablePatchSnapshotForInstanceResponse_snapshotDownloadUrl = Lens.lens (\GetDeployablePatchSnapshotForInstanceResponse' {snapshotDownloadUrl} -> snapshotDownloadUrl) (\s@GetDeployablePatchSnapshotForInstanceResponse' {} a -> s {snapshotDownloadUrl = a} :: GetDeployablePatchSnapshotForInstanceResponse)

-- | The user-defined snapshot ID.
getDeployablePatchSnapshotForInstanceResponse_snapshotId :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse (Prelude.Maybe Prelude.Text)
getDeployablePatchSnapshotForInstanceResponse_snapshotId = Lens.lens (\GetDeployablePatchSnapshotForInstanceResponse' {snapshotId} -> snapshotId) (\s@GetDeployablePatchSnapshotForInstanceResponse' {} a -> s {snapshotId = a} :: GetDeployablePatchSnapshotForInstanceResponse)

-- | The response's http status code.
getDeployablePatchSnapshotForInstanceResponse_httpStatus :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse Prelude.Int
getDeployablePatchSnapshotForInstanceResponse_httpStatus = Lens.lens (\GetDeployablePatchSnapshotForInstanceResponse' {httpStatus} -> httpStatus) (\s@GetDeployablePatchSnapshotForInstanceResponse' {} a -> s {httpStatus = a} :: GetDeployablePatchSnapshotForInstanceResponse)

instance
  Prelude.NFData
    GetDeployablePatchSnapshotForInstanceResponse
  where
  rnf
    GetDeployablePatchSnapshotForInstanceResponse' {..} =
      Prelude.rnf instanceId
        `Prelude.seq` Prelude.rnf product
        `Prelude.seq` Prelude.rnf snapshotDownloadUrl
        `Prelude.seq` Prelude.rnf snapshotId
        `Prelude.seq` Prelude.rnf httpStatus
