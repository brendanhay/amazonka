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
-- Module      : Network.AWS.SSM.GetDeployablePatchSnapshotForInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the current snapshot for the patch baseline the instance uses.
-- This API is primarily used by the AWS-RunPatchBaseline Systems Manager
-- document.
module Network.AWS.SSM.GetDeployablePatchSnapshotForInstance
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SSM.Types

-- | /See:/ 'newGetDeployablePatchSnapshotForInstance' smart constructor.
data GetDeployablePatchSnapshotForInstance = GetDeployablePatchSnapshotForInstance'
  { -- | Defines the basic information about a patch baseline override.
    baselineOverride :: Core.Maybe BaselineOverride,
    -- | The ID of the instance for which the appropriate patch snapshot should
    -- be retrieved.
    instanceId :: Core.Text,
    -- | The user-defined snapshot ID.
    snapshotId :: Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

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
-- 'instanceId', 'getDeployablePatchSnapshotForInstance_instanceId' - The ID of the instance for which the appropriate patch snapshot should
-- be retrieved.
--
-- 'snapshotId', 'getDeployablePatchSnapshotForInstance_snapshotId' - The user-defined snapshot ID.
newGetDeployablePatchSnapshotForInstance ::
  -- | 'instanceId'
  Core.Text ->
  -- | 'snapshotId'
  Core.Text ->
  GetDeployablePatchSnapshotForInstance
newGetDeployablePatchSnapshotForInstance
  pInstanceId_
  pSnapshotId_ =
    GetDeployablePatchSnapshotForInstance'
      { baselineOverride =
          Core.Nothing,
        instanceId = pInstanceId_,
        snapshotId = pSnapshotId_
      }

-- | Defines the basic information about a patch baseline override.
getDeployablePatchSnapshotForInstance_baselineOverride :: Lens.Lens' GetDeployablePatchSnapshotForInstance (Core.Maybe BaselineOverride)
getDeployablePatchSnapshotForInstance_baselineOverride = Lens.lens (\GetDeployablePatchSnapshotForInstance' {baselineOverride} -> baselineOverride) (\s@GetDeployablePatchSnapshotForInstance' {} a -> s {baselineOverride = a} :: GetDeployablePatchSnapshotForInstance)

-- | The ID of the instance for which the appropriate patch snapshot should
-- be retrieved.
getDeployablePatchSnapshotForInstance_instanceId :: Lens.Lens' GetDeployablePatchSnapshotForInstance Core.Text
getDeployablePatchSnapshotForInstance_instanceId = Lens.lens (\GetDeployablePatchSnapshotForInstance' {instanceId} -> instanceId) (\s@GetDeployablePatchSnapshotForInstance' {} a -> s {instanceId = a} :: GetDeployablePatchSnapshotForInstance)

-- | The user-defined snapshot ID.
getDeployablePatchSnapshotForInstance_snapshotId :: Lens.Lens' GetDeployablePatchSnapshotForInstance Core.Text
getDeployablePatchSnapshotForInstance_snapshotId = Lens.lens (\GetDeployablePatchSnapshotForInstance' {snapshotId} -> snapshotId) (\s@GetDeployablePatchSnapshotForInstance' {} a -> s {snapshotId = a} :: GetDeployablePatchSnapshotForInstance)

instance
  Core.AWSRequest
    GetDeployablePatchSnapshotForInstance
  where
  type
    AWSResponse
      GetDeployablePatchSnapshotForInstance =
      GetDeployablePatchSnapshotForInstanceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDeployablePatchSnapshotForInstanceResponse'
            Core.<$> (x Core..?> "InstanceId")
              Core.<*> (x Core..?> "Product")
              Core.<*> (x Core..?> "SnapshotDownloadUrl")
              Core.<*> (x Core..?> "SnapshotId")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    GetDeployablePatchSnapshotForInstance

instance
  Core.NFData
    GetDeployablePatchSnapshotForInstance

instance
  Core.ToHeaders
    GetDeployablePatchSnapshotForInstance
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonSSM.GetDeployablePatchSnapshotForInstance" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    GetDeployablePatchSnapshotForInstance
  where
  toJSON GetDeployablePatchSnapshotForInstance' {..} =
    Core.object
      ( Core.catMaybes
          [ ("BaselineOverride" Core..=)
              Core.<$> baselineOverride,
            Core.Just ("InstanceId" Core..= instanceId),
            Core.Just ("SnapshotId" Core..= snapshotId)
          ]
      )

instance
  Core.ToPath
    GetDeployablePatchSnapshotForInstance
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    GetDeployablePatchSnapshotForInstance
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDeployablePatchSnapshotForInstanceResponse' smart constructor.
data GetDeployablePatchSnapshotForInstanceResponse = GetDeployablePatchSnapshotForInstanceResponse'
  { -- | The ID of the instance.
    instanceId :: Core.Maybe Core.Text,
    -- | Returns the specific operating system (for example Windows Server 2012
    -- or Amazon Linux 2015.09) on the instance for the specified patch
    -- snapshot.
    product :: Core.Maybe Core.Text,
    -- | A pre-signed Amazon S3 URL that can be used to download the patch
    -- snapshot.
    snapshotDownloadUrl :: Core.Maybe Core.Text,
    -- | The user-defined snapshot ID.
    snapshotId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDeployablePatchSnapshotForInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'getDeployablePatchSnapshotForInstanceResponse_instanceId' - The ID of the instance.
--
-- 'product', 'getDeployablePatchSnapshotForInstanceResponse_product' - Returns the specific operating system (for example Windows Server 2012
-- or Amazon Linux 2015.09) on the instance for the specified patch
-- snapshot.
--
-- 'snapshotDownloadUrl', 'getDeployablePatchSnapshotForInstanceResponse_snapshotDownloadUrl' - A pre-signed Amazon S3 URL that can be used to download the patch
-- snapshot.
--
-- 'snapshotId', 'getDeployablePatchSnapshotForInstanceResponse_snapshotId' - The user-defined snapshot ID.
--
-- 'httpStatus', 'getDeployablePatchSnapshotForInstanceResponse_httpStatus' - The response's http status code.
newGetDeployablePatchSnapshotForInstanceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDeployablePatchSnapshotForInstanceResponse
newGetDeployablePatchSnapshotForInstanceResponse
  pHttpStatus_ =
    GetDeployablePatchSnapshotForInstanceResponse'
      { instanceId =
          Core.Nothing,
        product = Core.Nothing,
        snapshotDownloadUrl =
          Core.Nothing,
        snapshotId = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The ID of the instance.
getDeployablePatchSnapshotForInstanceResponse_instanceId :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse (Core.Maybe Core.Text)
getDeployablePatchSnapshotForInstanceResponse_instanceId = Lens.lens (\GetDeployablePatchSnapshotForInstanceResponse' {instanceId} -> instanceId) (\s@GetDeployablePatchSnapshotForInstanceResponse' {} a -> s {instanceId = a} :: GetDeployablePatchSnapshotForInstanceResponse)

-- | Returns the specific operating system (for example Windows Server 2012
-- or Amazon Linux 2015.09) on the instance for the specified patch
-- snapshot.
getDeployablePatchSnapshotForInstanceResponse_product :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse (Core.Maybe Core.Text)
getDeployablePatchSnapshotForInstanceResponse_product = Lens.lens (\GetDeployablePatchSnapshotForInstanceResponse' {product} -> product) (\s@GetDeployablePatchSnapshotForInstanceResponse' {} a -> s {product = a} :: GetDeployablePatchSnapshotForInstanceResponse)

-- | A pre-signed Amazon S3 URL that can be used to download the patch
-- snapshot.
getDeployablePatchSnapshotForInstanceResponse_snapshotDownloadUrl :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse (Core.Maybe Core.Text)
getDeployablePatchSnapshotForInstanceResponse_snapshotDownloadUrl = Lens.lens (\GetDeployablePatchSnapshotForInstanceResponse' {snapshotDownloadUrl} -> snapshotDownloadUrl) (\s@GetDeployablePatchSnapshotForInstanceResponse' {} a -> s {snapshotDownloadUrl = a} :: GetDeployablePatchSnapshotForInstanceResponse)

-- | The user-defined snapshot ID.
getDeployablePatchSnapshotForInstanceResponse_snapshotId :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse (Core.Maybe Core.Text)
getDeployablePatchSnapshotForInstanceResponse_snapshotId = Lens.lens (\GetDeployablePatchSnapshotForInstanceResponse' {snapshotId} -> snapshotId) (\s@GetDeployablePatchSnapshotForInstanceResponse' {} a -> s {snapshotId = a} :: GetDeployablePatchSnapshotForInstanceResponse)

-- | The response's http status code.
getDeployablePatchSnapshotForInstanceResponse_httpStatus :: Lens.Lens' GetDeployablePatchSnapshotForInstanceResponse Core.Int
getDeployablePatchSnapshotForInstanceResponse_httpStatus = Lens.lens (\GetDeployablePatchSnapshotForInstanceResponse' {httpStatus} -> httpStatus) (\s@GetDeployablePatchSnapshotForInstanceResponse' {} a -> s {httpStatus = a} :: GetDeployablePatchSnapshotForInstanceResponse)

instance
  Core.NFData
    GetDeployablePatchSnapshotForInstanceResponse
