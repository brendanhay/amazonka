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
-- Module      : Network.AWS.CognitoSync.UnsubscribeFromDataset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Unsubscribes from receiving notifications when a dataset is modified by
-- another device.
--
-- This API can only be called with temporary credentials provided by
-- Cognito Identity. You cannot call this API with developer credentials.
module Network.AWS.CognitoSync.UnsubscribeFromDataset
  ( -- * Creating a Request
    UnsubscribeFromDataset (..),
    newUnsubscribeFromDataset,

    -- * Request Lenses
    unsubscribeFromDataset_identityPoolId,
    unsubscribeFromDataset_identityId,
    unsubscribeFromDataset_datasetName,
    unsubscribeFromDataset_deviceId,

    -- * Destructuring the Response
    UnsubscribeFromDatasetResponse (..),
    newUnsubscribeFromDatasetResponse,

    -- * Response Lenses
    unsubscribeFromDatasetResponse_httpStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to UnsubscribeFromDataset.
--
-- /See:/ 'newUnsubscribeFromDataset' smart constructor.
data UnsubscribeFromDataset = UnsubscribeFromDataset'
  { -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. The ID of the pool to which this identity belongs.
    identityPoolId :: Core.Text,
    -- | Unique ID for this identity.
    identityId :: Core.Text,
    -- | The name of the dataset from which to unsubcribe.
    datasetName :: Core.Text,
    -- | The unique ID generated for this device by Cognito.
    deviceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UnsubscribeFromDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'unsubscribeFromDataset_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. The ID of the pool to which this identity belongs.
--
-- 'identityId', 'unsubscribeFromDataset_identityId' - Unique ID for this identity.
--
-- 'datasetName', 'unsubscribeFromDataset_datasetName' - The name of the dataset from which to unsubcribe.
--
-- 'deviceId', 'unsubscribeFromDataset_deviceId' - The unique ID generated for this device by Cognito.
newUnsubscribeFromDataset ::
  -- | 'identityPoolId'
  Core.Text ->
  -- | 'identityId'
  Core.Text ->
  -- | 'datasetName'
  Core.Text ->
  -- | 'deviceId'
  Core.Text ->
  UnsubscribeFromDataset
newUnsubscribeFromDataset
  pIdentityPoolId_
  pIdentityId_
  pDatasetName_
  pDeviceId_ =
    UnsubscribeFromDataset'
      { identityPoolId =
          pIdentityPoolId_,
        identityId = pIdentityId_,
        datasetName = pDatasetName_,
        deviceId = pDeviceId_
      }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. The ID of the pool to which this identity belongs.
unsubscribeFromDataset_identityPoolId :: Lens.Lens' UnsubscribeFromDataset Core.Text
unsubscribeFromDataset_identityPoolId = Lens.lens (\UnsubscribeFromDataset' {identityPoolId} -> identityPoolId) (\s@UnsubscribeFromDataset' {} a -> s {identityPoolId = a} :: UnsubscribeFromDataset)

-- | Unique ID for this identity.
unsubscribeFromDataset_identityId :: Lens.Lens' UnsubscribeFromDataset Core.Text
unsubscribeFromDataset_identityId = Lens.lens (\UnsubscribeFromDataset' {identityId} -> identityId) (\s@UnsubscribeFromDataset' {} a -> s {identityId = a} :: UnsubscribeFromDataset)

-- | The name of the dataset from which to unsubcribe.
unsubscribeFromDataset_datasetName :: Lens.Lens' UnsubscribeFromDataset Core.Text
unsubscribeFromDataset_datasetName = Lens.lens (\UnsubscribeFromDataset' {datasetName} -> datasetName) (\s@UnsubscribeFromDataset' {} a -> s {datasetName = a} :: UnsubscribeFromDataset)

-- | The unique ID generated for this device by Cognito.
unsubscribeFromDataset_deviceId :: Lens.Lens' UnsubscribeFromDataset Core.Text
unsubscribeFromDataset_deviceId = Lens.lens (\UnsubscribeFromDataset' {deviceId} -> deviceId) (\s@UnsubscribeFromDataset' {} a -> s {deviceId = a} :: UnsubscribeFromDataset)

instance Core.AWSRequest UnsubscribeFromDataset where
  type
    AWSResponse UnsubscribeFromDataset =
      UnsubscribeFromDatasetResponse
  request = Request.delete defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          UnsubscribeFromDatasetResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UnsubscribeFromDataset

instance Core.NFData UnsubscribeFromDataset

instance Core.ToHeaders UnsubscribeFromDataset where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath UnsubscribeFromDataset where
  toPath UnsubscribeFromDataset' {..} =
    Core.mconcat
      [ "/identitypools/",
        Core.toBS identityPoolId,
        "/identities/",
        Core.toBS identityId,
        "/datasets/",
        Core.toBS datasetName,
        "/subscriptions/",
        Core.toBS deviceId
      ]

instance Core.ToQuery UnsubscribeFromDataset where
  toQuery = Core.const Core.mempty

-- | Response to an UnsubscribeFromDataset request.
--
-- /See:/ 'newUnsubscribeFromDatasetResponse' smart constructor.
data UnsubscribeFromDatasetResponse = UnsubscribeFromDatasetResponse'
  { -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UnsubscribeFromDatasetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'unsubscribeFromDatasetResponse_httpStatus' - The response's http status code.
newUnsubscribeFromDatasetResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UnsubscribeFromDatasetResponse
newUnsubscribeFromDatasetResponse pHttpStatus_ =
  UnsubscribeFromDatasetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
unsubscribeFromDatasetResponse_httpStatus :: Lens.Lens' UnsubscribeFromDatasetResponse Core.Int
unsubscribeFromDatasetResponse_httpStatus = Lens.lens (\UnsubscribeFromDatasetResponse' {httpStatus} -> httpStatus) (\s@UnsubscribeFromDatasetResponse' {} a -> s {httpStatus = a} :: UnsubscribeFromDatasetResponse)

instance Core.NFData UnsubscribeFromDatasetResponse
