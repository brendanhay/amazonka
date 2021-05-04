{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.CognitoSync.SubscribeToDataset
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Subscribes to receive notifications when a dataset is modified by
-- another device.
--
-- This API can only be called with temporary credentials provided by
-- Cognito Identity. You cannot call this API with developer credentials.
module Network.AWS.CognitoSync.SubscribeToDataset
  ( -- * Creating a Request
    SubscribeToDataset (..),
    newSubscribeToDataset,

    -- * Request Lenses
    subscribeToDataset_identityPoolId,
    subscribeToDataset_identityId,
    subscribeToDataset_datasetName,
    subscribeToDataset_deviceId,

    -- * Destructuring the Response
    SubscribeToDatasetResponse (..),
    newSubscribeToDatasetResponse,

    -- * Response Lenses
    subscribeToDatasetResponse_httpStatus,
  )
where

import Network.AWS.CognitoSync.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | A request to SubscribeToDatasetRequest.
--
-- /See:/ 'newSubscribeToDataset' smart constructor.
data SubscribeToDataset = SubscribeToDataset'
  { -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. The ID of the pool to which the identity belongs.
    identityPoolId :: Prelude.Text,
    -- | Unique ID for this identity.
    identityId :: Prelude.Text,
    -- | The name of the dataset to subcribe to.
    datasetName :: Prelude.Text,
    -- | The unique ID generated for this device by Cognito.
    deviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SubscribeToDataset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'identityPoolId', 'subscribeToDataset_identityPoolId' - A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. The ID of the pool to which the identity belongs.
--
-- 'identityId', 'subscribeToDataset_identityId' - Unique ID for this identity.
--
-- 'datasetName', 'subscribeToDataset_datasetName' - The name of the dataset to subcribe to.
--
-- 'deviceId', 'subscribeToDataset_deviceId' - The unique ID generated for this device by Cognito.
newSubscribeToDataset ::
  -- | 'identityPoolId'
  Prelude.Text ->
  -- | 'identityId'
  Prelude.Text ->
  -- | 'datasetName'
  Prelude.Text ->
  -- | 'deviceId'
  Prelude.Text ->
  SubscribeToDataset
newSubscribeToDataset
  pIdentityPoolId_
  pIdentityId_
  pDatasetName_
  pDeviceId_ =
    SubscribeToDataset'
      { identityPoolId =
          pIdentityPoolId_,
        identityId = pIdentityId_,
        datasetName = pDatasetName_,
        deviceId = pDeviceId_
      }

-- | A name-spaced GUID (for example,
-- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
-- Cognito. The ID of the pool to which the identity belongs.
subscribeToDataset_identityPoolId :: Lens.Lens' SubscribeToDataset Prelude.Text
subscribeToDataset_identityPoolId = Lens.lens (\SubscribeToDataset' {identityPoolId} -> identityPoolId) (\s@SubscribeToDataset' {} a -> s {identityPoolId = a} :: SubscribeToDataset)

-- | Unique ID for this identity.
subscribeToDataset_identityId :: Lens.Lens' SubscribeToDataset Prelude.Text
subscribeToDataset_identityId = Lens.lens (\SubscribeToDataset' {identityId} -> identityId) (\s@SubscribeToDataset' {} a -> s {identityId = a} :: SubscribeToDataset)

-- | The name of the dataset to subcribe to.
subscribeToDataset_datasetName :: Lens.Lens' SubscribeToDataset Prelude.Text
subscribeToDataset_datasetName = Lens.lens (\SubscribeToDataset' {datasetName} -> datasetName) (\s@SubscribeToDataset' {} a -> s {datasetName = a} :: SubscribeToDataset)

-- | The unique ID generated for this device by Cognito.
subscribeToDataset_deviceId :: Lens.Lens' SubscribeToDataset Prelude.Text
subscribeToDataset_deviceId = Lens.lens (\SubscribeToDataset' {deviceId} -> deviceId) (\s@SubscribeToDataset' {} a -> s {deviceId = a} :: SubscribeToDataset)

instance Prelude.AWSRequest SubscribeToDataset where
  type
    Rs SubscribeToDataset =
      SubscribeToDatasetResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          SubscribeToDatasetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SubscribeToDataset

instance Prelude.NFData SubscribeToDataset

instance Prelude.ToHeaders SubscribeToDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON SubscribeToDataset where
  toJSON =
    Prelude.const (Prelude.Object Prelude.mempty)

instance Prelude.ToPath SubscribeToDataset where
  toPath SubscribeToDataset' {..} =
    Prelude.mconcat
      [ "/identitypools/",
        Prelude.toBS identityPoolId,
        "/identities/",
        Prelude.toBS identityId,
        "/datasets/",
        Prelude.toBS datasetName,
        "/subscriptions/",
        Prelude.toBS deviceId
      ]

instance Prelude.ToQuery SubscribeToDataset where
  toQuery = Prelude.const Prelude.mempty

-- | Response to a SubscribeToDataset request.
--
-- /See:/ 'newSubscribeToDatasetResponse' smart constructor.
data SubscribeToDatasetResponse = SubscribeToDatasetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'SubscribeToDatasetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'subscribeToDatasetResponse_httpStatus' - The response's http status code.
newSubscribeToDatasetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SubscribeToDatasetResponse
newSubscribeToDatasetResponse pHttpStatus_ =
  SubscribeToDatasetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
subscribeToDatasetResponse_httpStatus :: Lens.Lens' SubscribeToDatasetResponse Prelude.Int
subscribeToDatasetResponse_httpStatus = Lens.lens (\SubscribeToDatasetResponse' {httpStatus} -> httpStatus) (\s@SubscribeToDatasetResponse' {} a -> s {httpStatus = a} :: SubscribeToDatasetResponse)

instance Prelude.NFData SubscribeToDatasetResponse
