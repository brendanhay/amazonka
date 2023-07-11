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
-- Module      : Amazonka.CognitoSync.SubscribeToDataset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Subscribes to receive notifications when a dataset is modified by
-- another device.
--
-- This API can only be called with temporary credentials provided by
-- Cognito Identity. You cannot call this API with developer credentials.
module Amazonka.CognitoSync.SubscribeToDataset
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

import Amazonka.CognitoSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest SubscribeToDataset where
  type
    AWSResponse SubscribeToDataset =
      SubscribeToDatasetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          SubscribeToDatasetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SubscribeToDataset where
  hashWithSalt _salt SubscribeToDataset' {..} =
    _salt
      `Prelude.hashWithSalt` identityPoolId
      `Prelude.hashWithSalt` identityId
      `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` deviceId

instance Prelude.NFData SubscribeToDataset where
  rnf SubscribeToDataset' {..} =
    Prelude.rnf identityPoolId
      `Prelude.seq` Prelude.rnf identityId
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf deviceId

instance Data.ToHeaders SubscribeToDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON SubscribeToDataset where
  toJSON = Prelude.const (Data.Object Prelude.mempty)

instance Data.ToPath SubscribeToDataset where
  toPath SubscribeToDataset' {..} =
    Prelude.mconcat
      [ "/identitypools/",
        Data.toBS identityPoolId,
        "/identities/",
        Data.toBS identityId,
        "/datasets/",
        Data.toBS datasetName,
        "/subscriptions/",
        Data.toBS deviceId
      ]

instance Data.ToQuery SubscribeToDataset where
  toQuery = Prelude.const Prelude.mempty

-- | Response to a SubscribeToDataset request.
--
-- /See:/ 'newSubscribeToDatasetResponse' smart constructor.
data SubscribeToDatasetResponse = SubscribeToDatasetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData SubscribeToDatasetResponse where
  rnf SubscribeToDatasetResponse' {..} =
    Prelude.rnf httpStatus
