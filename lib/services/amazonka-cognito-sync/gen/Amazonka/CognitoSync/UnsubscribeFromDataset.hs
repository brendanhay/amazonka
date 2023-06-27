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
-- Module      : Amazonka.CognitoSync.UnsubscribeFromDataset
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.CognitoSync.UnsubscribeFromDataset
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

import Amazonka.CognitoSync.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | A request to UnsubscribeFromDataset.
--
-- /See:/ 'newUnsubscribeFromDataset' smart constructor.
data UnsubscribeFromDataset = UnsubscribeFromDataset'
  { -- | A name-spaced GUID (for example,
    -- us-east-1:23EC4050-6AEA-7089-A2DD-08002EXAMPLE) created by Amazon
    -- Cognito. The ID of the pool to which this identity belongs.
    identityPoolId :: Prelude.Text,
    -- | Unique ID for this identity.
    identityId :: Prelude.Text,
    -- | The name of the dataset from which to unsubcribe.
    datasetName :: Prelude.Text,
    -- | The unique ID generated for this device by Cognito.
    deviceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'identityId'
  Prelude.Text ->
  -- | 'datasetName'
  Prelude.Text ->
  -- | 'deviceId'
  Prelude.Text ->
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
unsubscribeFromDataset_identityPoolId :: Lens.Lens' UnsubscribeFromDataset Prelude.Text
unsubscribeFromDataset_identityPoolId = Lens.lens (\UnsubscribeFromDataset' {identityPoolId} -> identityPoolId) (\s@UnsubscribeFromDataset' {} a -> s {identityPoolId = a} :: UnsubscribeFromDataset)

-- | Unique ID for this identity.
unsubscribeFromDataset_identityId :: Lens.Lens' UnsubscribeFromDataset Prelude.Text
unsubscribeFromDataset_identityId = Lens.lens (\UnsubscribeFromDataset' {identityId} -> identityId) (\s@UnsubscribeFromDataset' {} a -> s {identityId = a} :: UnsubscribeFromDataset)

-- | The name of the dataset from which to unsubcribe.
unsubscribeFromDataset_datasetName :: Lens.Lens' UnsubscribeFromDataset Prelude.Text
unsubscribeFromDataset_datasetName = Lens.lens (\UnsubscribeFromDataset' {datasetName} -> datasetName) (\s@UnsubscribeFromDataset' {} a -> s {datasetName = a} :: UnsubscribeFromDataset)

-- | The unique ID generated for this device by Cognito.
unsubscribeFromDataset_deviceId :: Lens.Lens' UnsubscribeFromDataset Prelude.Text
unsubscribeFromDataset_deviceId = Lens.lens (\UnsubscribeFromDataset' {deviceId} -> deviceId) (\s@UnsubscribeFromDataset' {} a -> s {deviceId = a} :: UnsubscribeFromDataset)

instance Core.AWSRequest UnsubscribeFromDataset where
  type
    AWSResponse UnsubscribeFromDataset =
      UnsubscribeFromDatasetResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UnsubscribeFromDatasetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UnsubscribeFromDataset where
  hashWithSalt _salt UnsubscribeFromDataset' {..} =
    _salt
      `Prelude.hashWithSalt` identityPoolId
      `Prelude.hashWithSalt` identityId
      `Prelude.hashWithSalt` datasetName
      `Prelude.hashWithSalt` deviceId

instance Prelude.NFData UnsubscribeFromDataset where
  rnf UnsubscribeFromDataset' {..} =
    Prelude.rnf identityPoolId
      `Prelude.seq` Prelude.rnf identityId
      `Prelude.seq` Prelude.rnf datasetName
      `Prelude.seq` Prelude.rnf deviceId

instance Data.ToHeaders UnsubscribeFromDataset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath UnsubscribeFromDataset where
  toPath UnsubscribeFromDataset' {..} =
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

instance Data.ToQuery UnsubscribeFromDataset where
  toQuery = Prelude.const Prelude.mempty

-- | Response to an UnsubscribeFromDataset request.
--
-- /See:/ 'newUnsubscribeFromDatasetResponse' smart constructor.
data UnsubscribeFromDatasetResponse = UnsubscribeFromDatasetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UnsubscribeFromDatasetResponse
newUnsubscribeFromDatasetResponse pHttpStatus_ =
  UnsubscribeFromDatasetResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
unsubscribeFromDatasetResponse_httpStatus :: Lens.Lens' UnsubscribeFromDatasetResponse Prelude.Int
unsubscribeFromDatasetResponse_httpStatus = Lens.lens (\UnsubscribeFromDatasetResponse' {httpStatus} -> httpStatus) (\s@UnsubscribeFromDatasetResponse' {} a -> s {httpStatus = a} :: UnsubscribeFromDatasetResponse)

instance
  Prelude.NFData
    UnsubscribeFromDatasetResponse
  where
  rnf UnsubscribeFromDatasetResponse' {..} =
    Prelude.rnf httpStatus
