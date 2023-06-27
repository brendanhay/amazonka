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
-- Module      : Amazonka.Translate.UpdateParallelData
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a previously created parallel data resource by importing a new
-- input file from Amazon S3.
module Amazonka.Translate.UpdateParallelData
  ( -- * Creating a Request
    UpdateParallelData (..),
    newUpdateParallelData,

    -- * Request Lenses
    updateParallelData_description,
    updateParallelData_name,
    updateParallelData_parallelDataConfig,
    updateParallelData_clientToken,

    -- * Destructuring the Response
    UpdateParallelDataResponse (..),
    newUpdateParallelDataResponse,

    -- * Response Lenses
    updateParallelDataResponse_latestUpdateAttemptAt,
    updateParallelDataResponse_latestUpdateAttemptStatus,
    updateParallelDataResponse_name,
    updateParallelDataResponse_status,
    updateParallelDataResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Translate.Types

-- | /See:/ 'newUpdateParallelData' smart constructor.
data UpdateParallelData = UpdateParallelData'
  { -- | A custom description for the parallel data resource in Amazon Translate.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the parallel data resource being updated.
    name :: Prelude.Text,
    -- | Specifies the format and S3 location of the parallel data input file.
    parallelDataConfig :: ParallelDataConfig,
    -- | A unique identifier for the request. This token is automatically
    -- generated when you use Amazon Translate through an AWS SDK.
    clientToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateParallelData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateParallelData_description' - A custom description for the parallel data resource in Amazon Translate.
--
-- 'name', 'updateParallelData_name' - The name of the parallel data resource being updated.
--
-- 'parallelDataConfig', 'updateParallelData_parallelDataConfig' - Specifies the format and S3 location of the parallel data input file.
--
-- 'clientToken', 'updateParallelData_clientToken' - A unique identifier for the request. This token is automatically
-- generated when you use Amazon Translate through an AWS SDK.
newUpdateParallelData ::
  -- | 'name'
  Prelude.Text ->
  -- | 'parallelDataConfig'
  ParallelDataConfig ->
  -- | 'clientToken'
  Prelude.Text ->
  UpdateParallelData
newUpdateParallelData
  pName_
  pParallelDataConfig_
  pClientToken_ =
    UpdateParallelData'
      { description = Prelude.Nothing,
        name = pName_,
        parallelDataConfig = pParallelDataConfig_,
        clientToken = pClientToken_
      }

-- | A custom description for the parallel data resource in Amazon Translate.
updateParallelData_description :: Lens.Lens' UpdateParallelData (Prelude.Maybe Prelude.Text)
updateParallelData_description = Lens.lens (\UpdateParallelData' {description} -> description) (\s@UpdateParallelData' {} a -> s {description = a} :: UpdateParallelData)

-- | The name of the parallel data resource being updated.
updateParallelData_name :: Lens.Lens' UpdateParallelData Prelude.Text
updateParallelData_name = Lens.lens (\UpdateParallelData' {name} -> name) (\s@UpdateParallelData' {} a -> s {name = a} :: UpdateParallelData)

-- | Specifies the format and S3 location of the parallel data input file.
updateParallelData_parallelDataConfig :: Lens.Lens' UpdateParallelData ParallelDataConfig
updateParallelData_parallelDataConfig = Lens.lens (\UpdateParallelData' {parallelDataConfig} -> parallelDataConfig) (\s@UpdateParallelData' {} a -> s {parallelDataConfig = a} :: UpdateParallelData)

-- | A unique identifier for the request. This token is automatically
-- generated when you use Amazon Translate through an AWS SDK.
updateParallelData_clientToken :: Lens.Lens' UpdateParallelData Prelude.Text
updateParallelData_clientToken = Lens.lens (\UpdateParallelData' {clientToken} -> clientToken) (\s@UpdateParallelData' {} a -> s {clientToken = a} :: UpdateParallelData)

instance Core.AWSRequest UpdateParallelData where
  type
    AWSResponse UpdateParallelData =
      UpdateParallelDataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateParallelDataResponse'
            Prelude.<$> (x Data..?> "LatestUpdateAttemptAt")
            Prelude.<*> (x Data..?> "LatestUpdateAttemptStatus")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateParallelData where
  hashWithSalt _salt UpdateParallelData' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` parallelDataConfig
      `Prelude.hashWithSalt` clientToken

instance Prelude.NFData UpdateParallelData where
  rnf UpdateParallelData' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf parallelDataConfig
      `Prelude.seq` Prelude.rnf clientToken

instance Data.ToHeaders UpdateParallelData where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSShineFrontendService_20170701.UpdateParallelData" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateParallelData where
  toJSON UpdateParallelData' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just
              ("ParallelDataConfig" Data..= parallelDataConfig),
            Prelude.Just ("ClientToken" Data..= clientToken)
          ]
      )

instance Data.ToPath UpdateParallelData where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateParallelData where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateParallelDataResponse' smart constructor.
data UpdateParallelDataResponse = UpdateParallelDataResponse'
  { -- | The time that the most recent update was attempted.
    latestUpdateAttemptAt :: Prelude.Maybe Data.POSIX,
    -- | The status of the parallel data update attempt. When the updated
    -- parallel data resource is ready for you to use, the status is @ACTIVE@.
    latestUpdateAttemptStatus :: Prelude.Maybe ParallelDataStatus,
    -- | The name of the parallel data resource being updated.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the parallel data resource that you are attempting to
    -- update. Your update request is accepted only if this status is either
    -- @ACTIVE@ or @FAILED@.
    status :: Prelude.Maybe ParallelDataStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateParallelDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latestUpdateAttemptAt', 'updateParallelDataResponse_latestUpdateAttemptAt' - The time that the most recent update was attempted.
--
-- 'latestUpdateAttemptStatus', 'updateParallelDataResponse_latestUpdateAttemptStatus' - The status of the parallel data update attempt. When the updated
-- parallel data resource is ready for you to use, the status is @ACTIVE@.
--
-- 'name', 'updateParallelDataResponse_name' - The name of the parallel data resource being updated.
--
-- 'status', 'updateParallelDataResponse_status' - The status of the parallel data resource that you are attempting to
-- update. Your update request is accepted only if this status is either
-- @ACTIVE@ or @FAILED@.
--
-- 'httpStatus', 'updateParallelDataResponse_httpStatus' - The response's http status code.
newUpdateParallelDataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateParallelDataResponse
newUpdateParallelDataResponse pHttpStatus_ =
  UpdateParallelDataResponse'
    { latestUpdateAttemptAt =
        Prelude.Nothing,
      latestUpdateAttemptStatus = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The time that the most recent update was attempted.
updateParallelDataResponse_latestUpdateAttemptAt :: Lens.Lens' UpdateParallelDataResponse (Prelude.Maybe Prelude.UTCTime)
updateParallelDataResponse_latestUpdateAttemptAt = Lens.lens (\UpdateParallelDataResponse' {latestUpdateAttemptAt} -> latestUpdateAttemptAt) (\s@UpdateParallelDataResponse' {} a -> s {latestUpdateAttemptAt = a} :: UpdateParallelDataResponse) Prelude.. Lens.mapping Data._Time

-- | The status of the parallel data update attempt. When the updated
-- parallel data resource is ready for you to use, the status is @ACTIVE@.
updateParallelDataResponse_latestUpdateAttemptStatus :: Lens.Lens' UpdateParallelDataResponse (Prelude.Maybe ParallelDataStatus)
updateParallelDataResponse_latestUpdateAttemptStatus = Lens.lens (\UpdateParallelDataResponse' {latestUpdateAttemptStatus} -> latestUpdateAttemptStatus) (\s@UpdateParallelDataResponse' {} a -> s {latestUpdateAttemptStatus = a} :: UpdateParallelDataResponse)

-- | The name of the parallel data resource being updated.
updateParallelDataResponse_name :: Lens.Lens' UpdateParallelDataResponse (Prelude.Maybe Prelude.Text)
updateParallelDataResponse_name = Lens.lens (\UpdateParallelDataResponse' {name} -> name) (\s@UpdateParallelDataResponse' {} a -> s {name = a} :: UpdateParallelDataResponse)

-- | The status of the parallel data resource that you are attempting to
-- update. Your update request is accepted only if this status is either
-- @ACTIVE@ or @FAILED@.
updateParallelDataResponse_status :: Lens.Lens' UpdateParallelDataResponse (Prelude.Maybe ParallelDataStatus)
updateParallelDataResponse_status = Lens.lens (\UpdateParallelDataResponse' {status} -> status) (\s@UpdateParallelDataResponse' {} a -> s {status = a} :: UpdateParallelDataResponse)

-- | The response's http status code.
updateParallelDataResponse_httpStatus :: Lens.Lens' UpdateParallelDataResponse Prelude.Int
updateParallelDataResponse_httpStatus = Lens.lens (\UpdateParallelDataResponse' {httpStatus} -> httpStatus) (\s@UpdateParallelDataResponse' {} a -> s {httpStatus = a} :: UpdateParallelDataResponse)

instance Prelude.NFData UpdateParallelDataResponse where
  rnf UpdateParallelDataResponse' {..} =
    Prelude.rnf latestUpdateAttemptAt
      `Prelude.seq` Prelude.rnf latestUpdateAttemptStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
