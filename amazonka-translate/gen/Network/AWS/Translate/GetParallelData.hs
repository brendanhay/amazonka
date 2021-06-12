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
-- Module      : Network.AWS.Translate.GetParallelData
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about a parallel data resource.
module Network.AWS.Translate.GetParallelData
  ( -- * Creating a Request
    GetParallelData (..),
    newGetParallelData,

    -- * Request Lenses
    getParallelData_name,

    -- * Destructuring the Response
    GetParallelDataResponse (..),
    newGetParallelDataResponse,

    -- * Response Lenses
    getParallelDataResponse_auxiliaryDataLocation,
    getParallelDataResponse_parallelDataProperties,
    getParallelDataResponse_latestUpdateAttemptAuxiliaryDataLocation,
    getParallelDataResponse_dataLocation,
    getParallelDataResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.Translate.Types

-- | /See:/ 'newGetParallelData' smart constructor.
data GetParallelData = GetParallelData'
  { -- | The name of the parallel data resource that is being retrieved.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetParallelData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getParallelData_name' - The name of the parallel data resource that is being retrieved.
newGetParallelData ::
  -- | 'name'
  Core.Text ->
  GetParallelData
newGetParallelData pName_ =
  GetParallelData' {name = pName_}

-- | The name of the parallel data resource that is being retrieved.
getParallelData_name :: Lens.Lens' GetParallelData Core.Text
getParallelData_name = Lens.lens (\GetParallelData' {name} -> name) (\s@GetParallelData' {} a -> s {name = a} :: GetParallelData)

instance Core.AWSRequest GetParallelData where
  type
    AWSResponse GetParallelData =
      GetParallelDataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetParallelDataResponse'
            Core.<$> (x Core..?> "AuxiliaryDataLocation")
            Core.<*> (x Core..?> "ParallelDataProperties")
            Core.<*> ( x
                         Core..?> "LatestUpdateAttemptAuxiliaryDataLocation"
                     )
            Core.<*> (x Core..?> "DataLocation")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetParallelData

instance Core.NFData GetParallelData

instance Core.ToHeaders GetParallelData where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSShineFrontendService_20170701.GetParallelData" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetParallelData where
  toJSON GetParallelData' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath GetParallelData where
  toPath = Core.const "/"

instance Core.ToQuery GetParallelData where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetParallelDataResponse' smart constructor.
data GetParallelDataResponse = GetParallelDataResponse'
  { -- | The Amazon S3 location of a file that provides any errors or warnings
    -- that were produced by your input file. This file was created when Amazon
    -- Translate attempted to create a parallel data resource. The location is
    -- returned as a presigned URL to that has a 30 minute expiration.
    auxiliaryDataLocation :: Core.Maybe ParallelDataDataLocation,
    -- | The properties of the parallel data resource that is being retrieved.
    parallelDataProperties :: Core.Maybe ParallelDataProperties,
    -- | The Amazon S3 location of a file that provides any errors or warnings
    -- that were produced by your input file. This file was created when Amazon
    -- Translate attempted to update a parallel data resource. The location is
    -- returned as a presigned URL to that has a 30 minute expiration.
    latestUpdateAttemptAuxiliaryDataLocation :: Core.Maybe ParallelDataDataLocation,
    -- | The location of the most recent parallel data input file that was
    -- successfully imported into Amazon Translate. The location is returned as
    -- a presigned URL that has a 30 minute expiration.
    dataLocation :: Core.Maybe ParallelDataDataLocation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetParallelDataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'auxiliaryDataLocation', 'getParallelDataResponse_auxiliaryDataLocation' - The Amazon S3 location of a file that provides any errors or warnings
-- that were produced by your input file. This file was created when Amazon
-- Translate attempted to create a parallel data resource. The location is
-- returned as a presigned URL to that has a 30 minute expiration.
--
-- 'parallelDataProperties', 'getParallelDataResponse_parallelDataProperties' - The properties of the parallel data resource that is being retrieved.
--
-- 'latestUpdateAttemptAuxiliaryDataLocation', 'getParallelDataResponse_latestUpdateAttemptAuxiliaryDataLocation' - The Amazon S3 location of a file that provides any errors or warnings
-- that were produced by your input file. This file was created when Amazon
-- Translate attempted to update a parallel data resource. The location is
-- returned as a presigned URL to that has a 30 minute expiration.
--
-- 'dataLocation', 'getParallelDataResponse_dataLocation' - The location of the most recent parallel data input file that was
-- successfully imported into Amazon Translate. The location is returned as
-- a presigned URL that has a 30 minute expiration.
--
-- 'httpStatus', 'getParallelDataResponse_httpStatus' - The response's http status code.
newGetParallelDataResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetParallelDataResponse
newGetParallelDataResponse pHttpStatus_ =
  GetParallelDataResponse'
    { auxiliaryDataLocation =
        Core.Nothing,
      parallelDataProperties = Core.Nothing,
      latestUpdateAttemptAuxiliaryDataLocation =
        Core.Nothing,
      dataLocation = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon S3 location of a file that provides any errors or warnings
-- that were produced by your input file. This file was created when Amazon
-- Translate attempted to create a parallel data resource. The location is
-- returned as a presigned URL to that has a 30 minute expiration.
getParallelDataResponse_auxiliaryDataLocation :: Lens.Lens' GetParallelDataResponse (Core.Maybe ParallelDataDataLocation)
getParallelDataResponse_auxiliaryDataLocation = Lens.lens (\GetParallelDataResponse' {auxiliaryDataLocation} -> auxiliaryDataLocation) (\s@GetParallelDataResponse' {} a -> s {auxiliaryDataLocation = a} :: GetParallelDataResponse)

-- | The properties of the parallel data resource that is being retrieved.
getParallelDataResponse_parallelDataProperties :: Lens.Lens' GetParallelDataResponse (Core.Maybe ParallelDataProperties)
getParallelDataResponse_parallelDataProperties = Lens.lens (\GetParallelDataResponse' {parallelDataProperties} -> parallelDataProperties) (\s@GetParallelDataResponse' {} a -> s {parallelDataProperties = a} :: GetParallelDataResponse)

-- | The Amazon S3 location of a file that provides any errors or warnings
-- that were produced by your input file. This file was created when Amazon
-- Translate attempted to update a parallel data resource. The location is
-- returned as a presigned URL to that has a 30 minute expiration.
getParallelDataResponse_latestUpdateAttemptAuxiliaryDataLocation :: Lens.Lens' GetParallelDataResponse (Core.Maybe ParallelDataDataLocation)
getParallelDataResponse_latestUpdateAttemptAuxiliaryDataLocation = Lens.lens (\GetParallelDataResponse' {latestUpdateAttemptAuxiliaryDataLocation} -> latestUpdateAttemptAuxiliaryDataLocation) (\s@GetParallelDataResponse' {} a -> s {latestUpdateAttemptAuxiliaryDataLocation = a} :: GetParallelDataResponse)

-- | The location of the most recent parallel data input file that was
-- successfully imported into Amazon Translate. The location is returned as
-- a presigned URL that has a 30 minute expiration.
getParallelDataResponse_dataLocation :: Lens.Lens' GetParallelDataResponse (Core.Maybe ParallelDataDataLocation)
getParallelDataResponse_dataLocation = Lens.lens (\GetParallelDataResponse' {dataLocation} -> dataLocation) (\s@GetParallelDataResponse' {} a -> s {dataLocation = a} :: GetParallelDataResponse)

-- | The response's http status code.
getParallelDataResponse_httpStatus :: Lens.Lens' GetParallelDataResponse Core.Int
getParallelDataResponse_httpStatus = Lens.lens (\GetParallelDataResponse' {httpStatus} -> httpStatus) (\s@GetParallelDataResponse' {} a -> s {httpStatus = a} :: GetParallelDataResponse)

instance Core.NFData GetParallelDataResponse
