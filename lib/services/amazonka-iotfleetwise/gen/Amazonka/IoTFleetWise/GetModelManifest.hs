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
-- Module      : Amazonka.IoTFleetWise.GetModelManifest
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a vehicle model (model manifest).
module Amazonka.IoTFleetWise.GetModelManifest
  ( -- * Creating a Request
    GetModelManifest (..),
    newGetModelManifest,

    -- * Request Lenses
    getModelManifest_name,

    -- * Destructuring the Response
    GetModelManifestResponse (..),
    newGetModelManifestResponse,

    -- * Response Lenses
    getModelManifestResponse_description,
    getModelManifestResponse_signalCatalogArn,
    getModelManifestResponse_status,
    getModelManifestResponse_httpStatus,
    getModelManifestResponse_name,
    getModelManifestResponse_arn,
    getModelManifestResponse_creationTime,
    getModelManifestResponse_lastModificationTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTFleetWise.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetModelManifest' smart constructor.
data GetModelManifest = GetModelManifest'
  { -- | The name of the vehicle model to retrieve information about.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetModelManifest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getModelManifest_name' - The name of the vehicle model to retrieve information about.
newGetModelManifest ::
  -- | 'name'
  Prelude.Text ->
  GetModelManifest
newGetModelManifest pName_ =
  GetModelManifest' {name = pName_}

-- | The name of the vehicle model to retrieve information about.
getModelManifest_name :: Lens.Lens' GetModelManifest Prelude.Text
getModelManifest_name = Lens.lens (\GetModelManifest' {name} -> name) (\s@GetModelManifest' {} a -> s {name = a} :: GetModelManifest)

instance Core.AWSRequest GetModelManifest where
  type
    AWSResponse GetModelManifest =
      GetModelManifestResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetModelManifestResponse'
            Prelude.<$> (x Data..?> "description")
            Prelude.<*> (x Data..?> "signalCatalogArn")
            Prelude.<*> (x Data..?> "status")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "name")
            Prelude.<*> (x Data..:> "arn")
            Prelude.<*> (x Data..:> "creationTime")
            Prelude.<*> (x Data..:> "lastModificationTime")
      )

instance Prelude.Hashable GetModelManifest where
  hashWithSalt _salt GetModelManifest' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetModelManifest where
  rnf GetModelManifest' {..} = Prelude.rnf name

instance Data.ToHeaders GetModelManifest where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "IoTAutobahnControlPlane.GetModelManifest" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetModelManifest where
  toJSON GetModelManifest' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )

instance Data.ToPath GetModelManifest where
  toPath = Prelude.const "/"

instance Data.ToQuery GetModelManifest where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetModelManifestResponse' smart constructor.
data GetModelManifestResponse = GetModelManifestResponse'
  { -- | A brief description of the vehicle model.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the signal catalog associated with the vehicle model.
    signalCatalogArn :: Prelude.Maybe Prelude.Text,
    -- | The state of the vehicle model. If the status is @ACTIVE@, the vehicle
    -- model can\'t be edited. You can edit the vehicle model if the status is
    -- marked @DRAFT@.
    status :: Prelude.Maybe ManifestStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the vehicle model.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the vehicle model.
    arn :: Prelude.Text,
    -- | The time the vehicle model was created, in seconds since epoch (January
    -- 1, 1970 at midnight UTC time).
    creationTime :: Data.POSIX,
    -- | The last time the vehicle model was modified.
    lastModificationTime :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetModelManifestResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'getModelManifestResponse_description' - A brief description of the vehicle model.
--
-- 'signalCatalogArn', 'getModelManifestResponse_signalCatalogArn' - The ARN of the signal catalog associated with the vehicle model.
--
-- 'status', 'getModelManifestResponse_status' - The state of the vehicle model. If the status is @ACTIVE@, the vehicle
-- model can\'t be edited. You can edit the vehicle model if the status is
-- marked @DRAFT@.
--
-- 'httpStatus', 'getModelManifestResponse_httpStatus' - The response's http status code.
--
-- 'name', 'getModelManifestResponse_name' - The name of the vehicle model.
--
-- 'arn', 'getModelManifestResponse_arn' - The Amazon Resource Name (ARN) of the vehicle model.
--
-- 'creationTime', 'getModelManifestResponse_creationTime' - The time the vehicle model was created, in seconds since epoch (January
-- 1, 1970 at midnight UTC time).
--
-- 'lastModificationTime', 'getModelManifestResponse_lastModificationTime' - The last time the vehicle model was modified.
newGetModelManifestResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'lastModificationTime'
  Prelude.UTCTime ->
  GetModelManifestResponse
newGetModelManifestResponse
  pHttpStatus_
  pName_
  pArn_
  pCreationTime_
  pLastModificationTime_ =
    GetModelManifestResponse'
      { description =
          Prelude.Nothing,
        signalCatalogArn = Prelude.Nothing,
        status = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        name = pName_,
        arn = pArn_,
        creationTime = Data._Time Lens.# pCreationTime_,
        lastModificationTime =
          Data._Time Lens.# pLastModificationTime_
      }

-- | A brief description of the vehicle model.
getModelManifestResponse_description :: Lens.Lens' GetModelManifestResponse (Prelude.Maybe Prelude.Text)
getModelManifestResponse_description = Lens.lens (\GetModelManifestResponse' {description} -> description) (\s@GetModelManifestResponse' {} a -> s {description = a} :: GetModelManifestResponse)

-- | The ARN of the signal catalog associated with the vehicle model.
getModelManifestResponse_signalCatalogArn :: Lens.Lens' GetModelManifestResponse (Prelude.Maybe Prelude.Text)
getModelManifestResponse_signalCatalogArn = Lens.lens (\GetModelManifestResponse' {signalCatalogArn} -> signalCatalogArn) (\s@GetModelManifestResponse' {} a -> s {signalCatalogArn = a} :: GetModelManifestResponse)

-- | The state of the vehicle model. If the status is @ACTIVE@, the vehicle
-- model can\'t be edited. You can edit the vehicle model if the status is
-- marked @DRAFT@.
getModelManifestResponse_status :: Lens.Lens' GetModelManifestResponse (Prelude.Maybe ManifestStatus)
getModelManifestResponse_status = Lens.lens (\GetModelManifestResponse' {status} -> status) (\s@GetModelManifestResponse' {} a -> s {status = a} :: GetModelManifestResponse)

-- | The response's http status code.
getModelManifestResponse_httpStatus :: Lens.Lens' GetModelManifestResponse Prelude.Int
getModelManifestResponse_httpStatus = Lens.lens (\GetModelManifestResponse' {httpStatus} -> httpStatus) (\s@GetModelManifestResponse' {} a -> s {httpStatus = a} :: GetModelManifestResponse)

-- | The name of the vehicle model.
getModelManifestResponse_name :: Lens.Lens' GetModelManifestResponse Prelude.Text
getModelManifestResponse_name = Lens.lens (\GetModelManifestResponse' {name} -> name) (\s@GetModelManifestResponse' {} a -> s {name = a} :: GetModelManifestResponse)

-- | The Amazon Resource Name (ARN) of the vehicle model.
getModelManifestResponse_arn :: Lens.Lens' GetModelManifestResponse Prelude.Text
getModelManifestResponse_arn = Lens.lens (\GetModelManifestResponse' {arn} -> arn) (\s@GetModelManifestResponse' {} a -> s {arn = a} :: GetModelManifestResponse)

-- | The time the vehicle model was created, in seconds since epoch (January
-- 1, 1970 at midnight UTC time).
getModelManifestResponse_creationTime :: Lens.Lens' GetModelManifestResponse Prelude.UTCTime
getModelManifestResponse_creationTime = Lens.lens (\GetModelManifestResponse' {creationTime} -> creationTime) (\s@GetModelManifestResponse' {} a -> s {creationTime = a} :: GetModelManifestResponse) Prelude.. Data._Time

-- | The last time the vehicle model was modified.
getModelManifestResponse_lastModificationTime :: Lens.Lens' GetModelManifestResponse Prelude.UTCTime
getModelManifestResponse_lastModificationTime = Lens.lens (\GetModelManifestResponse' {lastModificationTime} -> lastModificationTime) (\s@GetModelManifestResponse' {} a -> s {lastModificationTime = a} :: GetModelManifestResponse) Prelude.. Data._Time

instance Prelude.NFData GetModelManifestResponse where
  rnf GetModelManifestResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf signalCatalogArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf lastModificationTime
