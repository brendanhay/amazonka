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
-- Module      : Amazonka.Location.GetGeofence
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the geofence details from a geofence collection.
module Amazonka.Location.GetGeofence
  ( -- * Creating a Request
    GetGeofence (..),
    newGetGeofence,

    -- * Request Lenses
    getGeofence_collectionName,
    getGeofence_geofenceId,

    -- * Destructuring the Response
    GetGeofenceResponse (..),
    newGetGeofenceResponse,

    -- * Response Lenses
    getGeofenceResponse_httpStatus,
    getGeofenceResponse_createTime,
    getGeofenceResponse_geofenceId,
    getGeofenceResponse_geometry,
    getGeofenceResponse_status,
    getGeofenceResponse_updateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetGeofence' smart constructor.
data GetGeofence = GetGeofence'
  { -- | The geofence collection storing the target geofence.
    collectionName :: Prelude.Text,
    -- | The geofence you\'re retrieving details for.
    geofenceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGeofence' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionName', 'getGeofence_collectionName' - The geofence collection storing the target geofence.
--
-- 'geofenceId', 'getGeofence_geofenceId' - The geofence you\'re retrieving details for.
newGetGeofence ::
  -- | 'collectionName'
  Prelude.Text ->
  -- | 'geofenceId'
  Prelude.Text ->
  GetGeofence
newGetGeofence pCollectionName_ pGeofenceId_ =
  GetGeofence'
    { collectionName = pCollectionName_,
      geofenceId = pGeofenceId_
    }

-- | The geofence collection storing the target geofence.
getGeofence_collectionName :: Lens.Lens' GetGeofence Prelude.Text
getGeofence_collectionName = Lens.lens (\GetGeofence' {collectionName} -> collectionName) (\s@GetGeofence' {} a -> s {collectionName = a} :: GetGeofence)

-- | The geofence you\'re retrieving details for.
getGeofence_geofenceId :: Lens.Lens' GetGeofence Prelude.Text
getGeofence_geofenceId = Lens.lens (\GetGeofence' {geofenceId} -> geofenceId) (\s@GetGeofence' {} a -> s {geofenceId = a} :: GetGeofence)

instance Core.AWSRequest GetGeofence where
  type AWSResponse GetGeofence = GetGeofenceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetGeofenceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CreateTime")
            Prelude.<*> (x Data..:> "GeofenceId")
            Prelude.<*> (x Data..:> "Geometry")
            Prelude.<*> (x Data..:> "Status")
            Prelude.<*> (x Data..:> "UpdateTime")
      )

instance Prelude.Hashable GetGeofence where
  hashWithSalt _salt GetGeofence' {..} =
    _salt
      `Prelude.hashWithSalt` collectionName
      `Prelude.hashWithSalt` geofenceId

instance Prelude.NFData GetGeofence where
  rnf GetGeofence' {..} =
    Prelude.rnf collectionName
      `Prelude.seq` Prelude.rnf geofenceId

instance Data.ToHeaders GetGeofence where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetGeofence where
  toPath GetGeofence' {..} =
    Prelude.mconcat
      [ "/geofencing/v0/collections/",
        Data.toBS collectionName,
        "/geofences/",
        Data.toBS geofenceId
      ]

instance Data.ToQuery GetGeofence where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetGeofenceResponse' smart constructor.
data GetGeofenceResponse = GetGeofenceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The timestamp for when the geofence collection was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@
    createTime :: Data.ISO8601,
    -- | The geofence identifier.
    geofenceId :: Prelude.Text,
    -- | Contains the geofence geometry details describing a polygon or a circle.
    geometry :: GeofenceGeometry,
    -- | Identifies the state of the geofence. A geofence will hold one of the
    -- following states:
    --
    -- -   @ACTIVE@ — The geofence has been indexed by the system.
    --
    -- -   @PENDING@ — The geofence is being processed by the system.
    --
    -- -   @FAILED@ — The geofence failed to be indexed by the system.
    --
    -- -   @DELETED@ — The geofence has been deleted from the system index.
    --
    -- -   @DELETING@ — The geofence is being deleted from the system index.
    status :: Prelude.Text,
    -- | The timestamp for when the geofence collection was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetGeofenceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getGeofenceResponse_httpStatus' - The response's http status code.
--
-- 'createTime', 'getGeofenceResponse_createTime' - The timestamp for when the geofence collection was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
--
-- 'geofenceId', 'getGeofenceResponse_geofenceId' - The geofence identifier.
--
-- 'geometry', 'getGeofenceResponse_geometry' - Contains the geofence geometry details describing a polygon or a circle.
--
-- 'status', 'getGeofenceResponse_status' - Identifies the state of the geofence. A geofence will hold one of the
-- following states:
--
-- -   @ACTIVE@ — The geofence has been indexed by the system.
--
-- -   @PENDING@ — The geofence is being processed by the system.
--
-- -   @FAILED@ — The geofence failed to be indexed by the system.
--
-- -   @DELETED@ — The geofence has been deleted from the system index.
--
-- -   @DELETING@ — The geofence is being deleted from the system index.
--
-- 'updateTime', 'getGeofenceResponse_updateTime' - The timestamp for when the geofence collection was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
newGetGeofenceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'geofenceId'
  Prelude.Text ->
  -- | 'geometry'
  GeofenceGeometry ->
  -- | 'status'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  GetGeofenceResponse
newGetGeofenceResponse
  pHttpStatus_
  pCreateTime_
  pGeofenceId_
  pGeometry_
  pStatus_
  pUpdateTime_ =
    GetGeofenceResponse'
      { httpStatus = pHttpStatus_,
        createTime = Data._Time Lens.# pCreateTime_,
        geofenceId = pGeofenceId_,
        geometry = pGeometry_,
        status = pStatus_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | The response's http status code.
getGeofenceResponse_httpStatus :: Lens.Lens' GetGeofenceResponse Prelude.Int
getGeofenceResponse_httpStatus = Lens.lens (\GetGeofenceResponse' {httpStatus} -> httpStatus) (\s@GetGeofenceResponse' {} a -> s {httpStatus = a} :: GetGeofenceResponse)

-- | The timestamp for when the geofence collection was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
getGeofenceResponse_createTime :: Lens.Lens' GetGeofenceResponse Prelude.UTCTime
getGeofenceResponse_createTime = Lens.lens (\GetGeofenceResponse' {createTime} -> createTime) (\s@GetGeofenceResponse' {} a -> s {createTime = a} :: GetGeofenceResponse) Prelude.. Data._Time

-- | The geofence identifier.
getGeofenceResponse_geofenceId :: Lens.Lens' GetGeofenceResponse Prelude.Text
getGeofenceResponse_geofenceId = Lens.lens (\GetGeofenceResponse' {geofenceId} -> geofenceId) (\s@GetGeofenceResponse' {} a -> s {geofenceId = a} :: GetGeofenceResponse)

-- | Contains the geofence geometry details describing a polygon or a circle.
getGeofenceResponse_geometry :: Lens.Lens' GetGeofenceResponse GeofenceGeometry
getGeofenceResponse_geometry = Lens.lens (\GetGeofenceResponse' {geometry} -> geometry) (\s@GetGeofenceResponse' {} a -> s {geometry = a} :: GetGeofenceResponse)

-- | Identifies the state of the geofence. A geofence will hold one of the
-- following states:
--
-- -   @ACTIVE@ — The geofence has been indexed by the system.
--
-- -   @PENDING@ — The geofence is being processed by the system.
--
-- -   @FAILED@ — The geofence failed to be indexed by the system.
--
-- -   @DELETED@ — The geofence has been deleted from the system index.
--
-- -   @DELETING@ — The geofence is being deleted from the system index.
getGeofenceResponse_status :: Lens.Lens' GetGeofenceResponse Prelude.Text
getGeofenceResponse_status = Lens.lens (\GetGeofenceResponse' {status} -> status) (\s@GetGeofenceResponse' {} a -> s {status = a} :: GetGeofenceResponse)

-- | The timestamp for when the geofence collection was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
getGeofenceResponse_updateTime :: Lens.Lens' GetGeofenceResponse Prelude.UTCTime
getGeofenceResponse_updateTime = Lens.lens (\GetGeofenceResponse' {updateTime} -> updateTime) (\s@GetGeofenceResponse' {} a -> s {updateTime = a} :: GetGeofenceResponse) Prelude.. Data._Time

instance Prelude.NFData GetGeofenceResponse where
  rnf GetGeofenceResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf geofenceId
      `Prelude.seq` Prelude.rnf geometry
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updateTime
