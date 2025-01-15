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
-- Module      : Amazonka.Location.PutGeofence
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Stores a geofence geometry in a given geofence collection, or updates
-- the geometry of an existing geofence if a geofence ID is included in the
-- request.
module Amazonka.Location.PutGeofence
  ( -- * Creating a Request
    PutGeofence (..),
    newPutGeofence,

    -- * Request Lenses
    putGeofence_collectionName,
    putGeofence_geofenceId,
    putGeofence_geometry,

    -- * Destructuring the Response
    PutGeofenceResponse (..),
    newPutGeofenceResponse,

    -- * Response Lenses
    putGeofenceResponse_httpStatus,
    putGeofenceResponse_createTime,
    putGeofenceResponse_geofenceId,
    putGeofenceResponse_updateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newPutGeofence' smart constructor.
data PutGeofence = PutGeofence'
  { -- | The geofence collection to store the geofence in.
    collectionName :: Prelude.Text,
    -- | An identifier for the geofence. For example, @ExampleGeofence-1@.
    geofenceId :: Prelude.Text,
    -- | Contains the details to specify the position of the geofence. Can be
    -- either a polygon or a circle. Including both will return a validation
    -- error.
    --
    -- Each
    -- <https://docs.aws.amazon.com/location-geofences/latest/APIReference/API_GeofenceGeometry.html geofence polygon>
    -- can have a maximum of 1,000 vertices.
    geometry :: GeofenceGeometry
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutGeofence' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collectionName', 'putGeofence_collectionName' - The geofence collection to store the geofence in.
--
-- 'geofenceId', 'putGeofence_geofenceId' - An identifier for the geofence. For example, @ExampleGeofence-1@.
--
-- 'geometry', 'putGeofence_geometry' - Contains the details to specify the position of the geofence. Can be
-- either a polygon or a circle. Including both will return a validation
-- error.
--
-- Each
-- <https://docs.aws.amazon.com/location-geofences/latest/APIReference/API_GeofenceGeometry.html geofence polygon>
-- can have a maximum of 1,000 vertices.
newPutGeofence ::
  -- | 'collectionName'
  Prelude.Text ->
  -- | 'geofenceId'
  Prelude.Text ->
  -- | 'geometry'
  GeofenceGeometry ->
  PutGeofence
newPutGeofence
  pCollectionName_
  pGeofenceId_
  pGeometry_ =
    PutGeofence'
      { collectionName = pCollectionName_,
        geofenceId = pGeofenceId_,
        geometry = pGeometry_
      }

-- | The geofence collection to store the geofence in.
putGeofence_collectionName :: Lens.Lens' PutGeofence Prelude.Text
putGeofence_collectionName = Lens.lens (\PutGeofence' {collectionName} -> collectionName) (\s@PutGeofence' {} a -> s {collectionName = a} :: PutGeofence)

-- | An identifier for the geofence. For example, @ExampleGeofence-1@.
putGeofence_geofenceId :: Lens.Lens' PutGeofence Prelude.Text
putGeofence_geofenceId = Lens.lens (\PutGeofence' {geofenceId} -> geofenceId) (\s@PutGeofence' {} a -> s {geofenceId = a} :: PutGeofence)

-- | Contains the details to specify the position of the geofence. Can be
-- either a polygon or a circle. Including both will return a validation
-- error.
--
-- Each
-- <https://docs.aws.amazon.com/location-geofences/latest/APIReference/API_GeofenceGeometry.html geofence polygon>
-- can have a maximum of 1,000 vertices.
putGeofence_geometry :: Lens.Lens' PutGeofence GeofenceGeometry
putGeofence_geometry = Lens.lens (\PutGeofence' {geometry} -> geometry) (\s@PutGeofence' {} a -> s {geometry = a} :: PutGeofence)

instance Core.AWSRequest PutGeofence where
  type AWSResponse PutGeofence = PutGeofenceResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          PutGeofenceResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CreateTime")
            Prelude.<*> (x Data..:> "GeofenceId")
            Prelude.<*> (x Data..:> "UpdateTime")
      )

instance Prelude.Hashable PutGeofence where
  hashWithSalt _salt PutGeofence' {..} =
    _salt
      `Prelude.hashWithSalt` collectionName
      `Prelude.hashWithSalt` geofenceId
      `Prelude.hashWithSalt` geometry

instance Prelude.NFData PutGeofence where
  rnf PutGeofence' {..} =
    Prelude.rnf collectionName `Prelude.seq`
      Prelude.rnf geofenceId `Prelude.seq`
        Prelude.rnf geometry

instance Data.ToHeaders PutGeofence where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutGeofence where
  toJSON PutGeofence' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Geometry" Data..= geometry)]
      )

instance Data.ToPath PutGeofence where
  toPath PutGeofence' {..} =
    Prelude.mconcat
      [ "/geofencing/v0/collections/",
        Data.toBS collectionName,
        "/geofences/",
        Data.toBS geofenceId
      ]

instance Data.ToQuery PutGeofence where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutGeofenceResponse' smart constructor.
data PutGeofenceResponse = PutGeofenceResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The timestamp for when the geofence was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@
    createTime :: Data.ISO8601,
    -- | The geofence identifier entered in the request.
    geofenceId :: Prelude.Text,
    -- | The timestamp for when the geofence was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutGeofenceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'putGeofenceResponse_httpStatus' - The response's http status code.
--
-- 'createTime', 'putGeofenceResponse_createTime' - The timestamp for when the geofence was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
--
-- 'geofenceId', 'putGeofenceResponse_geofenceId' - The geofence identifier entered in the request.
--
-- 'updateTime', 'putGeofenceResponse_updateTime' - The timestamp for when the geofence was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
newPutGeofenceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'geofenceId'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  PutGeofenceResponse
newPutGeofenceResponse
  pHttpStatus_
  pCreateTime_
  pGeofenceId_
  pUpdateTime_ =
    PutGeofenceResponse'
      { httpStatus = pHttpStatus_,
        createTime = Data._Time Lens.# pCreateTime_,
        geofenceId = pGeofenceId_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | The response's http status code.
putGeofenceResponse_httpStatus :: Lens.Lens' PutGeofenceResponse Prelude.Int
putGeofenceResponse_httpStatus = Lens.lens (\PutGeofenceResponse' {httpStatus} -> httpStatus) (\s@PutGeofenceResponse' {} a -> s {httpStatus = a} :: PutGeofenceResponse)

-- | The timestamp for when the geofence was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
putGeofenceResponse_createTime :: Lens.Lens' PutGeofenceResponse Prelude.UTCTime
putGeofenceResponse_createTime = Lens.lens (\PutGeofenceResponse' {createTime} -> createTime) (\s@PutGeofenceResponse' {} a -> s {createTime = a} :: PutGeofenceResponse) Prelude.. Data._Time

-- | The geofence identifier entered in the request.
putGeofenceResponse_geofenceId :: Lens.Lens' PutGeofenceResponse Prelude.Text
putGeofenceResponse_geofenceId = Lens.lens (\PutGeofenceResponse' {geofenceId} -> geofenceId) (\s@PutGeofenceResponse' {} a -> s {geofenceId = a} :: PutGeofenceResponse)

-- | The timestamp for when the geofence was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@
putGeofenceResponse_updateTime :: Lens.Lens' PutGeofenceResponse Prelude.UTCTime
putGeofenceResponse_updateTime = Lens.lens (\PutGeofenceResponse' {updateTime} -> updateTime) (\s@PutGeofenceResponse' {} a -> s {updateTime = a} :: PutGeofenceResponse) Prelude.. Data._Time

instance Prelude.NFData PutGeofenceResponse where
  rnf PutGeofenceResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf createTime `Prelude.seq`
        Prelude.rnf geofenceId `Prelude.seq`
          Prelude.rnf updateTime
