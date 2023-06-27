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
-- Module      : Amazonka.IoTWireless.UpdateResourcePosition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update the position information of a given wireless device or a wireless
-- gateway resource. The position coordinates are based on the
-- <https://gisgeography.com/wgs84-world-geodetic-system/ World Geodetic System (WGS84)>.
module Amazonka.IoTWireless.UpdateResourcePosition
  ( -- * Creating a Request
    UpdateResourcePosition (..),
    newUpdateResourcePosition,

    -- * Request Lenses
    updateResourcePosition_geoJsonPayload,
    updateResourcePosition_resourceIdentifier,
    updateResourcePosition_resourceType,

    -- * Destructuring the Response
    UpdateResourcePositionResponse (..),
    newUpdateResourcePositionResponse,

    -- * Response Lenses
    updateResourcePositionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateResourcePosition' smart constructor.
data UpdateResourcePosition = UpdateResourcePosition'
  { -- | The position information of the resource, displayed as a JSON payload.
    -- The payload uses the GeoJSON format, which a format that\'s used to
    -- encode geographic data structures. For more information, see
    -- <https://geojson.org/ GeoJSON>.
    geoJsonPayload :: Prelude.Maybe Prelude.ByteString,
    -- | The identifier of the resource for which position information is
    -- updated. It can be the wireless device ID or the wireless gateway ID,
    -- depending on the resource type.
    resourceIdentifier :: Prelude.Text,
    -- | The type of resource for which position information is updated, which
    -- can be a wireless device or a wireless gateway.
    resourceType :: PositionResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourcePosition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'geoJsonPayload', 'updateResourcePosition_geoJsonPayload' - The position information of the resource, displayed as a JSON payload.
-- The payload uses the GeoJSON format, which a format that\'s used to
-- encode geographic data structures. For more information, see
-- <https://geojson.org/ GeoJSON>.
--
-- 'resourceIdentifier', 'updateResourcePosition_resourceIdentifier' - The identifier of the resource for which position information is
-- updated. It can be the wireless device ID or the wireless gateway ID,
-- depending on the resource type.
--
-- 'resourceType', 'updateResourcePosition_resourceType' - The type of resource for which position information is updated, which
-- can be a wireless device or a wireless gateway.
newUpdateResourcePosition ::
  -- | 'resourceIdentifier'
  Prelude.Text ->
  -- | 'resourceType'
  PositionResourceType ->
  UpdateResourcePosition
newUpdateResourcePosition
  pResourceIdentifier_
  pResourceType_ =
    UpdateResourcePosition'
      { geoJsonPayload =
          Prelude.Nothing,
        resourceIdentifier = pResourceIdentifier_,
        resourceType = pResourceType_
      }

-- | The position information of the resource, displayed as a JSON payload.
-- The payload uses the GeoJSON format, which a format that\'s used to
-- encode geographic data structures. For more information, see
-- <https://geojson.org/ GeoJSON>.
updateResourcePosition_geoJsonPayload :: Lens.Lens' UpdateResourcePosition (Prelude.Maybe Prelude.ByteString)
updateResourcePosition_geoJsonPayload = Lens.lens (\UpdateResourcePosition' {geoJsonPayload} -> geoJsonPayload) (\s@UpdateResourcePosition' {} a -> s {geoJsonPayload = a} :: UpdateResourcePosition)

-- | The identifier of the resource for which position information is
-- updated. It can be the wireless device ID or the wireless gateway ID,
-- depending on the resource type.
updateResourcePosition_resourceIdentifier :: Lens.Lens' UpdateResourcePosition Prelude.Text
updateResourcePosition_resourceIdentifier = Lens.lens (\UpdateResourcePosition' {resourceIdentifier} -> resourceIdentifier) (\s@UpdateResourcePosition' {} a -> s {resourceIdentifier = a} :: UpdateResourcePosition)

-- | The type of resource for which position information is updated, which
-- can be a wireless device or a wireless gateway.
updateResourcePosition_resourceType :: Lens.Lens' UpdateResourcePosition PositionResourceType
updateResourcePosition_resourceType = Lens.lens (\UpdateResourcePosition' {resourceType} -> resourceType) (\s@UpdateResourcePosition' {} a -> s {resourceType = a} :: UpdateResourcePosition)

instance Core.AWSRequest UpdateResourcePosition where
  type
    AWSResponse UpdateResourcePosition =
      UpdateResourcePositionResponse
  request overrides =
    Request.patchBody (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateResourcePositionResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateResourcePosition where
  hashWithSalt _salt UpdateResourcePosition' {..} =
    _salt
      `Prelude.hashWithSalt` geoJsonPayload
      `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData UpdateResourcePosition where
  rnf UpdateResourcePosition' {..} =
    Prelude.rnf geoJsonPayload
      `Prelude.seq` Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToBody UpdateResourcePosition where
  toBody UpdateResourcePosition' {..} =
    Data.toBody geoJsonPayload

instance Data.ToHeaders UpdateResourcePosition where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath UpdateResourcePosition where
  toPath UpdateResourcePosition' {..} =
    Prelude.mconcat
      [ "/resource-positions/",
        Data.toBS resourceIdentifier
      ]

instance Data.ToQuery UpdateResourcePosition where
  toQuery UpdateResourcePosition' {..} =
    Prelude.mconcat
      ["resourceType" Data.=: resourceType]

-- | /See:/ 'newUpdateResourcePositionResponse' smart constructor.
data UpdateResourcePositionResponse = UpdateResourcePositionResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResourcePositionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateResourcePositionResponse_httpStatus' - The response's http status code.
newUpdateResourcePositionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateResourcePositionResponse
newUpdateResourcePositionResponse pHttpStatus_ =
  UpdateResourcePositionResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
updateResourcePositionResponse_httpStatus :: Lens.Lens' UpdateResourcePositionResponse Prelude.Int
updateResourcePositionResponse_httpStatus = Lens.lens (\UpdateResourcePositionResponse' {httpStatus} -> httpStatus) (\s@UpdateResourcePositionResponse' {} a -> s {httpStatus = a} :: UpdateResourcePositionResponse)

instance
  Prelude.NFData
    UpdateResourcePositionResponse
  where
  rnf UpdateResourcePositionResponse' {..} =
    Prelude.rnf httpStatus
