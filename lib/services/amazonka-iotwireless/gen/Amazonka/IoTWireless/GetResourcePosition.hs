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
-- Module      : Amazonka.IoTWireless.GetResourcePosition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the position information for a given wireless device or a wireless
-- gateway resource. The postion information uses the
-- <https://gisgeography.com/wgs84-world-geodetic-system/ World Geodetic System (WGS84)>.
module Amazonka.IoTWireless.GetResourcePosition
  ( -- * Creating a Request
    GetResourcePosition (..),
    newGetResourcePosition,

    -- * Request Lenses
    getResourcePosition_resourceIdentifier,
    getResourcePosition_resourceType,

    -- * Destructuring the Response
    GetResourcePositionResponse (..),
    newGetResourcePositionResponse,

    -- * Response Lenses
    getResourcePositionResponse_geoJsonPayload,
    getResourcePositionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResourcePosition' smart constructor.
data GetResourcePosition = GetResourcePosition'
  { -- | The identifier of the resource for which position information is
    -- retrieved. It can be the wireless device ID or the wireless gateway ID
    -- depending on the resource type.
    resourceIdentifier :: Prelude.Text,
    -- | The type of resource for which position information is retrieved, which
    -- can be a wireless device or a wireless gateway.
    resourceType :: PositionResourceType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourcePosition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceIdentifier', 'getResourcePosition_resourceIdentifier' - The identifier of the resource for which position information is
-- retrieved. It can be the wireless device ID or the wireless gateway ID
-- depending on the resource type.
--
-- 'resourceType', 'getResourcePosition_resourceType' - The type of resource for which position information is retrieved, which
-- can be a wireless device or a wireless gateway.
newGetResourcePosition ::
  -- | 'resourceIdentifier'
  Prelude.Text ->
  -- | 'resourceType'
  PositionResourceType ->
  GetResourcePosition
newGetResourcePosition
  pResourceIdentifier_
  pResourceType_ =
    GetResourcePosition'
      { resourceIdentifier =
          pResourceIdentifier_,
        resourceType = pResourceType_
      }

-- | The identifier of the resource for which position information is
-- retrieved. It can be the wireless device ID or the wireless gateway ID
-- depending on the resource type.
getResourcePosition_resourceIdentifier :: Lens.Lens' GetResourcePosition Prelude.Text
getResourcePosition_resourceIdentifier = Lens.lens (\GetResourcePosition' {resourceIdentifier} -> resourceIdentifier) (\s@GetResourcePosition' {} a -> s {resourceIdentifier = a} :: GetResourcePosition)

-- | The type of resource for which position information is retrieved, which
-- can be a wireless device or a wireless gateway.
getResourcePosition_resourceType :: Lens.Lens' GetResourcePosition PositionResourceType
getResourcePosition_resourceType = Lens.lens (\GetResourcePosition' {resourceType} -> resourceType) (\s@GetResourcePosition' {} a -> s {resourceType = a} :: GetResourcePosition)

instance Core.AWSRequest GetResourcePosition where
  type
    AWSResponse GetResourcePosition =
      GetResourcePositionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBytes
      ( \s h x ->
          GetResourcePositionResponse'
            Prelude.<$> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResourcePosition where
  hashWithSalt _salt GetResourcePosition' {..} =
    _salt `Prelude.hashWithSalt` resourceIdentifier
      `Prelude.hashWithSalt` resourceType

instance Prelude.NFData GetResourcePosition where
  rnf GetResourcePosition' {..} =
    Prelude.rnf resourceIdentifier
      `Prelude.seq` Prelude.rnf resourceType

instance Data.ToHeaders GetResourcePosition where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetResourcePosition where
  toPath GetResourcePosition' {..} =
    Prelude.mconcat
      [ "/resource-positions/",
        Data.toBS resourceIdentifier
      ]

instance Data.ToQuery GetResourcePosition where
  toQuery GetResourcePosition' {..} =
    Prelude.mconcat
      ["resourceType" Data.=: resourceType]

-- | /See:/ 'newGetResourcePositionResponse' smart constructor.
data GetResourcePositionResponse = GetResourcePositionResponse'
  { -- | The position information of the resource, displayed as a JSON payload.
    -- The payload uses the GeoJSON format, which a format that\'s used to
    -- encode geographic data structures. For more information, see
    -- <https://geojson.org/ GeoJSON>.
    geoJsonPayload :: Prelude.Maybe Prelude.ByteString,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResourcePositionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'geoJsonPayload', 'getResourcePositionResponse_geoJsonPayload' - The position information of the resource, displayed as a JSON payload.
-- The payload uses the GeoJSON format, which a format that\'s used to
-- encode geographic data structures. For more information, see
-- <https://geojson.org/ GeoJSON>.
--
-- 'httpStatus', 'getResourcePositionResponse_httpStatus' - The response's http status code.
newGetResourcePositionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResourcePositionResponse
newGetResourcePositionResponse pHttpStatus_ =
  GetResourcePositionResponse'
    { geoJsonPayload =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The position information of the resource, displayed as a JSON payload.
-- The payload uses the GeoJSON format, which a format that\'s used to
-- encode geographic data structures. For more information, see
-- <https://geojson.org/ GeoJSON>.
getResourcePositionResponse_geoJsonPayload :: Lens.Lens' GetResourcePositionResponse (Prelude.Maybe Prelude.ByteString)
getResourcePositionResponse_geoJsonPayload = Lens.lens (\GetResourcePositionResponse' {geoJsonPayload} -> geoJsonPayload) (\s@GetResourcePositionResponse' {} a -> s {geoJsonPayload = a} :: GetResourcePositionResponse)

-- | The response's http status code.
getResourcePositionResponse_httpStatus :: Lens.Lens' GetResourcePositionResponse Prelude.Int
getResourcePositionResponse_httpStatus = Lens.lens (\GetResourcePositionResponse' {httpStatus} -> httpStatus) (\s@GetResourcePositionResponse' {} a -> s {httpStatus = a} :: GetResourcePositionResponse)

instance Prelude.NFData GetResourcePositionResponse where
  rnf GetResourcePositionResponse' {..} =
    Prelude.rnf geoJsonPayload
      `Prelude.seq` Prelude.rnf httpStatus
