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
-- Module      : Amazonka.Location.UpdateMap
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified properties of a given map resource.
module Amazonka.Location.UpdateMap
  ( -- * Creating a Request
    UpdateMap (..),
    newUpdateMap,

    -- * Request Lenses
    updateMap_description,
    updateMap_pricingPlan,
    updateMap_mapName,

    -- * Destructuring the Response
    UpdateMapResponse (..),
    newUpdateMapResponse,

    -- * Response Lenses
    updateMapResponse_httpStatus,
    updateMapResponse_mapArn,
    updateMapResponse_mapName,
    updateMapResponse_updateTime,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateMap' smart constructor.
data UpdateMap = UpdateMap'
  { -- | Updates the description for the map resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | No longer used. If included, the only allowed value is
    -- @RequestBasedUsage@.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | The name of the map resource to update.
    mapName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMap' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateMap_description' - Updates the description for the map resource.
--
-- 'pricingPlan', 'updateMap_pricingPlan' - No longer used. If included, the only allowed value is
-- @RequestBasedUsage@.
--
-- 'mapName', 'updateMap_mapName' - The name of the map resource to update.
newUpdateMap ::
  -- | 'mapName'
  Prelude.Text ->
  UpdateMap
newUpdateMap pMapName_ =
  UpdateMap'
    { description = Prelude.Nothing,
      pricingPlan = Prelude.Nothing,
      mapName = pMapName_
    }

-- | Updates the description for the map resource.
updateMap_description :: Lens.Lens' UpdateMap (Prelude.Maybe Prelude.Text)
updateMap_description = Lens.lens (\UpdateMap' {description} -> description) (\s@UpdateMap' {} a -> s {description = a} :: UpdateMap)

-- | No longer used. If included, the only allowed value is
-- @RequestBasedUsage@.
updateMap_pricingPlan :: Lens.Lens' UpdateMap (Prelude.Maybe PricingPlan)
updateMap_pricingPlan = Lens.lens (\UpdateMap' {pricingPlan} -> pricingPlan) (\s@UpdateMap' {} a -> s {pricingPlan = a} :: UpdateMap)

-- | The name of the map resource to update.
updateMap_mapName :: Lens.Lens' UpdateMap Prelude.Text
updateMap_mapName = Lens.lens (\UpdateMap' {mapName} -> mapName) (\s@UpdateMap' {} a -> s {mapName = a} :: UpdateMap)

instance Core.AWSRequest UpdateMap where
  type AWSResponse UpdateMap = UpdateMapResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMapResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "MapArn")
            Prelude.<*> (x Data..:> "MapName")
            Prelude.<*> (x Data..:> "UpdateTime")
      )

instance Prelude.Hashable UpdateMap where
  hashWithSalt _salt UpdateMap' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` pricingPlan
      `Prelude.hashWithSalt` mapName

instance Prelude.NFData UpdateMap where
  rnf UpdateMap' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf pricingPlan
      `Prelude.seq` Prelude.rnf mapName

instance Data.ToHeaders UpdateMap where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateMap where
  toJSON UpdateMap' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("PricingPlan" Data..=) Prelude.<$> pricingPlan
          ]
      )

instance Data.ToPath UpdateMap where
  toPath UpdateMap' {..} =
    Prelude.mconcat
      ["/maps/v0/maps/", Data.toBS mapName]

instance Data.ToQuery UpdateMap where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMapResponse' smart constructor.
data UpdateMapResponse = UpdateMapResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the updated map resource. Used to
    -- specify a resource across AWS.
    --
    -- -   Format example: @arn:aws:geo:region:account-id:map\/ExampleMap@
    mapArn :: Prelude.Text,
    -- | The name of the updated map resource.
    mapName :: Prelude.Text,
    -- | The timestamp for when the map resource was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateMapResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateMapResponse_httpStatus' - The response's http status code.
--
-- 'mapArn', 'updateMapResponse_mapArn' - The Amazon Resource Name (ARN) of the updated map resource. Used to
-- specify a resource across AWS.
--
-- -   Format example: @arn:aws:geo:region:account-id:map\/ExampleMap@
--
-- 'mapName', 'updateMapResponse_mapName' - The name of the updated map resource.
--
-- 'updateTime', 'updateMapResponse_updateTime' - The timestamp for when the map resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
newUpdateMapResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'mapArn'
  Prelude.Text ->
  -- | 'mapName'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  UpdateMapResponse
newUpdateMapResponse
  pHttpStatus_
  pMapArn_
  pMapName_
  pUpdateTime_ =
    UpdateMapResponse'
      { httpStatus = pHttpStatus_,
        mapArn = pMapArn_,
        mapName = pMapName_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | The response's http status code.
updateMapResponse_httpStatus :: Lens.Lens' UpdateMapResponse Prelude.Int
updateMapResponse_httpStatus = Lens.lens (\UpdateMapResponse' {httpStatus} -> httpStatus) (\s@UpdateMapResponse' {} a -> s {httpStatus = a} :: UpdateMapResponse)

-- | The Amazon Resource Name (ARN) of the updated map resource. Used to
-- specify a resource across AWS.
--
-- -   Format example: @arn:aws:geo:region:account-id:map\/ExampleMap@
updateMapResponse_mapArn :: Lens.Lens' UpdateMapResponse Prelude.Text
updateMapResponse_mapArn = Lens.lens (\UpdateMapResponse' {mapArn} -> mapArn) (\s@UpdateMapResponse' {} a -> s {mapArn = a} :: UpdateMapResponse)

-- | The name of the updated map resource.
updateMapResponse_mapName :: Lens.Lens' UpdateMapResponse Prelude.Text
updateMapResponse_mapName = Lens.lens (\UpdateMapResponse' {mapName} -> mapName) (\s@UpdateMapResponse' {} a -> s {mapName = a} :: UpdateMapResponse)

-- | The timestamp for when the map resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
updateMapResponse_updateTime :: Lens.Lens' UpdateMapResponse Prelude.UTCTime
updateMapResponse_updateTime = Lens.lens (\UpdateMapResponse' {updateTime} -> updateTime) (\s@UpdateMapResponse' {} a -> s {updateTime = a} :: UpdateMapResponse) Prelude.. Data._Time

instance Prelude.NFData UpdateMapResponse where
  rnf UpdateMapResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf mapArn
      `Prelude.seq` Prelude.rnf mapName
      `Prelude.seq` Prelude.rnf updateTime
