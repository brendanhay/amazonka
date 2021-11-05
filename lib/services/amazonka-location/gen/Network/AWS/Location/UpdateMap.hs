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
-- Module      : Network.AWS.Location.UpdateMap
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified properties of a given map resource.
module Network.AWS.Location.UpdateMap
  ( -- * Creating a Request
    UpdateMap (..),
    newUpdateMap,

    -- * Request Lenses
    updateMap_pricingPlan,
    updateMap_description,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Location.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateMap' smart constructor.
data UpdateMap = UpdateMap'
  { -- | Updates the pricing plan for the map resource.
    --
    -- For more information about each pricing plan option restrictions, see
    -- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing>.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | Updates the description for the map resource.
    description :: Prelude.Maybe Prelude.Text,
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
-- 'pricingPlan', 'updateMap_pricingPlan' - Updates the pricing plan for the map resource.
--
-- For more information about each pricing plan option restrictions, see
-- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing>.
--
-- 'description', 'updateMap_description' - Updates the description for the map resource.
--
-- 'mapName', 'updateMap_mapName' - The name of the map resource to update.
newUpdateMap ::
  -- | 'mapName'
  Prelude.Text ->
  UpdateMap
newUpdateMap pMapName_ =
  UpdateMap'
    { pricingPlan = Prelude.Nothing,
      description = Prelude.Nothing,
      mapName = pMapName_
    }

-- | Updates the pricing plan for the map resource.
--
-- For more information about each pricing plan option restrictions, see
-- <https://aws.amazon.com/location/pricing/ Amazon Location Service pricing>.
updateMap_pricingPlan :: Lens.Lens' UpdateMap (Prelude.Maybe PricingPlan)
updateMap_pricingPlan = Lens.lens (\UpdateMap' {pricingPlan} -> pricingPlan) (\s@UpdateMap' {} a -> s {pricingPlan = a} :: UpdateMap)

-- | Updates the description for the map resource.
updateMap_description :: Lens.Lens' UpdateMap (Prelude.Maybe Prelude.Text)
updateMap_description = Lens.lens (\UpdateMap' {description} -> description) (\s@UpdateMap' {} a -> s {description = a} :: UpdateMap)

-- | The name of the map resource to update.
updateMap_mapName :: Lens.Lens' UpdateMap Prelude.Text
updateMap_mapName = Lens.lens (\UpdateMap' {mapName} -> mapName) (\s@UpdateMap' {} a -> s {mapName = a} :: UpdateMap)

instance Core.AWSRequest UpdateMap where
  type AWSResponse UpdateMap = UpdateMapResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateMapResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..:> "MapArn")
            Prelude.<*> (x Core..:> "MapName")
            Prelude.<*> (x Core..:> "UpdateTime")
      )

instance Prelude.Hashable UpdateMap

instance Prelude.NFData UpdateMap

instance Core.ToHeaders UpdateMap where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateMap where
  toJSON UpdateMap' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("PricingPlan" Core..=) Prelude.<$> pricingPlan,
            ("Description" Core..=) Prelude.<$> description
          ]
      )

instance Core.ToPath UpdateMap where
  toPath UpdateMap' {..} =
    Prelude.mconcat
      ["/maps/v0/maps/", Core.toBS mapName]

instance Core.ToQuery UpdateMap where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateMapResponse' smart constructor.
data UpdateMapResponse = UpdateMapResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the updated map resource. Used to
    -- specify a resource across AWS.
    --
    -- -   Format example: @arn:aws:geo:region:account-id:maps\/ExampleMap@
    mapArn :: Prelude.Text,
    -- | The name of the updated map resource.
    mapName :: Prelude.Text,
    -- | The timestamp for when the map resource was last updated in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    updateTime :: Core.POSIX
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
-- -   Format example: @arn:aws:geo:region:account-id:maps\/ExampleMap@
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
        updateTime = Core._Time Lens.# pUpdateTime_
      }

-- | The response's http status code.
updateMapResponse_httpStatus :: Lens.Lens' UpdateMapResponse Prelude.Int
updateMapResponse_httpStatus = Lens.lens (\UpdateMapResponse' {httpStatus} -> httpStatus) (\s@UpdateMapResponse' {} a -> s {httpStatus = a} :: UpdateMapResponse)

-- | The Amazon Resource Name (ARN) of the updated map resource. Used to
-- specify a resource across AWS.
--
-- -   Format example: @arn:aws:geo:region:account-id:maps\/ExampleMap@
updateMapResponse_mapArn :: Lens.Lens' UpdateMapResponse Prelude.Text
updateMapResponse_mapArn = Lens.lens (\UpdateMapResponse' {mapArn} -> mapArn) (\s@UpdateMapResponse' {} a -> s {mapArn = a} :: UpdateMapResponse)

-- | The name of the updated map resource.
updateMapResponse_mapName :: Lens.Lens' UpdateMapResponse Prelude.Text
updateMapResponse_mapName = Lens.lens (\UpdateMapResponse' {mapName} -> mapName) (\s@UpdateMapResponse' {} a -> s {mapName = a} :: UpdateMapResponse)

-- | The timestamp for when the map resource was last updated in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
updateMapResponse_updateTime :: Lens.Lens' UpdateMapResponse Prelude.UTCTime
updateMapResponse_updateTime = Lens.lens (\UpdateMapResponse' {updateTime} -> updateTime) (\s@UpdateMapResponse' {} a -> s {updateTime = a} :: UpdateMapResponse) Prelude.. Core._Time

instance Prelude.NFData UpdateMapResponse
