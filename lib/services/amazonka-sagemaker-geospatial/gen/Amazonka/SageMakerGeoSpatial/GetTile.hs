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
-- Module      : Amazonka.SageMakerGeoSpatial.GetTile
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a web mercator tile for the given Earth Observation job.
module Amazonka.SageMakerGeoSpatial.GetTile
  ( -- * Creating a Request
    GetTile (..),
    newGetTile,

    -- * Request Lenses
    getTile_imageMask,
    getTile_outputDataType,
    getTile_outputFormat,
    getTile_propertyFilters,
    getTile_timeRangeFilter,
    getTile_arn,
    getTile_imageAssets,
    getTile_target,
    getTile_x,
    getTile_y,
    getTile_z,

    -- * Destructuring the Response
    GetTileResponse (..),
    newGetTileResponse,

    -- * Response Lenses
    getTileResponse_httpStatus,
    getTileResponse_binaryFile,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerGeoSpatial.Types

-- | /See:/ 'newGetTile' smart constructor.
data GetTile = GetTile'
  { -- | Determines whether or not to return a valid data mask.
    imageMask :: Prelude.Maybe Prelude.Bool,
    -- | The output data type of the tile operation.
    outputDataType :: Prelude.Maybe OutputType,
    -- | The data format of the output tile. The formats include .npy, .png and
    -- .jpg.
    outputFormat :: Prelude.Maybe Prelude.Text,
    -- | Property filters for the imagery to tile.
    propertyFilters :: Prelude.Maybe Prelude.Text,
    -- | Time range filter applied to imagery to find the images to tile.
    timeRangeFilter :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the tile operation.
    arn :: Prelude.Text,
    -- | The particular assets or bands to tile.
    imageAssets :: Prelude.NonEmpty Prelude.Text,
    -- | Determines what part of the Earth Observation job to tile. \'INPUT\' or
    -- \'OUTPUT\' are the valid options.
    target :: TargetOptions,
    -- | The x coordinate of the tile input.
    x :: Prelude.Int,
    -- | The y coordinate of the tile input.
    y :: Prelude.Int,
    -- | The z coordinate of the tile input.
    z :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'imageMask', 'getTile_imageMask' - Determines whether or not to return a valid data mask.
--
-- 'outputDataType', 'getTile_outputDataType' - The output data type of the tile operation.
--
-- 'outputFormat', 'getTile_outputFormat' - The data format of the output tile. The formats include .npy, .png and
-- .jpg.
--
-- 'propertyFilters', 'getTile_propertyFilters' - Property filters for the imagery to tile.
--
-- 'timeRangeFilter', 'getTile_timeRangeFilter' - Time range filter applied to imagery to find the images to tile.
--
-- 'arn', 'getTile_arn' - The Amazon Resource Name (ARN) of the tile operation.
--
-- 'imageAssets', 'getTile_imageAssets' - The particular assets or bands to tile.
--
-- 'target', 'getTile_target' - Determines what part of the Earth Observation job to tile. \'INPUT\' or
-- \'OUTPUT\' are the valid options.
--
-- 'x', 'getTile_x' - The x coordinate of the tile input.
--
-- 'y', 'getTile_y' - The y coordinate of the tile input.
--
-- 'z', 'getTile_z' - The z coordinate of the tile input.
newGetTile ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'imageAssets'
  Prelude.NonEmpty Prelude.Text ->
  -- | 'target'
  TargetOptions ->
  -- | 'x'
  Prelude.Int ->
  -- | 'y'
  Prelude.Int ->
  -- | 'z'
  Prelude.Int ->
  GetTile
newGetTile pArn_ pImageAssets_ pTarget_ pX_ pY_ pZ_ =
  GetTile'
    { imageMask = Prelude.Nothing,
      outputDataType = Prelude.Nothing,
      outputFormat = Prelude.Nothing,
      propertyFilters = Prelude.Nothing,
      timeRangeFilter = Prelude.Nothing,
      arn = pArn_,
      imageAssets = Lens.coerced Lens.# pImageAssets_,
      target = pTarget_,
      x = pX_,
      y = pY_,
      z = pZ_
    }

-- | Determines whether or not to return a valid data mask.
getTile_imageMask :: Lens.Lens' GetTile (Prelude.Maybe Prelude.Bool)
getTile_imageMask = Lens.lens (\GetTile' {imageMask} -> imageMask) (\s@GetTile' {} a -> s {imageMask = a} :: GetTile)

-- | The output data type of the tile operation.
getTile_outputDataType :: Lens.Lens' GetTile (Prelude.Maybe OutputType)
getTile_outputDataType = Lens.lens (\GetTile' {outputDataType} -> outputDataType) (\s@GetTile' {} a -> s {outputDataType = a} :: GetTile)

-- | The data format of the output tile. The formats include .npy, .png and
-- .jpg.
getTile_outputFormat :: Lens.Lens' GetTile (Prelude.Maybe Prelude.Text)
getTile_outputFormat = Lens.lens (\GetTile' {outputFormat} -> outputFormat) (\s@GetTile' {} a -> s {outputFormat = a} :: GetTile)

-- | Property filters for the imagery to tile.
getTile_propertyFilters :: Lens.Lens' GetTile (Prelude.Maybe Prelude.Text)
getTile_propertyFilters = Lens.lens (\GetTile' {propertyFilters} -> propertyFilters) (\s@GetTile' {} a -> s {propertyFilters = a} :: GetTile)

-- | Time range filter applied to imagery to find the images to tile.
getTile_timeRangeFilter :: Lens.Lens' GetTile (Prelude.Maybe Prelude.Text)
getTile_timeRangeFilter = Lens.lens (\GetTile' {timeRangeFilter} -> timeRangeFilter) (\s@GetTile' {} a -> s {timeRangeFilter = a} :: GetTile)

-- | The Amazon Resource Name (ARN) of the tile operation.
getTile_arn :: Lens.Lens' GetTile Prelude.Text
getTile_arn = Lens.lens (\GetTile' {arn} -> arn) (\s@GetTile' {} a -> s {arn = a} :: GetTile)

-- | The particular assets or bands to tile.
getTile_imageAssets :: Lens.Lens' GetTile (Prelude.NonEmpty Prelude.Text)
getTile_imageAssets = Lens.lens (\GetTile' {imageAssets} -> imageAssets) (\s@GetTile' {} a -> s {imageAssets = a} :: GetTile) Prelude.. Lens.coerced

-- | Determines what part of the Earth Observation job to tile. \'INPUT\' or
-- \'OUTPUT\' are the valid options.
getTile_target :: Lens.Lens' GetTile TargetOptions
getTile_target = Lens.lens (\GetTile' {target} -> target) (\s@GetTile' {} a -> s {target = a} :: GetTile)

-- | The x coordinate of the tile input.
getTile_x :: Lens.Lens' GetTile Prelude.Int
getTile_x = Lens.lens (\GetTile' {x} -> x) (\s@GetTile' {} a -> s {x = a} :: GetTile)

-- | The y coordinate of the tile input.
getTile_y :: Lens.Lens' GetTile Prelude.Int
getTile_y = Lens.lens (\GetTile' {y} -> y) (\s@GetTile' {} a -> s {y = a} :: GetTile)

-- | The z coordinate of the tile input.
getTile_z :: Lens.Lens' GetTile Prelude.Int
getTile_z = Lens.lens (\GetTile' {z} -> z) (\s@GetTile' {} a -> s {z = a} :: GetTile)

instance Core.AWSRequest GetTile where
  type AWSResponse GetTile = GetTileResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveBody
      ( \s h x ->
          GetTileResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (Prelude.pure x)
      )

instance Prelude.Hashable GetTile where
  hashWithSalt _salt GetTile' {..} =
    _salt
      `Prelude.hashWithSalt` imageMask
      `Prelude.hashWithSalt` outputDataType
      `Prelude.hashWithSalt` outputFormat
      `Prelude.hashWithSalt` propertyFilters
      `Prelude.hashWithSalt` timeRangeFilter
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` imageAssets
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` x
      `Prelude.hashWithSalt` y
      `Prelude.hashWithSalt` z

instance Prelude.NFData GetTile where
  rnf GetTile' {..} =
    Prelude.rnf imageMask
      `Prelude.seq` Prelude.rnf outputDataType
      `Prelude.seq` Prelude.rnf outputFormat
      `Prelude.seq` Prelude.rnf propertyFilters
      `Prelude.seq` Prelude.rnf timeRangeFilter
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf imageAssets
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf x
      `Prelude.seq` Prelude.rnf y
      `Prelude.seq` Prelude.rnf z

instance Data.ToHeaders GetTile where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetTile where
  toPath GetTile' {..} =
    Prelude.mconcat
      [ "/tile/",
        Data.toBS z,
        "/",
        Data.toBS x,
        "/",
        Data.toBS y
      ]

instance Data.ToQuery GetTile where
  toQuery GetTile' {..} =
    Prelude.mconcat
      [ "ImageMask" Data.=: imageMask,
        "OutputDataType" Data.=: outputDataType,
        "OutputFormat" Data.=: outputFormat,
        "PropertyFilters" Data.=: propertyFilters,
        "TimeRangeFilter" Data.=: timeRangeFilter,
        "Arn" Data.=: arn,
        "ImageAssets"
          Data.=: Data.toQueryList "member" imageAssets,
        "Target" Data.=: target
      ]

-- | /See:/ 'newGetTileResponse' smart constructor.
data GetTileResponse = GetTileResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The output binary file.
    binaryFile :: Data.ResponseBody
  }
  deriving (Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTileResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getTileResponse_httpStatus' - The response's http status code.
--
-- 'binaryFile', 'getTileResponse_binaryFile' - The output binary file.
newGetTileResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'binaryFile'
  Data.ResponseBody ->
  GetTileResponse
newGetTileResponse pHttpStatus_ pBinaryFile_ =
  GetTileResponse'
    { httpStatus = pHttpStatus_,
      binaryFile = pBinaryFile_
    }

-- | The response's http status code.
getTileResponse_httpStatus :: Lens.Lens' GetTileResponse Prelude.Int
getTileResponse_httpStatus = Lens.lens (\GetTileResponse' {httpStatus} -> httpStatus) (\s@GetTileResponse' {} a -> s {httpStatus = a} :: GetTileResponse)

-- | The output binary file.
getTileResponse_binaryFile :: Lens.Lens' GetTileResponse Data.ResponseBody
getTileResponse_binaryFile = Lens.lens (\GetTileResponse' {binaryFile} -> binaryFile) (\s@GetTileResponse' {} a -> s {binaryFile = a} :: GetTileResponse)
