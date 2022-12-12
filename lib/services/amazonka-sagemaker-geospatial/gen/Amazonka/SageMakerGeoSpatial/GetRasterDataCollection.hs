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
-- Module      : Amazonka.SageMakerGeoSpatial.GetRasterDataCollection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Use this operation to get details of a specific raster data collection.
module Amazonka.SageMakerGeoSpatial.GetRasterDataCollection
  ( -- * Creating a Request
    GetRasterDataCollection (..),
    newGetRasterDataCollection,

    -- * Request Lenses
    getRasterDataCollection_arn,

    -- * Destructuring the Response
    GetRasterDataCollectionResponse (..),
    newGetRasterDataCollectionResponse,

    -- * Response Lenses
    getRasterDataCollectionResponse_tags,
    getRasterDataCollectionResponse_httpStatus,
    getRasterDataCollectionResponse_arn,
    getRasterDataCollectionResponse_description,
    getRasterDataCollectionResponse_descriptionPageUrl,
    getRasterDataCollectionResponse_imageSourceBands,
    getRasterDataCollectionResponse_name,
    getRasterDataCollectionResponse_supportedFilters,
    getRasterDataCollectionResponse_type,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMakerGeoSpatial.Types

-- | /See:/ 'newGetRasterDataCollection' smart constructor.
data GetRasterDataCollection = GetRasterDataCollection'
  { -- | The Amazon Resource Name (ARN) of the raster data collection.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRasterDataCollection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getRasterDataCollection_arn' - The Amazon Resource Name (ARN) of the raster data collection.
newGetRasterDataCollection ::
  -- | 'arn'
  Prelude.Text ->
  GetRasterDataCollection
newGetRasterDataCollection pArn_ =
  GetRasterDataCollection' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the raster data collection.
getRasterDataCollection_arn :: Lens.Lens' GetRasterDataCollection Prelude.Text
getRasterDataCollection_arn = Lens.lens (\GetRasterDataCollection' {arn} -> arn) (\s@GetRasterDataCollection' {} a -> s {arn = a} :: GetRasterDataCollection)

instance Core.AWSRequest GetRasterDataCollection where
  type
    AWSResponse GetRasterDataCollection =
      GetRasterDataCollectionResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRasterDataCollectionResponse'
            Prelude.<$> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Arn")
            Prelude.<*> (x Data..:> "Description")
            Prelude.<*> (x Data..:> "DescriptionPageUrl")
            Prelude.<*> ( x Data..?> "ImageSourceBands"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..:> "Name")
            Prelude.<*> ( x Data..?> "SupportedFilters"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..:> "Type")
      )

instance Prelude.Hashable GetRasterDataCollection where
  hashWithSalt _salt GetRasterDataCollection' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData GetRasterDataCollection where
  rnf GetRasterDataCollection' {..} = Prelude.rnf arn

instance Data.ToHeaders GetRasterDataCollection where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetRasterDataCollection where
  toPath GetRasterDataCollection' {..} =
    Prelude.mconcat
      ["/raster-data-collection/", Data.toBS arn]

instance Data.ToQuery GetRasterDataCollection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRasterDataCollectionResponse' smart constructor.
data GetRasterDataCollectionResponse = GetRasterDataCollectionResponse'
  { -- | Each tag consists of a key and a value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The Amazon Resource Name (ARN) of the raster data collection.
    arn :: Prelude.Text,
    -- | A description of the raster data collection.
    description :: Prelude.Text,
    -- | The URL of the description page.
    descriptionPageUrl :: Prelude.Text,
    imageSourceBands :: [Prelude.Text],
    -- | The name of the raster data collection.
    name :: Prelude.Text,
    -- | The filters supported by the raster data collection.
    supportedFilters :: [Filter],
    -- | The raster data collection type.
    type' :: DataCollectionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRasterDataCollectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'getRasterDataCollectionResponse_tags' - Each tag consists of a key and a value.
--
-- 'httpStatus', 'getRasterDataCollectionResponse_httpStatus' - The response's http status code.
--
-- 'arn', 'getRasterDataCollectionResponse_arn' - The Amazon Resource Name (ARN) of the raster data collection.
--
-- 'description', 'getRasterDataCollectionResponse_description' - A description of the raster data collection.
--
-- 'descriptionPageUrl', 'getRasterDataCollectionResponse_descriptionPageUrl' - The URL of the description page.
--
-- 'imageSourceBands', 'getRasterDataCollectionResponse_imageSourceBands' -
--
-- 'name', 'getRasterDataCollectionResponse_name' - The name of the raster data collection.
--
-- 'supportedFilters', 'getRasterDataCollectionResponse_supportedFilters' - The filters supported by the raster data collection.
--
-- 'type'', 'getRasterDataCollectionResponse_type' - The raster data collection type.
newGetRasterDataCollectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'descriptionPageUrl'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  DataCollectionType ->
  GetRasterDataCollectionResponse
newGetRasterDataCollectionResponse
  pHttpStatus_
  pArn_
  pDescription_
  pDescriptionPageUrl_
  pName_
  pType_ =
    GetRasterDataCollectionResponse'
      { tags =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        arn = pArn_,
        description = pDescription_,
        descriptionPageUrl = pDescriptionPageUrl_,
        imageSourceBands = Prelude.mempty,
        name = pName_,
        supportedFilters = Prelude.mempty,
        type' = pType_
      }

-- | Each tag consists of a key and a value.
getRasterDataCollectionResponse_tags :: Lens.Lens' GetRasterDataCollectionResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
getRasterDataCollectionResponse_tags = Lens.lens (\GetRasterDataCollectionResponse' {tags} -> tags) (\s@GetRasterDataCollectionResponse' {} a -> s {tags = a} :: GetRasterDataCollectionResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getRasterDataCollectionResponse_httpStatus :: Lens.Lens' GetRasterDataCollectionResponse Prelude.Int
getRasterDataCollectionResponse_httpStatus = Lens.lens (\GetRasterDataCollectionResponse' {httpStatus} -> httpStatus) (\s@GetRasterDataCollectionResponse' {} a -> s {httpStatus = a} :: GetRasterDataCollectionResponse)

-- | The Amazon Resource Name (ARN) of the raster data collection.
getRasterDataCollectionResponse_arn :: Lens.Lens' GetRasterDataCollectionResponse Prelude.Text
getRasterDataCollectionResponse_arn = Lens.lens (\GetRasterDataCollectionResponse' {arn} -> arn) (\s@GetRasterDataCollectionResponse' {} a -> s {arn = a} :: GetRasterDataCollectionResponse)

-- | A description of the raster data collection.
getRasterDataCollectionResponse_description :: Lens.Lens' GetRasterDataCollectionResponse Prelude.Text
getRasterDataCollectionResponse_description = Lens.lens (\GetRasterDataCollectionResponse' {description} -> description) (\s@GetRasterDataCollectionResponse' {} a -> s {description = a} :: GetRasterDataCollectionResponse)

-- | The URL of the description page.
getRasterDataCollectionResponse_descriptionPageUrl :: Lens.Lens' GetRasterDataCollectionResponse Prelude.Text
getRasterDataCollectionResponse_descriptionPageUrl = Lens.lens (\GetRasterDataCollectionResponse' {descriptionPageUrl} -> descriptionPageUrl) (\s@GetRasterDataCollectionResponse' {} a -> s {descriptionPageUrl = a} :: GetRasterDataCollectionResponse)

-- |
getRasterDataCollectionResponse_imageSourceBands :: Lens.Lens' GetRasterDataCollectionResponse [Prelude.Text]
getRasterDataCollectionResponse_imageSourceBands = Lens.lens (\GetRasterDataCollectionResponse' {imageSourceBands} -> imageSourceBands) (\s@GetRasterDataCollectionResponse' {} a -> s {imageSourceBands = a} :: GetRasterDataCollectionResponse) Prelude.. Lens.coerced

-- | The name of the raster data collection.
getRasterDataCollectionResponse_name :: Lens.Lens' GetRasterDataCollectionResponse Prelude.Text
getRasterDataCollectionResponse_name = Lens.lens (\GetRasterDataCollectionResponse' {name} -> name) (\s@GetRasterDataCollectionResponse' {} a -> s {name = a} :: GetRasterDataCollectionResponse)

-- | The filters supported by the raster data collection.
getRasterDataCollectionResponse_supportedFilters :: Lens.Lens' GetRasterDataCollectionResponse [Filter]
getRasterDataCollectionResponse_supportedFilters = Lens.lens (\GetRasterDataCollectionResponse' {supportedFilters} -> supportedFilters) (\s@GetRasterDataCollectionResponse' {} a -> s {supportedFilters = a} :: GetRasterDataCollectionResponse) Prelude.. Lens.coerced

-- | The raster data collection type.
getRasterDataCollectionResponse_type :: Lens.Lens' GetRasterDataCollectionResponse DataCollectionType
getRasterDataCollectionResponse_type = Lens.lens (\GetRasterDataCollectionResponse' {type'} -> type') (\s@GetRasterDataCollectionResponse' {} a -> s {type' = a} :: GetRasterDataCollectionResponse)

instance
  Prelude.NFData
    GetRasterDataCollectionResponse
  where
  rnf GetRasterDataCollectionResponse' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf descriptionPageUrl
      `Prelude.seq` Prelude.rnf imageSourceBands
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf supportedFilters
      `Prelude.seq` Prelude.rnf type'
