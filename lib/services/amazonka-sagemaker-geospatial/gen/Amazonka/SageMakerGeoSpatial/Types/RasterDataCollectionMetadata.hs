{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.SageMakerGeoSpatial.Types.RasterDataCollectionMetadata
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.RasterDataCollectionMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.DataCollectionType
import Amazonka.SageMakerGeoSpatial.Types.Filter

-- | Response object containing details for a specific RasterDataCollection.
--
-- /See:/ 'newRasterDataCollectionMetadata' smart constructor.
data RasterDataCollectionMetadata = RasterDataCollectionMetadata'
  { -- | The description URL of the raster data collection.
    descriptionPageUrl :: Prelude.Maybe Prelude.Text,
    -- | Each tag consists of a key and a value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the raster data collection.
    arn :: Prelude.Text,
    -- | A description of the raster data collection.
    description :: Prelude.Text,
    -- | The name of the raster data collection.
    name :: Prelude.Text,
    supportedFilters :: [Filter],
    -- | The type of raster data collection.
    type' :: DataCollectionType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RasterDataCollectionMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'descriptionPageUrl', 'rasterDataCollectionMetadata_descriptionPageUrl' - The description URL of the raster data collection.
--
-- 'tags', 'rasterDataCollectionMetadata_tags' - Each tag consists of a key and a value.
--
-- 'arn', 'rasterDataCollectionMetadata_arn' - The Amazon Resource Name (ARN) of the raster data collection.
--
-- 'description', 'rasterDataCollectionMetadata_description' - A description of the raster data collection.
--
-- 'name', 'rasterDataCollectionMetadata_name' - The name of the raster data collection.
--
-- 'supportedFilters', 'rasterDataCollectionMetadata_supportedFilters' -
--
-- 'type'', 'rasterDataCollectionMetadata_type' - The type of raster data collection.
newRasterDataCollectionMetadata ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  DataCollectionType ->
  RasterDataCollectionMetadata
newRasterDataCollectionMetadata
  pArn_
  pDescription_
  pName_
  pType_ =
    RasterDataCollectionMetadata'
      { descriptionPageUrl =
          Prelude.Nothing,
        tags = Prelude.Nothing,
        arn = pArn_,
        description = pDescription_,
        name = pName_,
        supportedFilters = Prelude.mempty,
        type' = pType_
      }

-- | The description URL of the raster data collection.
rasterDataCollectionMetadata_descriptionPageUrl :: Lens.Lens' RasterDataCollectionMetadata (Prelude.Maybe Prelude.Text)
rasterDataCollectionMetadata_descriptionPageUrl = Lens.lens (\RasterDataCollectionMetadata' {descriptionPageUrl} -> descriptionPageUrl) (\s@RasterDataCollectionMetadata' {} a -> s {descriptionPageUrl = a} :: RasterDataCollectionMetadata)

-- | Each tag consists of a key and a value.
rasterDataCollectionMetadata_tags :: Lens.Lens' RasterDataCollectionMetadata (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
rasterDataCollectionMetadata_tags = Lens.lens (\RasterDataCollectionMetadata' {tags} -> tags) (\s@RasterDataCollectionMetadata' {} a -> s {tags = a} :: RasterDataCollectionMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the raster data collection.
rasterDataCollectionMetadata_arn :: Lens.Lens' RasterDataCollectionMetadata Prelude.Text
rasterDataCollectionMetadata_arn = Lens.lens (\RasterDataCollectionMetadata' {arn} -> arn) (\s@RasterDataCollectionMetadata' {} a -> s {arn = a} :: RasterDataCollectionMetadata)

-- | A description of the raster data collection.
rasterDataCollectionMetadata_description :: Lens.Lens' RasterDataCollectionMetadata Prelude.Text
rasterDataCollectionMetadata_description = Lens.lens (\RasterDataCollectionMetadata' {description} -> description) (\s@RasterDataCollectionMetadata' {} a -> s {description = a} :: RasterDataCollectionMetadata)

-- | The name of the raster data collection.
rasterDataCollectionMetadata_name :: Lens.Lens' RasterDataCollectionMetadata Prelude.Text
rasterDataCollectionMetadata_name = Lens.lens (\RasterDataCollectionMetadata' {name} -> name) (\s@RasterDataCollectionMetadata' {} a -> s {name = a} :: RasterDataCollectionMetadata)

rasterDataCollectionMetadata_supportedFilters :: Lens.Lens' RasterDataCollectionMetadata [Filter]
rasterDataCollectionMetadata_supportedFilters = Lens.lens (\RasterDataCollectionMetadata' {supportedFilters} -> supportedFilters) (\s@RasterDataCollectionMetadata' {} a -> s {supportedFilters = a} :: RasterDataCollectionMetadata) Prelude.. Lens.coerced

-- | The type of raster data collection.
rasterDataCollectionMetadata_type :: Lens.Lens' RasterDataCollectionMetadata DataCollectionType
rasterDataCollectionMetadata_type = Lens.lens (\RasterDataCollectionMetadata' {type'} -> type') (\s@RasterDataCollectionMetadata' {} a -> s {type' = a} :: RasterDataCollectionMetadata)

instance Data.FromJSON RasterDataCollectionMetadata where
  parseJSON =
    Data.withObject
      "RasterDataCollectionMetadata"
      ( \x ->
          RasterDataCollectionMetadata'
            Prelude.<$> (x Data..:? "DescriptionPageUrl")
            Prelude.<*> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "Description")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> ( x
                            Data..:? "SupportedFilters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..: "Type")
      )

instance
  Prelude.Hashable
    RasterDataCollectionMetadata
  where
  hashWithSalt _salt RasterDataCollectionMetadata' {..} =
    _salt
      `Prelude.hashWithSalt` descriptionPageUrl
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` supportedFilters
      `Prelude.hashWithSalt` type'

instance Prelude.NFData RasterDataCollectionMetadata where
  rnf RasterDataCollectionMetadata' {..} =
    Prelude.rnf descriptionPageUrl
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf supportedFilters
      `Prelude.seq` Prelude.rnf type'
