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
-- Module      : Amazonka.Location.CreateMap
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a map resource in your AWS account, which provides map tiles of
-- different styles sourced from global location data providers.
--
-- If your application is tracking or routing assets you use in your
-- business, such as delivery vehicles or employees, you may only use HERE
-- as your geolocation provider. See section 82 of the
-- <http://aws.amazon.com/service-terms AWS service terms> for more
-- details.
module Amazonka.Location.CreateMap
  ( -- * Creating a Request
    CreateMap (..),
    newCreateMap,

    -- * Request Lenses
    createMap_description,
    createMap_pricingPlan,
    createMap_tags,
    createMap_configuration,
    createMap_mapName,

    -- * Destructuring the Response
    CreateMapResponse (..),
    newCreateMapResponse,

    -- * Response Lenses
    createMapResponse_httpStatus,
    createMapResponse_createTime,
    createMapResponse_mapArn,
    createMapResponse_mapName,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Location.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateMap' smart constructor.
data CreateMap = CreateMap'
  { -- | An optional description for the map resource.
    description :: Prelude.Maybe Prelude.Text,
    -- | No longer used. If included, the only allowed value is
    -- @RequestBasedUsage@.
    pricingPlan :: Prelude.Maybe PricingPlan,
    -- | Applies one or more tags to the map resource. A tag is a key-value pair
    -- helps manage, identify, search, and filter your resources by labelling
    -- them.
    --
    -- Format: @\"key\" : \"value\"@
    --
    -- Restrictions:
    --
    -- -   Maximum 50 tags per resource
    --
    -- -   Each resource tag must be unique with a maximum of one value.
    --
    -- -   Maximum key length: 128 Unicode characters in UTF-8
    --
    -- -   Maximum value length: 256 Unicode characters in UTF-8
    --
    -- -   Can use alphanumeric characters (A–Z, a–z, 0–9), and the following
    --     characters: + - = . _ : \/ \@.
    --
    -- -   Cannot use \"aws:\" as a prefix for a key.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | Specifies the @MapConfiguration@, including the map style, for the map
    -- resource that you create. The map style defines the look of maps and the
    -- data provider for your map resource.
    configuration :: MapConfiguration,
    -- | The name for the map resource.
    --
    -- Requirements:
    --
    -- -   Must contain only alphanumeric characters (A–Z, a–z, 0–9), hyphens
    --     (-), periods (.), and underscores (_).
    --
    -- -   Must be a unique map resource name.
    --
    -- -   No spaces allowed. For example, @ExampleMap@.
    mapName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMap' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createMap_description' - An optional description for the map resource.
--
-- 'pricingPlan', 'createMap_pricingPlan' - No longer used. If included, the only allowed value is
-- @RequestBasedUsage@.
--
-- 'tags', 'createMap_tags' - Applies one or more tags to the map resource. A tag is a key-value pair
-- helps manage, identify, search, and filter your resources by labelling
-- them.
--
-- Format: @\"key\" : \"value\"@
--
-- Restrictions:
--
-- -   Maximum 50 tags per resource
--
-- -   Each resource tag must be unique with a maximum of one value.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8
--
-- -   Maximum value length: 256 Unicode characters in UTF-8
--
-- -   Can use alphanumeric characters (A–Z, a–z, 0–9), and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Cannot use \"aws:\" as a prefix for a key.
--
-- 'configuration', 'createMap_configuration' - Specifies the @MapConfiguration@, including the map style, for the map
-- resource that you create. The map style defines the look of maps and the
-- data provider for your map resource.
--
-- 'mapName', 'createMap_mapName' - The name for the map resource.
--
-- Requirements:
--
-- -   Must contain only alphanumeric characters (A–Z, a–z, 0–9), hyphens
--     (-), periods (.), and underscores (_).
--
-- -   Must be a unique map resource name.
--
-- -   No spaces allowed. For example, @ExampleMap@.
newCreateMap ::
  -- | 'configuration'
  MapConfiguration ->
  -- | 'mapName'
  Prelude.Text ->
  CreateMap
newCreateMap pConfiguration_ pMapName_ =
  CreateMap'
    { description = Prelude.Nothing,
      pricingPlan = Prelude.Nothing,
      tags = Prelude.Nothing,
      configuration = pConfiguration_,
      mapName = pMapName_
    }

-- | An optional description for the map resource.
createMap_description :: Lens.Lens' CreateMap (Prelude.Maybe Prelude.Text)
createMap_description = Lens.lens (\CreateMap' {description} -> description) (\s@CreateMap' {} a -> s {description = a} :: CreateMap)

-- | No longer used. If included, the only allowed value is
-- @RequestBasedUsage@.
createMap_pricingPlan :: Lens.Lens' CreateMap (Prelude.Maybe PricingPlan)
createMap_pricingPlan = Lens.lens (\CreateMap' {pricingPlan} -> pricingPlan) (\s@CreateMap' {} a -> s {pricingPlan = a} :: CreateMap)

-- | Applies one or more tags to the map resource. A tag is a key-value pair
-- helps manage, identify, search, and filter your resources by labelling
-- them.
--
-- Format: @\"key\" : \"value\"@
--
-- Restrictions:
--
-- -   Maximum 50 tags per resource
--
-- -   Each resource tag must be unique with a maximum of one value.
--
-- -   Maximum key length: 128 Unicode characters in UTF-8
--
-- -   Maximum value length: 256 Unicode characters in UTF-8
--
-- -   Can use alphanumeric characters (A–Z, a–z, 0–9), and the following
--     characters: + - = . _ : \/ \@.
--
-- -   Cannot use \"aws:\" as a prefix for a key.
createMap_tags :: Lens.Lens' CreateMap (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createMap_tags = Lens.lens (\CreateMap' {tags} -> tags) (\s@CreateMap' {} a -> s {tags = a} :: CreateMap) Prelude.. Lens.mapping Lens.coerced

-- | Specifies the @MapConfiguration@, including the map style, for the map
-- resource that you create. The map style defines the look of maps and the
-- data provider for your map resource.
createMap_configuration :: Lens.Lens' CreateMap MapConfiguration
createMap_configuration = Lens.lens (\CreateMap' {configuration} -> configuration) (\s@CreateMap' {} a -> s {configuration = a} :: CreateMap)

-- | The name for the map resource.
--
-- Requirements:
--
-- -   Must contain only alphanumeric characters (A–Z, a–z, 0–9), hyphens
--     (-), periods (.), and underscores (_).
--
-- -   Must be a unique map resource name.
--
-- -   No spaces allowed. For example, @ExampleMap@.
createMap_mapName :: Lens.Lens' CreateMap Prelude.Text
createMap_mapName = Lens.lens (\CreateMap' {mapName} -> mapName) (\s@CreateMap' {} a -> s {mapName = a} :: CreateMap)

instance Core.AWSRequest CreateMap where
  type AWSResponse CreateMap = CreateMapResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateMapResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "CreateTime")
            Prelude.<*> (x Data..:> "MapArn")
            Prelude.<*> (x Data..:> "MapName")
      )

instance Prelude.Hashable CreateMap where
  hashWithSalt _salt CreateMap' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` pricingPlan
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` configuration
      `Prelude.hashWithSalt` mapName

instance Prelude.NFData CreateMap where
  rnf CreateMap' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf pricingPlan
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf configuration
      `Prelude.seq` Prelude.rnf mapName

instance Data.ToHeaders CreateMap where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateMap where
  toJSON CreateMap' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("PricingPlan" Data..=) Prelude.<$> pricingPlan,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Configuration" Data..= configuration),
            Prelude.Just ("MapName" Data..= mapName)
          ]
      )

instance Data.ToPath CreateMap where
  toPath = Prelude.const "/maps/v0/maps"

instance Data.ToQuery CreateMap where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateMapResponse' smart constructor.
data CreateMapResponse = CreateMapResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The timestamp for when the map resource was created in
    -- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
    -- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
    createTime :: Data.POSIX,
    -- | The Amazon Resource Name (ARN) for the map resource. Used to specify a
    -- resource across all AWS.
    --
    -- -   Format example: @arn:aws:geo:region:account-id:map\/ExampleMap@
    mapArn :: Prelude.Text,
    -- | The name of the map resource.
    mapName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateMapResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createMapResponse_httpStatus' - The response's http status code.
--
-- 'createTime', 'createMapResponse_createTime' - The timestamp for when the map resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
--
-- 'mapArn', 'createMapResponse_mapArn' - The Amazon Resource Name (ARN) for the map resource. Used to specify a
-- resource across all AWS.
--
-- -   Format example: @arn:aws:geo:region:account-id:map\/ExampleMap@
--
-- 'mapName', 'createMapResponse_mapName' - The name of the map resource.
newCreateMapResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'createTime'
  Prelude.UTCTime ->
  -- | 'mapArn'
  Prelude.Text ->
  -- | 'mapName'
  Prelude.Text ->
  CreateMapResponse
newCreateMapResponse
  pHttpStatus_
  pCreateTime_
  pMapArn_
  pMapName_ =
    CreateMapResponse'
      { httpStatus = pHttpStatus_,
        createTime = Data._Time Lens.# pCreateTime_,
        mapArn = pMapArn_,
        mapName = pMapName_
      }

-- | The response's http status code.
createMapResponse_httpStatus :: Lens.Lens' CreateMapResponse Prelude.Int
createMapResponse_httpStatus = Lens.lens (\CreateMapResponse' {httpStatus} -> httpStatus) (\s@CreateMapResponse' {} a -> s {httpStatus = a} :: CreateMapResponse)

-- | The timestamp for when the map resource was created in
-- <https://www.iso.org/iso-8601-date-and-time-format.html ISO 8601>
-- format: @YYYY-MM-DDThh:mm:ss.sssZ@.
createMapResponse_createTime :: Lens.Lens' CreateMapResponse Prelude.UTCTime
createMapResponse_createTime = Lens.lens (\CreateMapResponse' {createTime} -> createTime) (\s@CreateMapResponse' {} a -> s {createTime = a} :: CreateMapResponse) Prelude.. Data._Time

-- | The Amazon Resource Name (ARN) for the map resource. Used to specify a
-- resource across all AWS.
--
-- -   Format example: @arn:aws:geo:region:account-id:map\/ExampleMap@
createMapResponse_mapArn :: Lens.Lens' CreateMapResponse Prelude.Text
createMapResponse_mapArn = Lens.lens (\CreateMapResponse' {mapArn} -> mapArn) (\s@CreateMapResponse' {} a -> s {mapArn = a} :: CreateMapResponse)

-- | The name of the map resource.
createMapResponse_mapName :: Lens.Lens' CreateMapResponse Prelude.Text
createMapResponse_mapName = Lens.lens (\CreateMapResponse' {mapName} -> mapName) (\s@CreateMapResponse' {} a -> s {mapName = a} :: CreateMapResponse)

instance Prelude.NFData CreateMapResponse where
  rnf CreateMapResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf createTime
      `Prelude.seq` Prelude.rnf mapArn
      `Prelude.seq` Prelude.rnf mapName
