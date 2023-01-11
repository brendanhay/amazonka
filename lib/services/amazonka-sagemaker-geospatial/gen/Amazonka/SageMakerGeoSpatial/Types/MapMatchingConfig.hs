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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.MapMatchingConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.MapMatchingConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- |
--
-- /See:/ 'newMapMatchingConfig' smart constructor.
data MapMatchingConfig = MapMatchingConfig'
  { idAttributeName :: Prelude.Text,
    -- | The name of the timestamp attribute.
    timestampAttributeName :: Prelude.Text,
    -- | The name of the X-attribute
    xAttributeName :: Prelude.Text,
    -- | The name of the Y-attribute
    yAttributeName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'MapMatchingConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'idAttributeName', 'mapMatchingConfig_idAttributeName' -
--
-- 'timestampAttributeName', 'mapMatchingConfig_timestampAttributeName' - The name of the timestamp attribute.
--
-- 'xAttributeName', 'mapMatchingConfig_xAttributeName' - The name of the X-attribute
--
-- 'yAttributeName', 'mapMatchingConfig_yAttributeName' - The name of the Y-attribute
newMapMatchingConfig ::
  -- | 'idAttributeName'
  Prelude.Text ->
  -- | 'timestampAttributeName'
  Prelude.Text ->
  -- | 'xAttributeName'
  Prelude.Text ->
  -- | 'yAttributeName'
  Prelude.Text ->
  MapMatchingConfig
newMapMatchingConfig
  pIdAttributeName_
  pTimestampAttributeName_
  pXAttributeName_
  pYAttributeName_ =
    MapMatchingConfig'
      { idAttributeName =
          pIdAttributeName_,
        timestampAttributeName = pTimestampAttributeName_,
        xAttributeName = pXAttributeName_,
        yAttributeName = pYAttributeName_
      }

-- |
mapMatchingConfig_idAttributeName :: Lens.Lens' MapMatchingConfig Prelude.Text
mapMatchingConfig_idAttributeName = Lens.lens (\MapMatchingConfig' {idAttributeName} -> idAttributeName) (\s@MapMatchingConfig' {} a -> s {idAttributeName = a} :: MapMatchingConfig)

-- | The name of the timestamp attribute.
mapMatchingConfig_timestampAttributeName :: Lens.Lens' MapMatchingConfig Prelude.Text
mapMatchingConfig_timestampAttributeName = Lens.lens (\MapMatchingConfig' {timestampAttributeName} -> timestampAttributeName) (\s@MapMatchingConfig' {} a -> s {timestampAttributeName = a} :: MapMatchingConfig)

-- | The name of the X-attribute
mapMatchingConfig_xAttributeName :: Lens.Lens' MapMatchingConfig Prelude.Text
mapMatchingConfig_xAttributeName = Lens.lens (\MapMatchingConfig' {xAttributeName} -> xAttributeName) (\s@MapMatchingConfig' {} a -> s {xAttributeName = a} :: MapMatchingConfig)

-- | The name of the Y-attribute
mapMatchingConfig_yAttributeName :: Lens.Lens' MapMatchingConfig Prelude.Text
mapMatchingConfig_yAttributeName = Lens.lens (\MapMatchingConfig' {yAttributeName} -> yAttributeName) (\s@MapMatchingConfig' {} a -> s {yAttributeName = a} :: MapMatchingConfig)

instance Data.FromJSON MapMatchingConfig where
  parseJSON =
    Data.withObject
      "MapMatchingConfig"
      ( \x ->
          MapMatchingConfig'
            Prelude.<$> (x Data..: "IdAttributeName")
            Prelude.<*> (x Data..: "TimestampAttributeName")
            Prelude.<*> (x Data..: "XAttributeName")
            Prelude.<*> (x Data..: "YAttributeName")
      )

instance Prelude.Hashable MapMatchingConfig where
  hashWithSalt _salt MapMatchingConfig' {..} =
    _salt `Prelude.hashWithSalt` idAttributeName
      `Prelude.hashWithSalt` timestampAttributeName
      `Prelude.hashWithSalt` xAttributeName
      `Prelude.hashWithSalt` yAttributeName

instance Prelude.NFData MapMatchingConfig where
  rnf MapMatchingConfig' {..} =
    Prelude.rnf idAttributeName
      `Prelude.seq` Prelude.rnf timestampAttributeName
      `Prelude.seq` Prelude.rnf xAttributeName
      `Prelude.seq` Prelude.rnf yAttributeName

instance Data.ToJSON MapMatchingConfig where
  toJSON MapMatchingConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("IdAttributeName" Data..= idAttributeName),
            Prelude.Just
              ( "TimestampAttributeName"
                  Data..= timestampAttributeName
              ),
            Prelude.Just
              ("XAttributeName" Data..= xAttributeName),
            Prelude.Just
              ("YAttributeName" Data..= yAttributeName)
          ]
      )
