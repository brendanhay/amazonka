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
-- Module      : Amazonka.QuickSight.Types.ReferenceLineDataConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.ReferenceLineDataConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.AxisBinding
import Amazonka.QuickSight.Types.ReferenceLineDynamicDataConfiguration
import Amazonka.QuickSight.Types.ReferenceLineStaticDataConfiguration

-- | The data configuration of the reference line.
--
-- /See:/ 'newReferenceLineDataConfiguration' smart constructor.
data ReferenceLineDataConfiguration = ReferenceLineDataConfiguration'
  { -- | The axis binding type of the reference line. Choose one of the following
    -- options:
    --
    -- -   PrimaryY
    --
    -- -   SecondaryY
    axisBinding :: Prelude.Maybe AxisBinding,
    -- | The dynamic configuration of the reference line data configuration.
    dynamicConfiguration :: Prelude.Maybe ReferenceLineDynamicDataConfiguration,
    -- | The static data configuration of the reference line data configuration.
    staticConfiguration :: Prelude.Maybe ReferenceLineStaticDataConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferenceLineDataConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'axisBinding', 'referenceLineDataConfiguration_axisBinding' - The axis binding type of the reference line. Choose one of the following
-- options:
--
-- -   PrimaryY
--
-- -   SecondaryY
--
-- 'dynamicConfiguration', 'referenceLineDataConfiguration_dynamicConfiguration' - The dynamic configuration of the reference line data configuration.
--
-- 'staticConfiguration', 'referenceLineDataConfiguration_staticConfiguration' - The static data configuration of the reference line data configuration.
newReferenceLineDataConfiguration ::
  ReferenceLineDataConfiguration
newReferenceLineDataConfiguration =
  ReferenceLineDataConfiguration'
    { axisBinding =
        Prelude.Nothing,
      dynamicConfiguration = Prelude.Nothing,
      staticConfiguration = Prelude.Nothing
    }

-- | The axis binding type of the reference line. Choose one of the following
-- options:
--
-- -   PrimaryY
--
-- -   SecondaryY
referenceLineDataConfiguration_axisBinding :: Lens.Lens' ReferenceLineDataConfiguration (Prelude.Maybe AxisBinding)
referenceLineDataConfiguration_axisBinding = Lens.lens (\ReferenceLineDataConfiguration' {axisBinding} -> axisBinding) (\s@ReferenceLineDataConfiguration' {} a -> s {axisBinding = a} :: ReferenceLineDataConfiguration)

-- | The dynamic configuration of the reference line data configuration.
referenceLineDataConfiguration_dynamicConfiguration :: Lens.Lens' ReferenceLineDataConfiguration (Prelude.Maybe ReferenceLineDynamicDataConfiguration)
referenceLineDataConfiguration_dynamicConfiguration = Lens.lens (\ReferenceLineDataConfiguration' {dynamicConfiguration} -> dynamicConfiguration) (\s@ReferenceLineDataConfiguration' {} a -> s {dynamicConfiguration = a} :: ReferenceLineDataConfiguration)

-- | The static data configuration of the reference line data configuration.
referenceLineDataConfiguration_staticConfiguration :: Lens.Lens' ReferenceLineDataConfiguration (Prelude.Maybe ReferenceLineStaticDataConfiguration)
referenceLineDataConfiguration_staticConfiguration = Lens.lens (\ReferenceLineDataConfiguration' {staticConfiguration} -> staticConfiguration) (\s@ReferenceLineDataConfiguration' {} a -> s {staticConfiguration = a} :: ReferenceLineDataConfiguration)

instance Data.FromJSON ReferenceLineDataConfiguration where
  parseJSON =
    Data.withObject
      "ReferenceLineDataConfiguration"
      ( \x ->
          ReferenceLineDataConfiguration'
            Prelude.<$> (x Data..:? "AxisBinding")
            Prelude.<*> (x Data..:? "DynamicConfiguration")
            Prelude.<*> (x Data..:? "StaticConfiguration")
      )

instance
  Prelude.Hashable
    ReferenceLineDataConfiguration
  where
  hashWithSalt
    _salt
    ReferenceLineDataConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` axisBinding
        `Prelude.hashWithSalt` dynamicConfiguration
        `Prelude.hashWithSalt` staticConfiguration

instance
  Prelude.NFData
    ReferenceLineDataConfiguration
  where
  rnf ReferenceLineDataConfiguration' {..} =
    Prelude.rnf axisBinding `Prelude.seq`
      Prelude.rnf dynamicConfiguration `Prelude.seq`
        Prelude.rnf staticConfiguration

instance Data.ToJSON ReferenceLineDataConfiguration where
  toJSON ReferenceLineDataConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AxisBinding" Data..=) Prelude.<$> axisBinding,
            ("DynamicConfiguration" Data..=)
              Prelude.<$> dynamicConfiguration,
            ("StaticConfiguration" Data..=)
              Prelude.<$> staticConfiguration
          ]
      )
