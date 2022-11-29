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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.EnvironmentPropertyDescriptions
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.EnvironmentPropertyDescriptions where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisAnalyticsV2.Types.PropertyGroup
import qualified Amazonka.Prelude as Prelude

-- | Describes the execution properties for an Apache Flink runtime.
--
-- /See:/ 'newEnvironmentPropertyDescriptions' smart constructor.
data EnvironmentPropertyDescriptions = EnvironmentPropertyDescriptions'
  { -- | Describes the execution property groups.
    propertyGroupDescriptions :: Prelude.Maybe [PropertyGroup]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentPropertyDescriptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propertyGroupDescriptions', 'environmentPropertyDescriptions_propertyGroupDescriptions' - Describes the execution property groups.
newEnvironmentPropertyDescriptions ::
  EnvironmentPropertyDescriptions
newEnvironmentPropertyDescriptions =
  EnvironmentPropertyDescriptions'
    { propertyGroupDescriptions =
        Prelude.Nothing
    }

-- | Describes the execution property groups.
environmentPropertyDescriptions_propertyGroupDescriptions :: Lens.Lens' EnvironmentPropertyDescriptions (Prelude.Maybe [PropertyGroup])
environmentPropertyDescriptions_propertyGroupDescriptions = Lens.lens (\EnvironmentPropertyDescriptions' {propertyGroupDescriptions} -> propertyGroupDescriptions) (\s@EnvironmentPropertyDescriptions' {} a -> s {propertyGroupDescriptions = a} :: EnvironmentPropertyDescriptions) Prelude.. Lens.mapping Lens.coerced

instance
  Core.FromJSON
    EnvironmentPropertyDescriptions
  where
  parseJSON =
    Core.withObject
      "EnvironmentPropertyDescriptions"
      ( \x ->
          EnvironmentPropertyDescriptions'
            Prelude.<$> ( x Core..:? "PropertyGroupDescriptions"
                            Core..!= Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    EnvironmentPropertyDescriptions
  where
  hashWithSalt
    _salt
    EnvironmentPropertyDescriptions' {..} =
      _salt
        `Prelude.hashWithSalt` propertyGroupDescriptions

instance
  Prelude.NFData
    EnvironmentPropertyDescriptions
  where
  rnf EnvironmentPropertyDescriptions' {..} =
    Prelude.rnf propertyGroupDescriptions
