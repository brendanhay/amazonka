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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.EnvironmentProperties
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.EnvironmentProperties where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisAnalyticsV2.Types.PropertyGroup
import qualified Amazonka.Prelude as Prelude

-- | Describes execution properties for a Flink-based Kinesis Data Analytics
-- application.
--
-- /See:/ 'newEnvironmentProperties' smart constructor.
data EnvironmentProperties = EnvironmentProperties'
  { -- | Describes the execution property groups.
    propertyGroups :: [PropertyGroup]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentProperties' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propertyGroups', 'environmentProperties_propertyGroups' - Describes the execution property groups.
newEnvironmentProperties ::
  EnvironmentProperties
newEnvironmentProperties =
  EnvironmentProperties'
    { propertyGroups =
        Prelude.mempty
    }

-- | Describes the execution property groups.
environmentProperties_propertyGroups :: Lens.Lens' EnvironmentProperties [PropertyGroup]
environmentProperties_propertyGroups = Lens.lens (\EnvironmentProperties' {propertyGroups} -> propertyGroups) (\s@EnvironmentProperties' {} a -> s {propertyGroups = a} :: EnvironmentProperties) Prelude.. Lens.coerced

instance Prelude.Hashable EnvironmentProperties where
  hashWithSalt _salt EnvironmentProperties' {..} =
    _salt `Prelude.hashWithSalt` propertyGroups

instance Prelude.NFData EnvironmentProperties where
  rnf EnvironmentProperties' {..} =
    Prelude.rnf propertyGroups

instance Core.ToJSON EnvironmentProperties where
  toJSON EnvironmentProperties' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PropertyGroups" Core..= propertyGroups)
          ]
      )
