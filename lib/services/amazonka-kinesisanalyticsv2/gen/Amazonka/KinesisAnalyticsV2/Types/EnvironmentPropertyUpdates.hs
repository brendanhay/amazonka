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
-- Module      : Amazonka.KinesisAnalyticsV2.Types.EnvironmentPropertyUpdates
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.KinesisAnalyticsV2.Types.EnvironmentPropertyUpdates where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.KinesisAnalyticsV2.Types.PropertyGroup
import qualified Amazonka.Prelude as Prelude

-- | Describes updates to the execution property groups for a Flink-based
-- Kinesis Data Analytics application or a Studio notebook.
--
-- /See:/ 'newEnvironmentPropertyUpdates' smart constructor.
data EnvironmentPropertyUpdates = EnvironmentPropertyUpdates'
  { -- | Describes updates to the execution property groups.
    propertyGroups :: [PropertyGroup]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentPropertyUpdates' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'propertyGroups', 'environmentPropertyUpdates_propertyGroups' - Describes updates to the execution property groups.
newEnvironmentPropertyUpdates ::
  EnvironmentPropertyUpdates
newEnvironmentPropertyUpdates =
  EnvironmentPropertyUpdates'
    { propertyGroups =
        Prelude.mempty
    }

-- | Describes updates to the execution property groups.
environmentPropertyUpdates_propertyGroups :: Lens.Lens' EnvironmentPropertyUpdates [PropertyGroup]
environmentPropertyUpdates_propertyGroups = Lens.lens (\EnvironmentPropertyUpdates' {propertyGroups} -> propertyGroups) (\s@EnvironmentPropertyUpdates' {} a -> s {propertyGroups = a} :: EnvironmentPropertyUpdates) Prelude.. Lens.coerced

instance Prelude.Hashable EnvironmentPropertyUpdates where
  hashWithSalt _salt EnvironmentPropertyUpdates' {..} =
    _salt `Prelude.hashWithSalt` propertyGroups

instance Prelude.NFData EnvironmentPropertyUpdates where
  rnf EnvironmentPropertyUpdates' {..} =
    Prelude.rnf propertyGroups

instance Core.ToJSON EnvironmentPropertyUpdates where
  toJSON EnvironmentPropertyUpdates' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("PropertyGroups" Core..= propertyGroups)
          ]
      )
