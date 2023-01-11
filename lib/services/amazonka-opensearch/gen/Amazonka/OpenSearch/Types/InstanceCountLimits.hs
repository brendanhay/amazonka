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
-- Module      : Amazonka.OpenSearch.Types.InstanceCountLimits
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.InstanceCountLimits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Limits on the number of instances that can be created in OpenSearch
-- Service for a given instance type.
--
-- /See:/ 'newInstanceCountLimits' smart constructor.
data InstanceCountLimits = InstanceCountLimits'
  { -- | The minimum allowed number of instances.
    maximumInstanceCount :: Prelude.Maybe Prelude.Int,
    -- | The maximum allowed number of instances.
    minimumInstanceCount :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceCountLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maximumInstanceCount', 'instanceCountLimits_maximumInstanceCount' - The minimum allowed number of instances.
--
-- 'minimumInstanceCount', 'instanceCountLimits_minimumInstanceCount' - The maximum allowed number of instances.
newInstanceCountLimits ::
  InstanceCountLimits
newInstanceCountLimits =
  InstanceCountLimits'
    { maximumInstanceCount =
        Prelude.Nothing,
      minimumInstanceCount = Prelude.Nothing
    }

-- | The minimum allowed number of instances.
instanceCountLimits_maximumInstanceCount :: Lens.Lens' InstanceCountLimits (Prelude.Maybe Prelude.Int)
instanceCountLimits_maximumInstanceCount = Lens.lens (\InstanceCountLimits' {maximumInstanceCount} -> maximumInstanceCount) (\s@InstanceCountLimits' {} a -> s {maximumInstanceCount = a} :: InstanceCountLimits)

-- | The maximum allowed number of instances.
instanceCountLimits_minimumInstanceCount :: Lens.Lens' InstanceCountLimits (Prelude.Maybe Prelude.Int)
instanceCountLimits_minimumInstanceCount = Lens.lens (\InstanceCountLimits' {minimumInstanceCount} -> minimumInstanceCount) (\s@InstanceCountLimits' {} a -> s {minimumInstanceCount = a} :: InstanceCountLimits)

instance Data.FromJSON InstanceCountLimits where
  parseJSON =
    Data.withObject
      "InstanceCountLimits"
      ( \x ->
          InstanceCountLimits'
            Prelude.<$> (x Data..:? "MaximumInstanceCount")
            Prelude.<*> (x Data..:? "MinimumInstanceCount")
      )

instance Prelude.Hashable InstanceCountLimits where
  hashWithSalt _salt InstanceCountLimits' {..} =
    _salt `Prelude.hashWithSalt` maximumInstanceCount
      `Prelude.hashWithSalt` minimumInstanceCount

instance Prelude.NFData InstanceCountLimits where
  rnf InstanceCountLimits' {..} =
    Prelude.rnf maximumInstanceCount
      `Prelude.seq` Prelude.rnf minimumInstanceCount
