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
-- Module      : Amazonka.OpenSearch.Types.InstanceLimits
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.InstanceLimits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types.InstanceCountLimits
import qualified Amazonka.Prelude as Prelude

-- | Instance-related attributes that are available for a given instance
-- type.
--
-- /See:/ 'newInstanceLimits' smart constructor.
data InstanceLimits = InstanceLimits'
  { -- | Limits on the number of instances that can be created for a given
    -- instance type.
    instanceCountLimits :: Prelude.Maybe InstanceCountLimits
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceLimits' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceCountLimits', 'instanceLimits_instanceCountLimits' - Limits on the number of instances that can be created for a given
-- instance type.
newInstanceLimits ::
  InstanceLimits
newInstanceLimits =
  InstanceLimits'
    { instanceCountLimits =
        Prelude.Nothing
    }

-- | Limits on the number of instances that can be created for a given
-- instance type.
instanceLimits_instanceCountLimits :: Lens.Lens' InstanceLimits (Prelude.Maybe InstanceCountLimits)
instanceLimits_instanceCountLimits = Lens.lens (\InstanceLimits' {instanceCountLimits} -> instanceCountLimits) (\s@InstanceLimits' {} a -> s {instanceCountLimits = a} :: InstanceLimits)

instance Data.FromJSON InstanceLimits where
  parseJSON =
    Data.withObject
      "InstanceLimits"
      ( \x ->
          InstanceLimits'
            Prelude.<$> (x Data..:? "InstanceCountLimits")
      )

instance Prelude.Hashable InstanceLimits where
  hashWithSalt _salt InstanceLimits' {..} =
    _salt `Prelude.hashWithSalt` instanceCountLimits

instance Prelude.NFData InstanceLimits where
  rnf InstanceLimits' {..} =
    Prelude.rnf instanceCountLimits
