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
-- Module      : Amazonka.ElasticSearch.Types.InstanceLimits
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElasticSearch.Types.InstanceLimits where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types.InstanceCountLimits
import qualified Amazonka.Prelude as Prelude

-- | InstanceLimits represents the list of instance related attributes that
-- are available for given InstanceType.
--
-- /See:/ 'newInstanceLimits' smart constructor.
data InstanceLimits = InstanceLimits'
  { instanceCountLimits :: Prelude.Maybe InstanceCountLimits
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
-- 'instanceCountLimits', 'instanceLimits_instanceCountLimits' - Undocumented member.
newInstanceLimits ::
  InstanceLimits
newInstanceLimits =
  InstanceLimits'
    { instanceCountLimits =
        Prelude.Nothing
    }

-- | Undocumented member.
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
