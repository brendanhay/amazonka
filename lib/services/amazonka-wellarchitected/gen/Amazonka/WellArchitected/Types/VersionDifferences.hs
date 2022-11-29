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
-- Module      : Amazonka.WellArchitected.Types.VersionDifferences
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WellArchitected.Types.VersionDifferences where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.WellArchitected.Types.PillarDifference

-- | The differences between the base and latest versions of the lens.
--
-- /See:/ 'newVersionDifferences' smart constructor.
data VersionDifferences = VersionDifferences'
  { -- | The differences between the base and latest versions of the lens.
    pillarDifferences :: Prelude.Maybe [PillarDifference]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VersionDifferences' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pillarDifferences', 'versionDifferences_pillarDifferences' - The differences between the base and latest versions of the lens.
newVersionDifferences ::
  VersionDifferences
newVersionDifferences =
  VersionDifferences'
    { pillarDifferences =
        Prelude.Nothing
    }

-- | The differences between the base and latest versions of the lens.
versionDifferences_pillarDifferences :: Lens.Lens' VersionDifferences (Prelude.Maybe [PillarDifference])
versionDifferences_pillarDifferences = Lens.lens (\VersionDifferences' {pillarDifferences} -> pillarDifferences) (\s@VersionDifferences' {} a -> s {pillarDifferences = a} :: VersionDifferences) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON VersionDifferences where
  parseJSON =
    Core.withObject
      "VersionDifferences"
      ( \x ->
          VersionDifferences'
            Prelude.<$> ( x Core..:? "PillarDifferences"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable VersionDifferences where
  hashWithSalt _salt VersionDifferences' {..} =
    _salt `Prelude.hashWithSalt` pillarDifferences

instance Prelude.NFData VersionDifferences where
  rnf VersionDifferences' {..} =
    Prelude.rnf pillarDifferences
