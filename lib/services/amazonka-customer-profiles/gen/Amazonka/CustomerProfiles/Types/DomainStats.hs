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
-- Module      : Amazonka.CustomerProfiles.Types.DomainStats
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.DomainStats where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Usage-specific statistics about the domain.
--
-- /See:/ 'newDomainStats' smart constructor.
data DomainStats = DomainStats'
  { -- | The number of profiles that you are currently paying for in the domain.
    -- If you have more than 100 objects associated with a single profile, that
    -- profile counts as two profiles. If you have more than 200 objects, that
    -- profile counts as three, and so on.
    meteringProfileCount :: Prelude.Maybe Prelude.Integer,
    -- | The total number of profiles currently in the domain.
    profileCount :: Prelude.Maybe Prelude.Integer,
    -- | The total number of objects in domain.
    objectCount :: Prelude.Maybe Prelude.Integer,
    -- | The total size, in bytes, of all objects in the domain.
    totalSize :: Prelude.Maybe Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DomainStats' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'meteringProfileCount', 'domainStats_meteringProfileCount' - The number of profiles that you are currently paying for in the domain.
-- If you have more than 100 objects associated with a single profile, that
-- profile counts as two profiles. If you have more than 200 objects, that
-- profile counts as three, and so on.
--
-- 'profileCount', 'domainStats_profileCount' - The total number of profiles currently in the domain.
--
-- 'objectCount', 'domainStats_objectCount' - The total number of objects in domain.
--
-- 'totalSize', 'domainStats_totalSize' - The total size, in bytes, of all objects in the domain.
newDomainStats ::
  DomainStats
newDomainStats =
  DomainStats'
    { meteringProfileCount =
        Prelude.Nothing,
      profileCount = Prelude.Nothing,
      objectCount = Prelude.Nothing,
      totalSize = Prelude.Nothing
    }

-- | The number of profiles that you are currently paying for in the domain.
-- If you have more than 100 objects associated with a single profile, that
-- profile counts as two profiles. If you have more than 200 objects, that
-- profile counts as three, and so on.
domainStats_meteringProfileCount :: Lens.Lens' DomainStats (Prelude.Maybe Prelude.Integer)
domainStats_meteringProfileCount = Lens.lens (\DomainStats' {meteringProfileCount} -> meteringProfileCount) (\s@DomainStats' {} a -> s {meteringProfileCount = a} :: DomainStats)

-- | The total number of profiles currently in the domain.
domainStats_profileCount :: Lens.Lens' DomainStats (Prelude.Maybe Prelude.Integer)
domainStats_profileCount = Lens.lens (\DomainStats' {profileCount} -> profileCount) (\s@DomainStats' {} a -> s {profileCount = a} :: DomainStats)

-- | The total number of objects in domain.
domainStats_objectCount :: Lens.Lens' DomainStats (Prelude.Maybe Prelude.Integer)
domainStats_objectCount = Lens.lens (\DomainStats' {objectCount} -> objectCount) (\s@DomainStats' {} a -> s {objectCount = a} :: DomainStats)

-- | The total size, in bytes, of all objects in the domain.
domainStats_totalSize :: Lens.Lens' DomainStats (Prelude.Maybe Prelude.Integer)
domainStats_totalSize = Lens.lens (\DomainStats' {totalSize} -> totalSize) (\s@DomainStats' {} a -> s {totalSize = a} :: DomainStats)

instance Core.FromJSON DomainStats where
  parseJSON =
    Core.withObject
      "DomainStats"
      ( \x ->
          DomainStats'
            Prelude.<$> (x Core..:? "MeteringProfileCount")
            Prelude.<*> (x Core..:? "ProfileCount")
            Prelude.<*> (x Core..:? "ObjectCount")
            Prelude.<*> (x Core..:? "TotalSize")
      )

instance Prelude.Hashable DomainStats where
  hashWithSalt _salt DomainStats' {..} =
    _salt `Prelude.hashWithSalt` meteringProfileCount
      `Prelude.hashWithSalt` profileCount
      `Prelude.hashWithSalt` objectCount
      `Prelude.hashWithSalt` totalSize

instance Prelude.NFData DomainStats where
  rnf DomainStats' {..} =
    Prelude.rnf meteringProfileCount
      `Prelude.seq` Prelude.rnf profileCount
      `Prelude.seq` Prelude.rnf objectCount
      `Prelude.seq` Prelude.rnf totalSize
