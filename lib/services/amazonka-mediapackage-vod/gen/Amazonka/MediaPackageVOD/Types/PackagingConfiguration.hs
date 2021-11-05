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
-- Module      : Amazonka.MediaPackageVOD.Types.PackagingConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.PackagingConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MediaPackageVOD.Types.CmafPackage
import Amazonka.MediaPackageVOD.Types.DashPackage
import Amazonka.MediaPackageVOD.Types.HlsPackage
import Amazonka.MediaPackageVOD.Types.MssPackage
import qualified Amazonka.Prelude as Prelude

-- | A MediaPackage VOD PackagingConfiguration resource.
--
-- /See:/ 'newPackagingConfiguration' smart constructor.
data PackagingConfiguration = PackagingConfiguration'
  { hlsPackage :: Prelude.Maybe HlsPackage,
    -- | The ARN of the PackagingConfiguration.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of a PackagingGroup.
    packagingGroupId :: Prelude.Maybe Prelude.Text,
    dashPackage :: Prelude.Maybe DashPackage,
    mssPackage :: Prelude.Maybe MssPackage,
    -- | The ID of the PackagingConfiguration.
    id :: Prelude.Maybe Prelude.Text,
    cmafPackage :: Prelude.Maybe CmafPackage,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackagingConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'hlsPackage', 'packagingConfiguration_hlsPackage' - Undocumented member.
--
-- 'arn', 'packagingConfiguration_arn' - The ARN of the PackagingConfiguration.
--
-- 'packagingGroupId', 'packagingConfiguration_packagingGroupId' - The ID of a PackagingGroup.
--
-- 'dashPackage', 'packagingConfiguration_dashPackage' - Undocumented member.
--
-- 'mssPackage', 'packagingConfiguration_mssPackage' - Undocumented member.
--
-- 'id', 'packagingConfiguration_id' - The ID of the PackagingConfiguration.
--
-- 'cmafPackage', 'packagingConfiguration_cmafPackage' - Undocumented member.
--
-- 'tags', 'packagingConfiguration_tags' - Undocumented member.
newPackagingConfiguration ::
  PackagingConfiguration
newPackagingConfiguration =
  PackagingConfiguration'
    { hlsPackage =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      packagingGroupId = Prelude.Nothing,
      dashPackage = Prelude.Nothing,
      mssPackage = Prelude.Nothing,
      id = Prelude.Nothing,
      cmafPackage = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | Undocumented member.
packagingConfiguration_hlsPackage :: Lens.Lens' PackagingConfiguration (Prelude.Maybe HlsPackage)
packagingConfiguration_hlsPackage = Lens.lens (\PackagingConfiguration' {hlsPackage} -> hlsPackage) (\s@PackagingConfiguration' {} a -> s {hlsPackage = a} :: PackagingConfiguration)

-- | The ARN of the PackagingConfiguration.
packagingConfiguration_arn :: Lens.Lens' PackagingConfiguration (Prelude.Maybe Prelude.Text)
packagingConfiguration_arn = Lens.lens (\PackagingConfiguration' {arn} -> arn) (\s@PackagingConfiguration' {} a -> s {arn = a} :: PackagingConfiguration)

-- | The ID of a PackagingGroup.
packagingConfiguration_packagingGroupId :: Lens.Lens' PackagingConfiguration (Prelude.Maybe Prelude.Text)
packagingConfiguration_packagingGroupId = Lens.lens (\PackagingConfiguration' {packagingGroupId} -> packagingGroupId) (\s@PackagingConfiguration' {} a -> s {packagingGroupId = a} :: PackagingConfiguration)

-- | Undocumented member.
packagingConfiguration_dashPackage :: Lens.Lens' PackagingConfiguration (Prelude.Maybe DashPackage)
packagingConfiguration_dashPackage = Lens.lens (\PackagingConfiguration' {dashPackage} -> dashPackage) (\s@PackagingConfiguration' {} a -> s {dashPackage = a} :: PackagingConfiguration)

-- | Undocumented member.
packagingConfiguration_mssPackage :: Lens.Lens' PackagingConfiguration (Prelude.Maybe MssPackage)
packagingConfiguration_mssPackage = Lens.lens (\PackagingConfiguration' {mssPackage} -> mssPackage) (\s@PackagingConfiguration' {} a -> s {mssPackage = a} :: PackagingConfiguration)

-- | The ID of the PackagingConfiguration.
packagingConfiguration_id :: Lens.Lens' PackagingConfiguration (Prelude.Maybe Prelude.Text)
packagingConfiguration_id = Lens.lens (\PackagingConfiguration' {id} -> id) (\s@PackagingConfiguration' {} a -> s {id = a} :: PackagingConfiguration)

-- | Undocumented member.
packagingConfiguration_cmafPackage :: Lens.Lens' PackagingConfiguration (Prelude.Maybe CmafPackage)
packagingConfiguration_cmafPackage = Lens.lens (\PackagingConfiguration' {cmafPackage} -> cmafPackage) (\s@PackagingConfiguration' {} a -> s {cmafPackage = a} :: PackagingConfiguration)

-- | Undocumented member.
packagingConfiguration_tags :: Lens.Lens' PackagingConfiguration (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
packagingConfiguration_tags = Lens.lens (\PackagingConfiguration' {tags} -> tags) (\s@PackagingConfiguration' {} a -> s {tags = a} :: PackagingConfiguration) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON PackagingConfiguration where
  parseJSON =
    Core.withObject
      "PackagingConfiguration"
      ( \x ->
          PackagingConfiguration'
            Prelude.<$> (x Core..:? "hlsPackage")
            Prelude.<*> (x Core..:? "arn")
            Prelude.<*> (x Core..:? "packagingGroupId")
            Prelude.<*> (x Core..:? "dashPackage")
            Prelude.<*> (x Core..:? "mssPackage")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "cmafPackage")
            Prelude.<*> (x Core..:? "tags" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable PackagingConfiguration

instance Prelude.NFData PackagingConfiguration
