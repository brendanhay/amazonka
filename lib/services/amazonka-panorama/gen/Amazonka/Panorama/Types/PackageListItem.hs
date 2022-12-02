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
-- Module      : Amazonka.Panorama.Types.PackageListItem
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Panorama.Types.PackageListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A package summary.
--
-- /See:/ 'newPackageListItem' smart constructor.
data PackageListItem = PackageListItem'
  { -- | The package\'s tags.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The package\'s name.
    packageName :: Prelude.Maybe Prelude.Text,
    -- | When the package was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The package\'s ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The package\'s ID.
    packageId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackageListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'packageListItem_tags' - The package\'s tags.
--
-- 'packageName', 'packageListItem_packageName' - The package\'s name.
--
-- 'createdTime', 'packageListItem_createdTime' - When the package was created.
--
-- 'arn', 'packageListItem_arn' - The package\'s ARN.
--
-- 'packageId', 'packageListItem_packageId' - The package\'s ID.
newPackageListItem ::
  PackageListItem
newPackageListItem =
  PackageListItem'
    { tags = Prelude.Nothing,
      packageName = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      packageId = Prelude.Nothing
    }

-- | The package\'s tags.
packageListItem_tags :: Lens.Lens' PackageListItem (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
packageListItem_tags = Lens.lens (\PackageListItem' {tags} -> tags) (\s@PackageListItem' {} a -> s {tags = a} :: PackageListItem) Prelude.. Lens.mapping Lens.coerced

-- | The package\'s name.
packageListItem_packageName :: Lens.Lens' PackageListItem (Prelude.Maybe Prelude.Text)
packageListItem_packageName = Lens.lens (\PackageListItem' {packageName} -> packageName) (\s@PackageListItem' {} a -> s {packageName = a} :: PackageListItem)

-- | When the package was created.
packageListItem_createdTime :: Lens.Lens' PackageListItem (Prelude.Maybe Prelude.UTCTime)
packageListItem_createdTime = Lens.lens (\PackageListItem' {createdTime} -> createdTime) (\s@PackageListItem' {} a -> s {createdTime = a} :: PackageListItem) Prelude.. Lens.mapping Data._Time

-- | The package\'s ARN.
packageListItem_arn :: Lens.Lens' PackageListItem (Prelude.Maybe Prelude.Text)
packageListItem_arn = Lens.lens (\PackageListItem' {arn} -> arn) (\s@PackageListItem' {} a -> s {arn = a} :: PackageListItem)

-- | The package\'s ID.
packageListItem_packageId :: Lens.Lens' PackageListItem (Prelude.Maybe Prelude.Text)
packageListItem_packageId = Lens.lens (\PackageListItem' {packageId} -> packageId) (\s@PackageListItem' {} a -> s {packageId = a} :: PackageListItem)

instance Data.FromJSON PackageListItem where
  parseJSON =
    Data.withObject
      "PackageListItem"
      ( \x ->
          PackageListItem'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "PackageName")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "PackageId")
      )

instance Prelude.Hashable PackageListItem where
  hashWithSalt _salt PackageListItem' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` packageName
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` packageId

instance Prelude.NFData PackageListItem where
  rnf PackageListItem' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf packageName
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf packageId
