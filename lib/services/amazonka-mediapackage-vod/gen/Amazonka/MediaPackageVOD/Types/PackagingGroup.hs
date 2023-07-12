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
-- Module      : Amazonka.MediaPackageVOD.Types.PackagingGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.PackagingGroup where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MediaPackageVOD.Types.Authorization
import Amazonka.MediaPackageVOD.Types.EgressAccessLogs
import qualified Amazonka.Prelude as Prelude

-- | A MediaPackage VOD PackagingGroup resource.
--
-- /See:/ 'newPackagingGroup' smart constructor.
data PackagingGroup = PackagingGroup'
  { -- | The approximate asset count of the PackagingGroup.
    approximateAssetCount :: Prelude.Maybe Prelude.Int,
    -- | The ARN of the PackagingGroup.
    arn :: Prelude.Maybe Prelude.Text,
    authorization :: Prelude.Maybe Authorization,
    -- | The fully qualified domain name for Assets in the PackagingGroup.
    domainName :: Prelude.Maybe Prelude.Text,
    egressAccessLogs :: Prelude.Maybe EgressAccessLogs,
    -- | The ID of the PackagingGroup.
    id :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PackagingGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'approximateAssetCount', 'packagingGroup_approximateAssetCount' - The approximate asset count of the PackagingGroup.
--
-- 'arn', 'packagingGroup_arn' - The ARN of the PackagingGroup.
--
-- 'authorization', 'packagingGroup_authorization' - Undocumented member.
--
-- 'domainName', 'packagingGroup_domainName' - The fully qualified domain name for Assets in the PackagingGroup.
--
-- 'egressAccessLogs', 'packagingGroup_egressAccessLogs' - Undocumented member.
--
-- 'id', 'packagingGroup_id' - The ID of the PackagingGroup.
--
-- 'tags', 'packagingGroup_tags' - Undocumented member.
newPackagingGroup ::
  PackagingGroup
newPackagingGroup =
  PackagingGroup'
    { approximateAssetCount =
        Prelude.Nothing,
      arn = Prelude.Nothing,
      authorization = Prelude.Nothing,
      domainName = Prelude.Nothing,
      egressAccessLogs = Prelude.Nothing,
      id = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The approximate asset count of the PackagingGroup.
packagingGroup_approximateAssetCount :: Lens.Lens' PackagingGroup (Prelude.Maybe Prelude.Int)
packagingGroup_approximateAssetCount = Lens.lens (\PackagingGroup' {approximateAssetCount} -> approximateAssetCount) (\s@PackagingGroup' {} a -> s {approximateAssetCount = a} :: PackagingGroup)

-- | The ARN of the PackagingGroup.
packagingGroup_arn :: Lens.Lens' PackagingGroup (Prelude.Maybe Prelude.Text)
packagingGroup_arn = Lens.lens (\PackagingGroup' {arn} -> arn) (\s@PackagingGroup' {} a -> s {arn = a} :: PackagingGroup)

-- | Undocumented member.
packagingGroup_authorization :: Lens.Lens' PackagingGroup (Prelude.Maybe Authorization)
packagingGroup_authorization = Lens.lens (\PackagingGroup' {authorization} -> authorization) (\s@PackagingGroup' {} a -> s {authorization = a} :: PackagingGroup)

-- | The fully qualified domain name for Assets in the PackagingGroup.
packagingGroup_domainName :: Lens.Lens' PackagingGroup (Prelude.Maybe Prelude.Text)
packagingGroup_domainName = Lens.lens (\PackagingGroup' {domainName} -> domainName) (\s@PackagingGroup' {} a -> s {domainName = a} :: PackagingGroup)

-- | Undocumented member.
packagingGroup_egressAccessLogs :: Lens.Lens' PackagingGroup (Prelude.Maybe EgressAccessLogs)
packagingGroup_egressAccessLogs = Lens.lens (\PackagingGroup' {egressAccessLogs} -> egressAccessLogs) (\s@PackagingGroup' {} a -> s {egressAccessLogs = a} :: PackagingGroup)

-- | The ID of the PackagingGroup.
packagingGroup_id :: Lens.Lens' PackagingGroup (Prelude.Maybe Prelude.Text)
packagingGroup_id = Lens.lens (\PackagingGroup' {id} -> id) (\s@PackagingGroup' {} a -> s {id = a} :: PackagingGroup)

-- | Undocumented member.
packagingGroup_tags :: Lens.Lens' PackagingGroup (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
packagingGroup_tags = Lens.lens (\PackagingGroup' {tags} -> tags) (\s@PackagingGroup' {} a -> s {tags = a} :: PackagingGroup) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON PackagingGroup where
  parseJSON =
    Data.withObject
      "PackagingGroup"
      ( \x ->
          PackagingGroup'
            Prelude.<$> (x Data..:? "approximateAssetCount")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "authorization")
            Prelude.<*> (x Data..:? "domainName")
            Prelude.<*> (x Data..:? "egressAccessLogs")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable PackagingGroup where
  hashWithSalt _salt PackagingGroup' {..} =
    _salt
      `Prelude.hashWithSalt` approximateAssetCount
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` authorization
      `Prelude.hashWithSalt` domainName
      `Prelude.hashWithSalt` egressAccessLogs
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` tags

instance Prelude.NFData PackagingGroup where
  rnf PackagingGroup' {..} =
    Prelude.rnf approximateAssetCount
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf authorization
      `Prelude.seq` Prelude.rnf domainName
      `Prelude.seq` Prelude.rnf egressAccessLogs
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf tags
