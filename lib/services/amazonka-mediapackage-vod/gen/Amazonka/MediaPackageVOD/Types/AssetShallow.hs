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
-- Module      : Amazonka.MediaPackageVOD.Types.AssetShallow
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaPackageVOD.Types.AssetShallow where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A MediaPackage VOD Asset resource.
--
-- /See:/ 'newAssetShallow' smart constructor.
data AssetShallow = AssetShallow'
  { -- | The resource ID to include in SPEKE key requests.
    resourceId :: Prelude.Maybe Prelude.Text,
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The IAM role ARN used to access the source S3 bucket.
    sourceRoleArn :: Prelude.Maybe Prelude.Text,
    -- | ARN of the source object in S3.
    sourceArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the PackagingGroup for the Asset.
    packagingGroupId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the Asset.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the Asset.
    id :: Prelude.Maybe Prelude.Text,
    -- | The time the Asset was initially submitted for Ingest.
    createdAt :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetShallow' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'assetShallow_resourceId' - The resource ID to include in SPEKE key requests.
--
-- 'tags', 'assetShallow_tags' - Undocumented member.
--
-- 'sourceRoleArn', 'assetShallow_sourceRoleArn' - The IAM role ARN used to access the source S3 bucket.
--
-- 'sourceArn', 'assetShallow_sourceArn' - ARN of the source object in S3.
--
-- 'packagingGroupId', 'assetShallow_packagingGroupId' - The ID of the PackagingGroup for the Asset.
--
-- 'arn', 'assetShallow_arn' - The ARN of the Asset.
--
-- 'id', 'assetShallow_id' - The unique identifier for the Asset.
--
-- 'createdAt', 'assetShallow_createdAt' - The time the Asset was initially submitted for Ingest.
newAssetShallow ::
  AssetShallow
newAssetShallow =
  AssetShallow'
    { resourceId = Prelude.Nothing,
      tags = Prelude.Nothing,
      sourceRoleArn = Prelude.Nothing,
      sourceArn = Prelude.Nothing,
      packagingGroupId = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing,
      createdAt = Prelude.Nothing
    }

-- | The resource ID to include in SPEKE key requests.
assetShallow_resourceId :: Lens.Lens' AssetShallow (Prelude.Maybe Prelude.Text)
assetShallow_resourceId = Lens.lens (\AssetShallow' {resourceId} -> resourceId) (\s@AssetShallow' {} a -> s {resourceId = a} :: AssetShallow)

-- | Undocumented member.
assetShallow_tags :: Lens.Lens' AssetShallow (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
assetShallow_tags = Lens.lens (\AssetShallow' {tags} -> tags) (\s@AssetShallow' {} a -> s {tags = a} :: AssetShallow) Prelude.. Lens.mapping Lens.coerced

-- | The IAM role ARN used to access the source S3 bucket.
assetShallow_sourceRoleArn :: Lens.Lens' AssetShallow (Prelude.Maybe Prelude.Text)
assetShallow_sourceRoleArn = Lens.lens (\AssetShallow' {sourceRoleArn} -> sourceRoleArn) (\s@AssetShallow' {} a -> s {sourceRoleArn = a} :: AssetShallow)

-- | ARN of the source object in S3.
assetShallow_sourceArn :: Lens.Lens' AssetShallow (Prelude.Maybe Prelude.Text)
assetShallow_sourceArn = Lens.lens (\AssetShallow' {sourceArn} -> sourceArn) (\s@AssetShallow' {} a -> s {sourceArn = a} :: AssetShallow)

-- | The ID of the PackagingGroup for the Asset.
assetShallow_packagingGroupId :: Lens.Lens' AssetShallow (Prelude.Maybe Prelude.Text)
assetShallow_packagingGroupId = Lens.lens (\AssetShallow' {packagingGroupId} -> packagingGroupId) (\s@AssetShallow' {} a -> s {packagingGroupId = a} :: AssetShallow)

-- | The ARN of the Asset.
assetShallow_arn :: Lens.Lens' AssetShallow (Prelude.Maybe Prelude.Text)
assetShallow_arn = Lens.lens (\AssetShallow' {arn} -> arn) (\s@AssetShallow' {} a -> s {arn = a} :: AssetShallow)

-- | The unique identifier for the Asset.
assetShallow_id :: Lens.Lens' AssetShallow (Prelude.Maybe Prelude.Text)
assetShallow_id = Lens.lens (\AssetShallow' {id} -> id) (\s@AssetShallow' {} a -> s {id = a} :: AssetShallow)

-- | The time the Asset was initially submitted for Ingest.
assetShallow_createdAt :: Lens.Lens' AssetShallow (Prelude.Maybe Prelude.Text)
assetShallow_createdAt = Lens.lens (\AssetShallow' {createdAt} -> createdAt) (\s@AssetShallow' {} a -> s {createdAt = a} :: AssetShallow)

instance Data.FromJSON AssetShallow where
  parseJSON =
    Data.withObject
      "AssetShallow"
      ( \x ->
          AssetShallow'
            Prelude.<$> (x Data..:? "resourceId")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "sourceRoleArn")
            Prelude.<*> (x Data..:? "sourceArn")
            Prelude.<*> (x Data..:? "packagingGroupId")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "createdAt")
      )

instance Prelude.Hashable AssetShallow where
  hashWithSalt _salt AssetShallow' {..} =
    _salt `Prelude.hashWithSalt` resourceId
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` sourceRoleArn
      `Prelude.hashWithSalt` sourceArn
      `Prelude.hashWithSalt` packagingGroupId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` createdAt

instance Prelude.NFData AssetShallow where
  rnf AssetShallow' {..} =
    Prelude.rnf resourceId
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf sourceRoleArn
      `Prelude.seq` Prelude.rnf sourceArn
      `Prelude.seq` Prelude.rnf packagingGroupId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf createdAt
