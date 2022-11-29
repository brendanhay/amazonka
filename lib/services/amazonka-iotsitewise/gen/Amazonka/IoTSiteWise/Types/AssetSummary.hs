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
-- Module      : Amazonka.IoTSiteWise.Types.AssetSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AssetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.IoTSiteWise.Types.AssetHierarchy
import Amazonka.IoTSiteWise.Types.AssetStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains a summary of an asset.
--
-- /See:/ 'newAssetSummary' smart constructor.
data AssetSummary = AssetSummary'
  { -- | A description for the asset.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the asset.
    id :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the asset, which has the following format.
    --
    -- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset\/${AssetId}@
    arn :: Prelude.Text,
    -- | The name of the asset.
    name :: Prelude.Text,
    -- | The ID of the asset model used to create this asset.
    assetModelId :: Prelude.Text,
    -- | The date the asset was created, in Unix epoch time.
    creationDate :: Core.POSIX,
    -- | The date the asset was last updated, in Unix epoch time.
    lastUpdateDate :: Core.POSIX,
    -- | The current status of the asset.
    status :: AssetStatus,
    -- | A list of asset hierarchies that each contain a @hierarchyId@. A
    -- hierarchy specifies allowed parent\/child asset relationships.
    hierarchies :: [AssetHierarchy]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'assetSummary_description' - A description for the asset.
--
-- 'id', 'assetSummary_id' - The ID of the asset.
--
-- 'arn', 'assetSummary_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the asset, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset\/${AssetId}@
--
-- 'name', 'assetSummary_name' - The name of the asset.
--
-- 'assetModelId', 'assetSummary_assetModelId' - The ID of the asset model used to create this asset.
--
-- 'creationDate', 'assetSummary_creationDate' - The date the asset was created, in Unix epoch time.
--
-- 'lastUpdateDate', 'assetSummary_lastUpdateDate' - The date the asset was last updated, in Unix epoch time.
--
-- 'status', 'assetSummary_status' - The current status of the asset.
--
-- 'hierarchies', 'assetSummary_hierarchies' - A list of asset hierarchies that each contain a @hierarchyId@. A
-- hierarchy specifies allowed parent\/child asset relationships.
newAssetSummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'assetModelId'
  Prelude.Text ->
  -- | 'creationDate'
  Prelude.UTCTime ->
  -- | 'lastUpdateDate'
  Prelude.UTCTime ->
  -- | 'status'
  AssetStatus ->
  AssetSummary
newAssetSummary
  pId_
  pArn_
  pName_
  pAssetModelId_
  pCreationDate_
  pLastUpdateDate_
  pStatus_ =
    AssetSummary'
      { description = Prelude.Nothing,
        id = pId_,
        arn = pArn_,
        name = pName_,
        assetModelId = pAssetModelId_,
        creationDate = Core._Time Lens.# pCreationDate_,
        lastUpdateDate = Core._Time Lens.# pLastUpdateDate_,
        status = pStatus_,
        hierarchies = Prelude.mempty
      }

-- | A description for the asset.
assetSummary_description :: Lens.Lens' AssetSummary (Prelude.Maybe Prelude.Text)
assetSummary_description = Lens.lens (\AssetSummary' {description} -> description) (\s@AssetSummary' {} a -> s {description = a} :: AssetSummary)

-- | The ID of the asset.
assetSummary_id :: Lens.Lens' AssetSummary Prelude.Text
assetSummary_id = Lens.lens (\AssetSummary' {id} -> id) (\s@AssetSummary' {} a -> s {id = a} :: AssetSummary)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the asset, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset\/${AssetId}@
assetSummary_arn :: Lens.Lens' AssetSummary Prelude.Text
assetSummary_arn = Lens.lens (\AssetSummary' {arn} -> arn) (\s@AssetSummary' {} a -> s {arn = a} :: AssetSummary)

-- | The name of the asset.
assetSummary_name :: Lens.Lens' AssetSummary Prelude.Text
assetSummary_name = Lens.lens (\AssetSummary' {name} -> name) (\s@AssetSummary' {} a -> s {name = a} :: AssetSummary)

-- | The ID of the asset model used to create this asset.
assetSummary_assetModelId :: Lens.Lens' AssetSummary Prelude.Text
assetSummary_assetModelId = Lens.lens (\AssetSummary' {assetModelId} -> assetModelId) (\s@AssetSummary' {} a -> s {assetModelId = a} :: AssetSummary)

-- | The date the asset was created, in Unix epoch time.
assetSummary_creationDate :: Lens.Lens' AssetSummary Prelude.UTCTime
assetSummary_creationDate = Lens.lens (\AssetSummary' {creationDate} -> creationDate) (\s@AssetSummary' {} a -> s {creationDate = a} :: AssetSummary) Prelude.. Core._Time

-- | The date the asset was last updated, in Unix epoch time.
assetSummary_lastUpdateDate :: Lens.Lens' AssetSummary Prelude.UTCTime
assetSummary_lastUpdateDate = Lens.lens (\AssetSummary' {lastUpdateDate} -> lastUpdateDate) (\s@AssetSummary' {} a -> s {lastUpdateDate = a} :: AssetSummary) Prelude.. Core._Time

-- | The current status of the asset.
assetSummary_status :: Lens.Lens' AssetSummary AssetStatus
assetSummary_status = Lens.lens (\AssetSummary' {status} -> status) (\s@AssetSummary' {} a -> s {status = a} :: AssetSummary)

-- | A list of asset hierarchies that each contain a @hierarchyId@. A
-- hierarchy specifies allowed parent\/child asset relationships.
assetSummary_hierarchies :: Lens.Lens' AssetSummary [AssetHierarchy]
assetSummary_hierarchies = Lens.lens (\AssetSummary' {hierarchies} -> hierarchies) (\s@AssetSummary' {} a -> s {hierarchies = a} :: AssetSummary) Prelude.. Lens.coerced

instance Core.FromJSON AssetSummary where
  parseJSON =
    Core.withObject
      "AssetSummary"
      ( \x ->
          AssetSummary'
            Prelude.<$> (x Core..:? "description")
            Prelude.<*> (x Core..: "id")
            Prelude.<*> (x Core..: "arn")
            Prelude.<*> (x Core..: "name")
            Prelude.<*> (x Core..: "assetModelId")
            Prelude.<*> (x Core..: "creationDate")
            Prelude.<*> (x Core..: "lastUpdateDate")
            Prelude.<*> (x Core..: "status")
            Prelude.<*> (x Core..:? "hierarchies" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable AssetSummary where
  hashWithSalt _salt AssetSummary' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` assetModelId
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` lastUpdateDate
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` hierarchies

instance Prelude.NFData AssetSummary where
  rnf AssetSummary' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf assetModelId
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf lastUpdateDate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf hierarchies
