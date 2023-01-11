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
-- Module      : Amazonka.IoTSiteWise.Types.AssociatedAssetsSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AssociatedAssetsSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.AssetHierarchy
import Amazonka.IoTSiteWise.Types.AssetStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains a summary of an associated asset.
--
-- /See:/ 'newAssociatedAssetsSummary' smart constructor.
data AssociatedAssetsSummary = AssociatedAssetsSummary'
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
    -- | The ID of the asset model used to create the asset.
    assetModelId :: Prelude.Text,
    -- | The date the asset was created, in Unix epoch time.
    creationDate :: Data.POSIX,
    -- | The date the asset was last updated, in Unix epoch time.
    lastUpdateDate :: Data.POSIX,
    -- | The current status of the asset.
    status :: AssetStatus,
    -- | A list of asset hierarchies that each contain a @hierarchyId@. A
    -- hierarchy specifies allowed parent\/child asset relationships.
    hierarchies :: [AssetHierarchy]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssociatedAssetsSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'associatedAssetsSummary_description' - A description for the asset.
--
-- 'id', 'associatedAssetsSummary_id' - The ID of the asset.
--
-- 'arn', 'associatedAssetsSummary_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the asset, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset\/${AssetId}@
--
-- 'name', 'associatedAssetsSummary_name' - The name of the asset.
--
-- 'assetModelId', 'associatedAssetsSummary_assetModelId' - The ID of the asset model used to create the asset.
--
-- 'creationDate', 'associatedAssetsSummary_creationDate' - The date the asset was created, in Unix epoch time.
--
-- 'lastUpdateDate', 'associatedAssetsSummary_lastUpdateDate' - The date the asset was last updated, in Unix epoch time.
--
-- 'status', 'associatedAssetsSummary_status' - The current status of the asset.
--
-- 'hierarchies', 'associatedAssetsSummary_hierarchies' - A list of asset hierarchies that each contain a @hierarchyId@. A
-- hierarchy specifies allowed parent\/child asset relationships.
newAssociatedAssetsSummary ::
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
  AssociatedAssetsSummary
newAssociatedAssetsSummary
  pId_
  pArn_
  pName_
  pAssetModelId_
  pCreationDate_
  pLastUpdateDate_
  pStatus_ =
    AssociatedAssetsSummary'
      { description =
          Prelude.Nothing,
        id = pId_,
        arn = pArn_,
        name = pName_,
        assetModelId = pAssetModelId_,
        creationDate = Data._Time Lens.# pCreationDate_,
        lastUpdateDate =
          Data._Time Lens.# pLastUpdateDate_,
        status = pStatus_,
        hierarchies = Prelude.mempty
      }

-- | A description for the asset.
associatedAssetsSummary_description :: Lens.Lens' AssociatedAssetsSummary (Prelude.Maybe Prelude.Text)
associatedAssetsSummary_description = Lens.lens (\AssociatedAssetsSummary' {description} -> description) (\s@AssociatedAssetsSummary' {} a -> s {description = a} :: AssociatedAssetsSummary)

-- | The ID of the asset.
associatedAssetsSummary_id :: Lens.Lens' AssociatedAssetsSummary Prelude.Text
associatedAssetsSummary_id = Lens.lens (\AssociatedAssetsSummary' {id} -> id) (\s@AssociatedAssetsSummary' {} a -> s {id = a} :: AssociatedAssetsSummary)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the asset, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset\/${AssetId}@
associatedAssetsSummary_arn :: Lens.Lens' AssociatedAssetsSummary Prelude.Text
associatedAssetsSummary_arn = Lens.lens (\AssociatedAssetsSummary' {arn} -> arn) (\s@AssociatedAssetsSummary' {} a -> s {arn = a} :: AssociatedAssetsSummary)

-- | The name of the asset.
associatedAssetsSummary_name :: Lens.Lens' AssociatedAssetsSummary Prelude.Text
associatedAssetsSummary_name = Lens.lens (\AssociatedAssetsSummary' {name} -> name) (\s@AssociatedAssetsSummary' {} a -> s {name = a} :: AssociatedAssetsSummary)

-- | The ID of the asset model used to create the asset.
associatedAssetsSummary_assetModelId :: Lens.Lens' AssociatedAssetsSummary Prelude.Text
associatedAssetsSummary_assetModelId = Lens.lens (\AssociatedAssetsSummary' {assetModelId} -> assetModelId) (\s@AssociatedAssetsSummary' {} a -> s {assetModelId = a} :: AssociatedAssetsSummary)

-- | The date the asset was created, in Unix epoch time.
associatedAssetsSummary_creationDate :: Lens.Lens' AssociatedAssetsSummary Prelude.UTCTime
associatedAssetsSummary_creationDate = Lens.lens (\AssociatedAssetsSummary' {creationDate} -> creationDate) (\s@AssociatedAssetsSummary' {} a -> s {creationDate = a} :: AssociatedAssetsSummary) Prelude.. Data._Time

-- | The date the asset was last updated, in Unix epoch time.
associatedAssetsSummary_lastUpdateDate :: Lens.Lens' AssociatedAssetsSummary Prelude.UTCTime
associatedAssetsSummary_lastUpdateDate = Lens.lens (\AssociatedAssetsSummary' {lastUpdateDate} -> lastUpdateDate) (\s@AssociatedAssetsSummary' {} a -> s {lastUpdateDate = a} :: AssociatedAssetsSummary) Prelude.. Data._Time

-- | The current status of the asset.
associatedAssetsSummary_status :: Lens.Lens' AssociatedAssetsSummary AssetStatus
associatedAssetsSummary_status = Lens.lens (\AssociatedAssetsSummary' {status} -> status) (\s@AssociatedAssetsSummary' {} a -> s {status = a} :: AssociatedAssetsSummary)

-- | A list of asset hierarchies that each contain a @hierarchyId@. A
-- hierarchy specifies allowed parent\/child asset relationships.
associatedAssetsSummary_hierarchies :: Lens.Lens' AssociatedAssetsSummary [AssetHierarchy]
associatedAssetsSummary_hierarchies = Lens.lens (\AssociatedAssetsSummary' {hierarchies} -> hierarchies) (\s@AssociatedAssetsSummary' {} a -> s {hierarchies = a} :: AssociatedAssetsSummary) Prelude.. Lens.coerced

instance Data.FromJSON AssociatedAssetsSummary where
  parseJSON =
    Data.withObject
      "AssociatedAssetsSummary"
      ( \x ->
          AssociatedAssetsSummary'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "assetModelId")
            Prelude.<*> (x Data..: "creationDate")
            Prelude.<*> (x Data..: "lastUpdateDate")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..:? "hierarchies" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable AssociatedAssetsSummary where
  hashWithSalt _salt AssociatedAssetsSummary' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` assetModelId
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` lastUpdateDate
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` hierarchies

instance Prelude.NFData AssociatedAssetsSummary where
  rnf AssociatedAssetsSummary' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf assetModelId
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf lastUpdateDate
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf hierarchies
