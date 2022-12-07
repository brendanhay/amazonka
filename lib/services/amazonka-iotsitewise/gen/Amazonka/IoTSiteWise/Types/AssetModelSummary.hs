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
-- Module      : Amazonka.IoTSiteWise.Types.AssetModelSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTSiteWise.Types.AssetModelSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTSiteWise.Types.AssetModelStatus
import qualified Amazonka.Prelude as Prelude

-- | Contains a summary of an asset model.
--
-- /See:/ 'newAssetModelSummary' smart constructor.
data AssetModelSummary = AssetModelSummary'
  { -- | The ID of the asset model (used with IoT SiteWise APIs).
    id :: Prelude.Text,
    -- | The
    -- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
    -- of the asset model, which has the following format.
    --
    -- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset-model\/${AssetModelId}@
    arn :: Prelude.Text,
    -- | The name of the asset model.
    name :: Prelude.Text,
    -- | The asset model description.
    description :: Prelude.Text,
    -- | The date the asset model was created, in Unix epoch time.
    creationDate :: Data.POSIX,
    -- | The date the asset model was last updated, in Unix epoch time.
    lastUpdateDate :: Data.POSIX,
    -- | The current status of the asset model.
    status :: AssetModelStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AssetModelSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'assetModelSummary_id' - The ID of the asset model (used with IoT SiteWise APIs).
--
-- 'arn', 'assetModelSummary_arn' - The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the asset model, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset-model\/${AssetModelId}@
--
-- 'name', 'assetModelSummary_name' - The name of the asset model.
--
-- 'description', 'assetModelSummary_description' - The asset model description.
--
-- 'creationDate', 'assetModelSummary_creationDate' - The date the asset model was created, in Unix epoch time.
--
-- 'lastUpdateDate', 'assetModelSummary_lastUpdateDate' - The date the asset model was last updated, in Unix epoch time.
--
-- 'status', 'assetModelSummary_status' - The current status of the asset model.
newAssetModelSummary ::
  -- | 'id'
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'description'
  Prelude.Text ->
  -- | 'creationDate'
  Prelude.UTCTime ->
  -- | 'lastUpdateDate'
  Prelude.UTCTime ->
  -- | 'status'
  AssetModelStatus ->
  AssetModelSummary
newAssetModelSummary
  pId_
  pArn_
  pName_
  pDescription_
  pCreationDate_
  pLastUpdateDate_
  pStatus_ =
    AssetModelSummary'
      { id = pId_,
        arn = pArn_,
        name = pName_,
        description = pDescription_,
        creationDate = Data._Time Lens.# pCreationDate_,
        lastUpdateDate = Data._Time Lens.# pLastUpdateDate_,
        status = pStatus_
      }

-- | The ID of the asset model (used with IoT SiteWise APIs).
assetModelSummary_id :: Lens.Lens' AssetModelSummary Prelude.Text
assetModelSummary_id = Lens.lens (\AssetModelSummary' {id} -> id) (\s@AssetModelSummary' {} a -> s {id = a} :: AssetModelSummary)

-- | The
-- <https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>
-- of the asset model, which has the following format.
--
-- @arn:${Partition}:iotsitewise:${Region}:${Account}:asset-model\/${AssetModelId}@
assetModelSummary_arn :: Lens.Lens' AssetModelSummary Prelude.Text
assetModelSummary_arn = Lens.lens (\AssetModelSummary' {arn} -> arn) (\s@AssetModelSummary' {} a -> s {arn = a} :: AssetModelSummary)

-- | The name of the asset model.
assetModelSummary_name :: Lens.Lens' AssetModelSummary Prelude.Text
assetModelSummary_name = Lens.lens (\AssetModelSummary' {name} -> name) (\s@AssetModelSummary' {} a -> s {name = a} :: AssetModelSummary)

-- | The asset model description.
assetModelSummary_description :: Lens.Lens' AssetModelSummary Prelude.Text
assetModelSummary_description = Lens.lens (\AssetModelSummary' {description} -> description) (\s@AssetModelSummary' {} a -> s {description = a} :: AssetModelSummary)

-- | The date the asset model was created, in Unix epoch time.
assetModelSummary_creationDate :: Lens.Lens' AssetModelSummary Prelude.UTCTime
assetModelSummary_creationDate = Lens.lens (\AssetModelSummary' {creationDate} -> creationDate) (\s@AssetModelSummary' {} a -> s {creationDate = a} :: AssetModelSummary) Prelude.. Data._Time

-- | The date the asset model was last updated, in Unix epoch time.
assetModelSummary_lastUpdateDate :: Lens.Lens' AssetModelSummary Prelude.UTCTime
assetModelSummary_lastUpdateDate = Lens.lens (\AssetModelSummary' {lastUpdateDate} -> lastUpdateDate) (\s@AssetModelSummary' {} a -> s {lastUpdateDate = a} :: AssetModelSummary) Prelude.. Data._Time

-- | The current status of the asset model.
assetModelSummary_status :: Lens.Lens' AssetModelSummary AssetModelStatus
assetModelSummary_status = Lens.lens (\AssetModelSummary' {status} -> status) (\s@AssetModelSummary' {} a -> s {status = a} :: AssetModelSummary)

instance Data.FromJSON AssetModelSummary where
  parseJSON =
    Data.withObject
      "AssetModelSummary"
      ( \x ->
          AssetModelSummary'
            Prelude.<$> (x Data..: "id")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "description")
            Prelude.<*> (x Data..: "creationDate")
            Prelude.<*> (x Data..: "lastUpdateDate")
            Prelude.<*> (x Data..: "status")
      )

instance Prelude.Hashable AssetModelSummary where
  hashWithSalt _salt AssetModelSummary' {..} =
    _salt `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` lastUpdateDate
      `Prelude.hashWithSalt` status

instance Prelude.NFData AssetModelSummary where
  rnf AssetModelSummary' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf lastUpdateDate
      `Prelude.seq` Prelude.rnf status
