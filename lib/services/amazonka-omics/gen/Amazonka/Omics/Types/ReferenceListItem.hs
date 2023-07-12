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
-- Module      : Amazonka.Omics.Types.ReferenceListItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReferenceListItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.ReferenceStatus
import qualified Amazonka.Prelude as Prelude

-- | A genome reference.
--
-- /See:/ 'newReferenceListItem' smart constructor.
data ReferenceListItem = ReferenceListItem'
  { -- | The reference\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The reference\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The reference\'s status.
    status :: Prelude.Maybe ReferenceStatus,
    -- | The reference\'s ARN.
    arn :: Prelude.Text,
    -- | When the reference was created.
    creationTime :: Data.ISO8601,
    -- | The reference\'s ID.
    id :: Prelude.Text,
    -- | The reference\'s MD5 checksum.
    md5 :: Prelude.Text,
    -- | The reference\'s store ID.
    referenceStoreId :: Prelude.Text,
    -- | When the reference was updated.
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferenceListItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'referenceListItem_description' - The reference\'s description.
--
-- 'name', 'referenceListItem_name' - The reference\'s name.
--
-- 'status', 'referenceListItem_status' - The reference\'s status.
--
-- 'arn', 'referenceListItem_arn' - The reference\'s ARN.
--
-- 'creationTime', 'referenceListItem_creationTime' - When the reference was created.
--
-- 'id', 'referenceListItem_id' - The reference\'s ID.
--
-- 'md5', 'referenceListItem_md5' - The reference\'s MD5 checksum.
--
-- 'referenceStoreId', 'referenceListItem_referenceStoreId' - The reference\'s store ID.
--
-- 'updateTime', 'referenceListItem_updateTime' - When the reference was updated.
newReferenceListItem ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'id'
  Prelude.Text ->
  -- | 'md5'
  Prelude.Text ->
  -- | 'referenceStoreId'
  Prelude.Text ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  ReferenceListItem
newReferenceListItem
  pArn_
  pCreationTime_
  pId_
  pMd5_
  pReferenceStoreId_
  pUpdateTime_ =
    ReferenceListItem'
      { description = Prelude.Nothing,
        name = Prelude.Nothing,
        status = Prelude.Nothing,
        arn = pArn_,
        creationTime = Data._Time Lens.# pCreationTime_,
        id = pId_,
        md5 = pMd5_,
        referenceStoreId = pReferenceStoreId_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | The reference\'s description.
referenceListItem_description :: Lens.Lens' ReferenceListItem (Prelude.Maybe Prelude.Text)
referenceListItem_description = Lens.lens (\ReferenceListItem' {description} -> description) (\s@ReferenceListItem' {} a -> s {description = a} :: ReferenceListItem)

-- | The reference\'s name.
referenceListItem_name :: Lens.Lens' ReferenceListItem (Prelude.Maybe Prelude.Text)
referenceListItem_name = Lens.lens (\ReferenceListItem' {name} -> name) (\s@ReferenceListItem' {} a -> s {name = a} :: ReferenceListItem)

-- | The reference\'s status.
referenceListItem_status :: Lens.Lens' ReferenceListItem (Prelude.Maybe ReferenceStatus)
referenceListItem_status = Lens.lens (\ReferenceListItem' {status} -> status) (\s@ReferenceListItem' {} a -> s {status = a} :: ReferenceListItem)

-- | The reference\'s ARN.
referenceListItem_arn :: Lens.Lens' ReferenceListItem Prelude.Text
referenceListItem_arn = Lens.lens (\ReferenceListItem' {arn} -> arn) (\s@ReferenceListItem' {} a -> s {arn = a} :: ReferenceListItem)

-- | When the reference was created.
referenceListItem_creationTime :: Lens.Lens' ReferenceListItem Prelude.UTCTime
referenceListItem_creationTime = Lens.lens (\ReferenceListItem' {creationTime} -> creationTime) (\s@ReferenceListItem' {} a -> s {creationTime = a} :: ReferenceListItem) Prelude.. Data._Time

-- | The reference\'s ID.
referenceListItem_id :: Lens.Lens' ReferenceListItem Prelude.Text
referenceListItem_id = Lens.lens (\ReferenceListItem' {id} -> id) (\s@ReferenceListItem' {} a -> s {id = a} :: ReferenceListItem)

-- | The reference\'s MD5 checksum.
referenceListItem_md5 :: Lens.Lens' ReferenceListItem Prelude.Text
referenceListItem_md5 = Lens.lens (\ReferenceListItem' {md5} -> md5) (\s@ReferenceListItem' {} a -> s {md5 = a} :: ReferenceListItem)

-- | The reference\'s store ID.
referenceListItem_referenceStoreId :: Lens.Lens' ReferenceListItem Prelude.Text
referenceListItem_referenceStoreId = Lens.lens (\ReferenceListItem' {referenceStoreId} -> referenceStoreId) (\s@ReferenceListItem' {} a -> s {referenceStoreId = a} :: ReferenceListItem)

-- | When the reference was updated.
referenceListItem_updateTime :: Lens.Lens' ReferenceListItem Prelude.UTCTime
referenceListItem_updateTime = Lens.lens (\ReferenceListItem' {updateTime} -> updateTime) (\s@ReferenceListItem' {} a -> s {updateTime = a} :: ReferenceListItem) Prelude.. Data._Time

instance Data.FromJSON ReferenceListItem where
  parseJSON =
    Data.withObject
      "ReferenceListItem"
      ( \x ->
          ReferenceListItem'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "md5")
            Prelude.<*> (x Data..: "referenceStoreId")
            Prelude.<*> (x Data..: "updateTime")
      )

instance Prelude.Hashable ReferenceListItem where
  hashWithSalt _salt ReferenceListItem' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` md5
      `Prelude.hashWithSalt` referenceStoreId
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData ReferenceListItem where
  rnf ReferenceListItem' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf md5
      `Prelude.seq` Prelude.rnf referenceStoreId
      `Prelude.seq` Prelude.rnf updateTime
