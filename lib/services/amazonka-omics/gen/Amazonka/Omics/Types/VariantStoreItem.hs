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
-- Module      : Amazonka.Omics.Types.VariantStoreItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.VariantStoreItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.ReferenceItem
import Amazonka.Omics.Types.SseConfig
import Amazonka.Omics.Types.StoreStatus
import qualified Amazonka.Prelude as Prelude

-- | A variant store.
--
-- /See:/ 'newVariantStoreItem' smart constructor.
data VariantStoreItem = VariantStoreItem'
  { -- | When the store was created.
    creationTime :: Data.ISO8601,
    -- | The store\'s description.
    description :: Prelude.Text,
    -- | The store\'s ID.
    id :: Prelude.Text,
    -- | The store\'s name.
    name :: Prelude.Text,
    -- | The store\'s genome reference.
    reference :: ReferenceItem,
    -- | The store\'s server-side encryption (SSE) settings.
    sseConfig :: SseConfig,
    -- | The store\'s status.
    status :: StoreStatus,
    -- | The store\'s status message.
    statusMessage :: Prelude.Text,
    -- | The store\'s ARN.
    storeArn :: Prelude.Text,
    -- | The store\'s size in bytes.
    storeSizeBytes :: Prelude.Integer,
    -- | When the store was updated.
    updateTime :: Data.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VariantStoreItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'variantStoreItem_creationTime' - When the store was created.
--
-- 'description', 'variantStoreItem_description' - The store\'s description.
--
-- 'id', 'variantStoreItem_id' - The store\'s ID.
--
-- 'name', 'variantStoreItem_name' - The store\'s name.
--
-- 'reference', 'variantStoreItem_reference' - The store\'s genome reference.
--
-- 'sseConfig', 'variantStoreItem_sseConfig' - The store\'s server-side encryption (SSE) settings.
--
-- 'status', 'variantStoreItem_status' - The store\'s status.
--
-- 'statusMessage', 'variantStoreItem_statusMessage' - The store\'s status message.
--
-- 'storeArn', 'variantStoreItem_storeArn' - The store\'s ARN.
--
-- 'storeSizeBytes', 'variantStoreItem_storeSizeBytes' - The store\'s size in bytes.
--
-- 'updateTime', 'variantStoreItem_updateTime' - When the store was updated.
newVariantStoreItem ::
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'description'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'reference'
  ReferenceItem ->
  -- | 'sseConfig'
  SseConfig ->
  -- | 'status'
  StoreStatus ->
  -- | 'statusMessage'
  Prelude.Text ->
  -- | 'storeArn'
  Prelude.Text ->
  -- | 'storeSizeBytes'
  Prelude.Integer ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  VariantStoreItem
newVariantStoreItem
  pCreationTime_
  pDescription_
  pId_
  pName_
  pReference_
  pSseConfig_
  pStatus_
  pStatusMessage_
  pStoreArn_
  pStoreSizeBytes_
  pUpdateTime_ =
    VariantStoreItem'
      { creationTime =
          Data._Time Lens.# pCreationTime_,
        description = pDescription_,
        id = pId_,
        name = pName_,
        reference = pReference_,
        sseConfig = pSseConfig_,
        status = pStatus_,
        statusMessage = pStatusMessage_,
        storeArn = pStoreArn_,
        storeSizeBytes = pStoreSizeBytes_,
        updateTime = Data._Time Lens.# pUpdateTime_
      }

-- | When the store was created.
variantStoreItem_creationTime :: Lens.Lens' VariantStoreItem Prelude.UTCTime
variantStoreItem_creationTime = Lens.lens (\VariantStoreItem' {creationTime} -> creationTime) (\s@VariantStoreItem' {} a -> s {creationTime = a} :: VariantStoreItem) Prelude.. Data._Time

-- | The store\'s description.
variantStoreItem_description :: Lens.Lens' VariantStoreItem Prelude.Text
variantStoreItem_description = Lens.lens (\VariantStoreItem' {description} -> description) (\s@VariantStoreItem' {} a -> s {description = a} :: VariantStoreItem)

-- | The store\'s ID.
variantStoreItem_id :: Lens.Lens' VariantStoreItem Prelude.Text
variantStoreItem_id = Lens.lens (\VariantStoreItem' {id} -> id) (\s@VariantStoreItem' {} a -> s {id = a} :: VariantStoreItem)

-- | The store\'s name.
variantStoreItem_name :: Lens.Lens' VariantStoreItem Prelude.Text
variantStoreItem_name = Lens.lens (\VariantStoreItem' {name} -> name) (\s@VariantStoreItem' {} a -> s {name = a} :: VariantStoreItem)

-- | The store\'s genome reference.
variantStoreItem_reference :: Lens.Lens' VariantStoreItem ReferenceItem
variantStoreItem_reference = Lens.lens (\VariantStoreItem' {reference} -> reference) (\s@VariantStoreItem' {} a -> s {reference = a} :: VariantStoreItem)

-- | The store\'s server-side encryption (SSE) settings.
variantStoreItem_sseConfig :: Lens.Lens' VariantStoreItem SseConfig
variantStoreItem_sseConfig = Lens.lens (\VariantStoreItem' {sseConfig} -> sseConfig) (\s@VariantStoreItem' {} a -> s {sseConfig = a} :: VariantStoreItem)

-- | The store\'s status.
variantStoreItem_status :: Lens.Lens' VariantStoreItem StoreStatus
variantStoreItem_status = Lens.lens (\VariantStoreItem' {status} -> status) (\s@VariantStoreItem' {} a -> s {status = a} :: VariantStoreItem)

-- | The store\'s status message.
variantStoreItem_statusMessage :: Lens.Lens' VariantStoreItem Prelude.Text
variantStoreItem_statusMessage = Lens.lens (\VariantStoreItem' {statusMessage} -> statusMessage) (\s@VariantStoreItem' {} a -> s {statusMessage = a} :: VariantStoreItem)

-- | The store\'s ARN.
variantStoreItem_storeArn :: Lens.Lens' VariantStoreItem Prelude.Text
variantStoreItem_storeArn = Lens.lens (\VariantStoreItem' {storeArn} -> storeArn) (\s@VariantStoreItem' {} a -> s {storeArn = a} :: VariantStoreItem)

-- | The store\'s size in bytes.
variantStoreItem_storeSizeBytes :: Lens.Lens' VariantStoreItem Prelude.Integer
variantStoreItem_storeSizeBytes = Lens.lens (\VariantStoreItem' {storeSizeBytes} -> storeSizeBytes) (\s@VariantStoreItem' {} a -> s {storeSizeBytes = a} :: VariantStoreItem)

-- | When the store was updated.
variantStoreItem_updateTime :: Lens.Lens' VariantStoreItem Prelude.UTCTime
variantStoreItem_updateTime = Lens.lens (\VariantStoreItem' {updateTime} -> updateTime) (\s@VariantStoreItem' {} a -> s {updateTime = a} :: VariantStoreItem) Prelude.. Data._Time

instance Data.FromJSON VariantStoreItem where
  parseJSON =
    Data.withObject
      "VariantStoreItem"
      ( \x ->
          VariantStoreItem'
            Prelude.<$> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "description")
            Prelude.<*> (x Data..: "id")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "reference")
            Prelude.<*> (x Data..: "sseConfig")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "statusMessage")
            Prelude.<*> (x Data..: "storeArn")
            Prelude.<*> (x Data..: "storeSizeBytes")
            Prelude.<*> (x Data..: "updateTime")
      )

instance Prelude.Hashable VariantStoreItem where
  hashWithSalt _salt VariantStoreItem' {..} =
    _salt
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` reference
      `Prelude.hashWithSalt` sseConfig
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` storeArn
      `Prelude.hashWithSalt` storeSizeBytes
      `Prelude.hashWithSalt` updateTime

instance Prelude.NFData VariantStoreItem where
  rnf VariantStoreItem' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf reference
      `Prelude.seq` Prelude.rnf sseConfig
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf storeArn
      `Prelude.seq` Prelude.rnf storeSizeBytes
      `Prelude.seq` Prelude.rnf updateTime
