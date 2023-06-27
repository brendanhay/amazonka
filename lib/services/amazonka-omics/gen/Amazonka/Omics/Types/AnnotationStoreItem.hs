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
-- Module      : Amazonka.Omics.Types.AnnotationStoreItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.AnnotationStoreItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.ReferenceItem
import Amazonka.Omics.Types.SseConfig
import Amazonka.Omics.Types.StoreFormat
import Amazonka.Omics.Types.StoreStatus
import qualified Amazonka.Prelude as Prelude

-- | An annotation store.
--
-- /See:/ 'newAnnotationStoreItem' smart constructor.
data AnnotationStoreItem = AnnotationStoreItem'
  { -- | The store\'s ID.
    id :: Prelude.Text,
    -- | The store\'s genome reference.
    reference :: ReferenceItem,
    -- | The store\'s status.
    status :: StoreStatus,
    -- | The store\'s ARN.
    storeArn :: Prelude.Text,
    -- | The store\'s name.
    name :: Prelude.Text,
    -- | The store\'s file format.
    storeFormat :: StoreFormat,
    -- | The store\'s description.
    description :: Prelude.Text,
    -- | The store\'s server-side encryption (SSE) settings.
    sseConfig :: SseConfig,
    -- | The store\'s creation time.
    creationTime :: Data.ISO8601,
    -- | When the store was updated.
    updateTime :: Data.ISO8601,
    -- | The store\'s status message.
    statusMessage :: Prelude.Text,
    -- | The store\'s size in bytes.
    storeSizeBytes :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AnnotationStoreItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'annotationStoreItem_id' - The store\'s ID.
--
-- 'reference', 'annotationStoreItem_reference' - The store\'s genome reference.
--
-- 'status', 'annotationStoreItem_status' - The store\'s status.
--
-- 'storeArn', 'annotationStoreItem_storeArn' - The store\'s ARN.
--
-- 'name', 'annotationStoreItem_name' - The store\'s name.
--
-- 'storeFormat', 'annotationStoreItem_storeFormat' - The store\'s file format.
--
-- 'description', 'annotationStoreItem_description' - The store\'s description.
--
-- 'sseConfig', 'annotationStoreItem_sseConfig' - The store\'s server-side encryption (SSE) settings.
--
-- 'creationTime', 'annotationStoreItem_creationTime' - The store\'s creation time.
--
-- 'updateTime', 'annotationStoreItem_updateTime' - When the store was updated.
--
-- 'statusMessage', 'annotationStoreItem_statusMessage' - The store\'s status message.
--
-- 'storeSizeBytes', 'annotationStoreItem_storeSizeBytes' - The store\'s size in bytes.
newAnnotationStoreItem ::
  -- | 'id'
  Prelude.Text ->
  -- | 'reference'
  ReferenceItem ->
  -- | 'status'
  StoreStatus ->
  -- | 'storeArn'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'storeFormat'
  StoreFormat ->
  -- | 'description'
  Prelude.Text ->
  -- | 'sseConfig'
  SseConfig ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'updateTime'
  Prelude.UTCTime ->
  -- | 'statusMessage'
  Prelude.Text ->
  -- | 'storeSizeBytes'
  Prelude.Integer ->
  AnnotationStoreItem
newAnnotationStoreItem
  pId_
  pReference_
  pStatus_
  pStoreArn_
  pName_
  pStoreFormat_
  pDescription_
  pSseConfig_
  pCreationTime_
  pUpdateTime_
  pStatusMessage_
  pStoreSizeBytes_ =
    AnnotationStoreItem'
      { id = pId_,
        reference = pReference_,
        status = pStatus_,
        storeArn = pStoreArn_,
        name = pName_,
        storeFormat = pStoreFormat_,
        description = pDescription_,
        sseConfig = pSseConfig_,
        creationTime = Data._Time Lens.# pCreationTime_,
        updateTime = Data._Time Lens.# pUpdateTime_,
        statusMessage = pStatusMessage_,
        storeSizeBytes = pStoreSizeBytes_
      }

-- | The store\'s ID.
annotationStoreItem_id :: Lens.Lens' AnnotationStoreItem Prelude.Text
annotationStoreItem_id = Lens.lens (\AnnotationStoreItem' {id} -> id) (\s@AnnotationStoreItem' {} a -> s {id = a} :: AnnotationStoreItem)

-- | The store\'s genome reference.
annotationStoreItem_reference :: Lens.Lens' AnnotationStoreItem ReferenceItem
annotationStoreItem_reference = Lens.lens (\AnnotationStoreItem' {reference} -> reference) (\s@AnnotationStoreItem' {} a -> s {reference = a} :: AnnotationStoreItem)

-- | The store\'s status.
annotationStoreItem_status :: Lens.Lens' AnnotationStoreItem StoreStatus
annotationStoreItem_status = Lens.lens (\AnnotationStoreItem' {status} -> status) (\s@AnnotationStoreItem' {} a -> s {status = a} :: AnnotationStoreItem)

-- | The store\'s ARN.
annotationStoreItem_storeArn :: Lens.Lens' AnnotationStoreItem Prelude.Text
annotationStoreItem_storeArn = Lens.lens (\AnnotationStoreItem' {storeArn} -> storeArn) (\s@AnnotationStoreItem' {} a -> s {storeArn = a} :: AnnotationStoreItem)

-- | The store\'s name.
annotationStoreItem_name :: Lens.Lens' AnnotationStoreItem Prelude.Text
annotationStoreItem_name = Lens.lens (\AnnotationStoreItem' {name} -> name) (\s@AnnotationStoreItem' {} a -> s {name = a} :: AnnotationStoreItem)

-- | The store\'s file format.
annotationStoreItem_storeFormat :: Lens.Lens' AnnotationStoreItem StoreFormat
annotationStoreItem_storeFormat = Lens.lens (\AnnotationStoreItem' {storeFormat} -> storeFormat) (\s@AnnotationStoreItem' {} a -> s {storeFormat = a} :: AnnotationStoreItem)

-- | The store\'s description.
annotationStoreItem_description :: Lens.Lens' AnnotationStoreItem Prelude.Text
annotationStoreItem_description = Lens.lens (\AnnotationStoreItem' {description} -> description) (\s@AnnotationStoreItem' {} a -> s {description = a} :: AnnotationStoreItem)

-- | The store\'s server-side encryption (SSE) settings.
annotationStoreItem_sseConfig :: Lens.Lens' AnnotationStoreItem SseConfig
annotationStoreItem_sseConfig = Lens.lens (\AnnotationStoreItem' {sseConfig} -> sseConfig) (\s@AnnotationStoreItem' {} a -> s {sseConfig = a} :: AnnotationStoreItem)

-- | The store\'s creation time.
annotationStoreItem_creationTime :: Lens.Lens' AnnotationStoreItem Prelude.UTCTime
annotationStoreItem_creationTime = Lens.lens (\AnnotationStoreItem' {creationTime} -> creationTime) (\s@AnnotationStoreItem' {} a -> s {creationTime = a} :: AnnotationStoreItem) Prelude.. Data._Time

-- | When the store was updated.
annotationStoreItem_updateTime :: Lens.Lens' AnnotationStoreItem Prelude.UTCTime
annotationStoreItem_updateTime = Lens.lens (\AnnotationStoreItem' {updateTime} -> updateTime) (\s@AnnotationStoreItem' {} a -> s {updateTime = a} :: AnnotationStoreItem) Prelude.. Data._Time

-- | The store\'s status message.
annotationStoreItem_statusMessage :: Lens.Lens' AnnotationStoreItem Prelude.Text
annotationStoreItem_statusMessage = Lens.lens (\AnnotationStoreItem' {statusMessage} -> statusMessage) (\s@AnnotationStoreItem' {} a -> s {statusMessage = a} :: AnnotationStoreItem)

-- | The store\'s size in bytes.
annotationStoreItem_storeSizeBytes :: Lens.Lens' AnnotationStoreItem Prelude.Integer
annotationStoreItem_storeSizeBytes = Lens.lens (\AnnotationStoreItem' {storeSizeBytes} -> storeSizeBytes) (\s@AnnotationStoreItem' {} a -> s {storeSizeBytes = a} :: AnnotationStoreItem)

instance Data.FromJSON AnnotationStoreItem where
  parseJSON =
    Data.withObject
      "AnnotationStoreItem"
      ( \x ->
          AnnotationStoreItem'
            Prelude.<$> (x Data..: "id")
            Prelude.<*> (x Data..: "reference")
            Prelude.<*> (x Data..: "status")
            Prelude.<*> (x Data..: "storeArn")
            Prelude.<*> (x Data..: "name")
            Prelude.<*> (x Data..: "storeFormat")
            Prelude.<*> (x Data..: "description")
            Prelude.<*> (x Data..: "sseConfig")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "updateTime")
            Prelude.<*> (x Data..: "statusMessage")
            Prelude.<*> (x Data..: "storeSizeBytes")
      )

instance Prelude.Hashable AnnotationStoreItem where
  hashWithSalt _salt AnnotationStoreItem' {..} =
    _salt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` reference
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` storeArn
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` storeFormat
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` sseConfig
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` updateTime
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` storeSizeBytes

instance Prelude.NFData AnnotationStoreItem where
  rnf AnnotationStoreItem' {..} =
    Prelude.rnf id
      `Prelude.seq` Prelude.rnf reference
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf storeArn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf storeFormat
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf sseConfig
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf updateTime
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf storeSizeBytes
