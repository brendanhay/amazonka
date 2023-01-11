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
-- Module      : Amazonka.Omics.Types.SequenceStoreDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.SequenceStoreDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.SseConfig
import qualified Amazonka.Prelude as Prelude

-- | Details about a sequence store.
--
-- /See:/ 'newSequenceStoreDetail' smart constructor.
data SequenceStoreDetail = SequenceStoreDetail'
  { -- | The store\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The store\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The store\'s server-side encryption (SSE) settings.
    sseConfig :: Prelude.Maybe SseConfig,
    -- | The store\'s ARN.
    arn :: Prelude.Text,
    -- | When the store was created.
    creationTime :: Data.ISO8601,
    -- | The store\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SequenceStoreDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'sequenceStoreDetail_description' - The store\'s description.
--
-- 'name', 'sequenceStoreDetail_name' - The store\'s name.
--
-- 'sseConfig', 'sequenceStoreDetail_sseConfig' - The store\'s server-side encryption (SSE) settings.
--
-- 'arn', 'sequenceStoreDetail_arn' - The store\'s ARN.
--
-- 'creationTime', 'sequenceStoreDetail_creationTime' - When the store was created.
--
-- 'id', 'sequenceStoreDetail_id' - The store\'s ID.
newSequenceStoreDetail ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'id'
  Prelude.Text ->
  SequenceStoreDetail
newSequenceStoreDetail pArn_ pCreationTime_ pId_ =
  SequenceStoreDetail'
    { description = Prelude.Nothing,
      name = Prelude.Nothing,
      sseConfig = Prelude.Nothing,
      arn = pArn_,
      creationTime = Data._Time Lens.# pCreationTime_,
      id = pId_
    }

-- | The store\'s description.
sequenceStoreDetail_description :: Lens.Lens' SequenceStoreDetail (Prelude.Maybe Prelude.Text)
sequenceStoreDetail_description = Lens.lens (\SequenceStoreDetail' {description} -> description) (\s@SequenceStoreDetail' {} a -> s {description = a} :: SequenceStoreDetail)

-- | The store\'s name.
sequenceStoreDetail_name :: Lens.Lens' SequenceStoreDetail (Prelude.Maybe Prelude.Text)
sequenceStoreDetail_name = Lens.lens (\SequenceStoreDetail' {name} -> name) (\s@SequenceStoreDetail' {} a -> s {name = a} :: SequenceStoreDetail)

-- | The store\'s server-side encryption (SSE) settings.
sequenceStoreDetail_sseConfig :: Lens.Lens' SequenceStoreDetail (Prelude.Maybe SseConfig)
sequenceStoreDetail_sseConfig = Lens.lens (\SequenceStoreDetail' {sseConfig} -> sseConfig) (\s@SequenceStoreDetail' {} a -> s {sseConfig = a} :: SequenceStoreDetail)

-- | The store\'s ARN.
sequenceStoreDetail_arn :: Lens.Lens' SequenceStoreDetail Prelude.Text
sequenceStoreDetail_arn = Lens.lens (\SequenceStoreDetail' {arn} -> arn) (\s@SequenceStoreDetail' {} a -> s {arn = a} :: SequenceStoreDetail)

-- | When the store was created.
sequenceStoreDetail_creationTime :: Lens.Lens' SequenceStoreDetail Prelude.UTCTime
sequenceStoreDetail_creationTime = Lens.lens (\SequenceStoreDetail' {creationTime} -> creationTime) (\s@SequenceStoreDetail' {} a -> s {creationTime = a} :: SequenceStoreDetail) Prelude.. Data._Time

-- | The store\'s ID.
sequenceStoreDetail_id :: Lens.Lens' SequenceStoreDetail Prelude.Text
sequenceStoreDetail_id = Lens.lens (\SequenceStoreDetail' {id} -> id) (\s@SequenceStoreDetail' {} a -> s {id = a} :: SequenceStoreDetail)

instance Data.FromJSON SequenceStoreDetail where
  parseJSON =
    Data.withObject
      "SequenceStoreDetail"
      ( \x ->
          SequenceStoreDetail'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "sseConfig")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "id")
      )

instance Prelude.Hashable SequenceStoreDetail where
  hashWithSalt _salt SequenceStoreDetail' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sseConfig
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` id

instance Prelude.NFData SequenceStoreDetail where
  rnf SequenceStoreDetail' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sseConfig
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf id
