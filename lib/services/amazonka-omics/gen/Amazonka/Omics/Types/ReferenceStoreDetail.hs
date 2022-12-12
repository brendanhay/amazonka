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
-- Module      : Amazonka.Omics.Types.ReferenceStoreDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Omics.Types.ReferenceStoreDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Omics.Types.SseConfig
import qualified Amazonka.Prelude as Prelude

-- | Details about a reference store.
--
-- /See:/ 'newReferenceStoreDetail' smart constructor.
data ReferenceStoreDetail = ReferenceStoreDetail'
  { -- | The store\'s description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The store\'s name.
    name :: Prelude.Maybe Prelude.Text,
    -- | The store\'s server-side encryption (SSE) settings.
    sseConfig :: Prelude.Maybe SseConfig,
    -- | The store\'s ARN.
    arn :: Prelude.Text,
    -- | When the store was created.
    creationTime :: Data.POSIX,
    -- | The store\'s ID.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReferenceStoreDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'referenceStoreDetail_description' - The store\'s description.
--
-- 'name', 'referenceStoreDetail_name' - The store\'s name.
--
-- 'sseConfig', 'referenceStoreDetail_sseConfig' - The store\'s server-side encryption (SSE) settings.
--
-- 'arn', 'referenceStoreDetail_arn' - The store\'s ARN.
--
-- 'creationTime', 'referenceStoreDetail_creationTime' - When the store was created.
--
-- 'id', 'referenceStoreDetail_id' - The store\'s ID.
newReferenceStoreDetail ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'id'
  Prelude.Text ->
  ReferenceStoreDetail
newReferenceStoreDetail pArn_ pCreationTime_ pId_ =
  ReferenceStoreDetail'
    { description =
        Prelude.Nothing,
      name = Prelude.Nothing,
      sseConfig = Prelude.Nothing,
      arn = pArn_,
      creationTime = Data._Time Lens.# pCreationTime_,
      id = pId_
    }

-- | The store\'s description.
referenceStoreDetail_description :: Lens.Lens' ReferenceStoreDetail (Prelude.Maybe Prelude.Text)
referenceStoreDetail_description = Lens.lens (\ReferenceStoreDetail' {description} -> description) (\s@ReferenceStoreDetail' {} a -> s {description = a} :: ReferenceStoreDetail)

-- | The store\'s name.
referenceStoreDetail_name :: Lens.Lens' ReferenceStoreDetail (Prelude.Maybe Prelude.Text)
referenceStoreDetail_name = Lens.lens (\ReferenceStoreDetail' {name} -> name) (\s@ReferenceStoreDetail' {} a -> s {name = a} :: ReferenceStoreDetail)

-- | The store\'s server-side encryption (SSE) settings.
referenceStoreDetail_sseConfig :: Lens.Lens' ReferenceStoreDetail (Prelude.Maybe SseConfig)
referenceStoreDetail_sseConfig = Lens.lens (\ReferenceStoreDetail' {sseConfig} -> sseConfig) (\s@ReferenceStoreDetail' {} a -> s {sseConfig = a} :: ReferenceStoreDetail)

-- | The store\'s ARN.
referenceStoreDetail_arn :: Lens.Lens' ReferenceStoreDetail Prelude.Text
referenceStoreDetail_arn = Lens.lens (\ReferenceStoreDetail' {arn} -> arn) (\s@ReferenceStoreDetail' {} a -> s {arn = a} :: ReferenceStoreDetail)

-- | When the store was created.
referenceStoreDetail_creationTime :: Lens.Lens' ReferenceStoreDetail Prelude.UTCTime
referenceStoreDetail_creationTime = Lens.lens (\ReferenceStoreDetail' {creationTime} -> creationTime) (\s@ReferenceStoreDetail' {} a -> s {creationTime = a} :: ReferenceStoreDetail) Prelude.. Data._Time

-- | The store\'s ID.
referenceStoreDetail_id :: Lens.Lens' ReferenceStoreDetail Prelude.Text
referenceStoreDetail_id = Lens.lens (\ReferenceStoreDetail' {id} -> id) (\s@ReferenceStoreDetail' {} a -> s {id = a} :: ReferenceStoreDetail)

instance Data.FromJSON ReferenceStoreDetail where
  parseJSON =
    Data.withObject
      "ReferenceStoreDetail"
      ( \x ->
          ReferenceStoreDetail'
            Prelude.<$> (x Data..:? "description")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "sseConfig")
            Prelude.<*> (x Data..: "arn")
            Prelude.<*> (x Data..: "creationTime")
            Prelude.<*> (x Data..: "id")
      )

instance Prelude.Hashable ReferenceStoreDetail where
  hashWithSalt _salt ReferenceStoreDetail' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` sseConfig
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` id

instance Prelude.NFData ReferenceStoreDetail where
  rnf ReferenceStoreDetail' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf sseConfig
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf id
