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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.ListVectorEnrichmentJobOutputConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.ListVectorEnrichmentJobOutputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobStatus
import Amazonka.SageMakerGeoSpatial.Types.VectorEnrichmentJobType

-- | An object containing information about the output file.
--
-- /See:/ 'newListVectorEnrichmentJobOutputConfig' smart constructor.
data ListVectorEnrichmentJobOutputConfig = ListVectorEnrichmentJobOutputConfig'
  { -- | Each tag consists of a key and a value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the list of the Vector Enrichment
    -- jobs.
    arn :: Prelude.Text,
    -- | The creation time.
    creationTime :: Data.POSIX,
    -- | The duration of the session, in seconds.
    durationInSeconds :: Prelude.Int,
    -- | The names of the Vector Enrichment jobs in the list.
    name :: Prelude.Text,
    -- | The status of the Vector Enrichment jobs list.
    status :: VectorEnrichmentJobStatus,
    -- | The type of the list of Vector Enrichment jobs.
    type' :: VectorEnrichmentJobType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVectorEnrichmentJobOutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'listVectorEnrichmentJobOutputConfig_tags' - Each tag consists of a key and a value.
--
-- 'arn', 'listVectorEnrichmentJobOutputConfig_arn' - The Amazon Resource Name (ARN) of the list of the Vector Enrichment
-- jobs.
--
-- 'creationTime', 'listVectorEnrichmentJobOutputConfig_creationTime' - The creation time.
--
-- 'durationInSeconds', 'listVectorEnrichmentJobOutputConfig_durationInSeconds' - The duration of the session, in seconds.
--
-- 'name', 'listVectorEnrichmentJobOutputConfig_name' - The names of the Vector Enrichment jobs in the list.
--
-- 'status', 'listVectorEnrichmentJobOutputConfig_status' - The status of the Vector Enrichment jobs list.
--
-- 'type'', 'listVectorEnrichmentJobOutputConfig_type' - The type of the list of Vector Enrichment jobs.
newListVectorEnrichmentJobOutputConfig ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'durationInSeconds'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'status'
  VectorEnrichmentJobStatus ->
  -- | 'type''
  VectorEnrichmentJobType ->
  ListVectorEnrichmentJobOutputConfig
newListVectorEnrichmentJobOutputConfig
  pArn_
  pCreationTime_
  pDurationInSeconds_
  pName_
  pStatus_
  pType_ =
    ListVectorEnrichmentJobOutputConfig'
      { tags =
          Prelude.Nothing,
        arn = pArn_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        durationInSeconds =
          pDurationInSeconds_,
        name = pName_,
        status = pStatus_,
        type' = pType_
      }

-- | Each tag consists of a key and a value.
listVectorEnrichmentJobOutputConfig_tags :: Lens.Lens' ListVectorEnrichmentJobOutputConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listVectorEnrichmentJobOutputConfig_tags = Lens.lens (\ListVectorEnrichmentJobOutputConfig' {tags} -> tags) (\s@ListVectorEnrichmentJobOutputConfig' {} a -> s {tags = a} :: ListVectorEnrichmentJobOutputConfig) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the list of the Vector Enrichment
-- jobs.
listVectorEnrichmentJobOutputConfig_arn :: Lens.Lens' ListVectorEnrichmentJobOutputConfig Prelude.Text
listVectorEnrichmentJobOutputConfig_arn = Lens.lens (\ListVectorEnrichmentJobOutputConfig' {arn} -> arn) (\s@ListVectorEnrichmentJobOutputConfig' {} a -> s {arn = a} :: ListVectorEnrichmentJobOutputConfig)

-- | The creation time.
listVectorEnrichmentJobOutputConfig_creationTime :: Lens.Lens' ListVectorEnrichmentJobOutputConfig Prelude.UTCTime
listVectorEnrichmentJobOutputConfig_creationTime = Lens.lens (\ListVectorEnrichmentJobOutputConfig' {creationTime} -> creationTime) (\s@ListVectorEnrichmentJobOutputConfig' {} a -> s {creationTime = a} :: ListVectorEnrichmentJobOutputConfig) Prelude.. Data._Time

-- | The duration of the session, in seconds.
listVectorEnrichmentJobOutputConfig_durationInSeconds :: Lens.Lens' ListVectorEnrichmentJobOutputConfig Prelude.Int
listVectorEnrichmentJobOutputConfig_durationInSeconds = Lens.lens (\ListVectorEnrichmentJobOutputConfig' {durationInSeconds} -> durationInSeconds) (\s@ListVectorEnrichmentJobOutputConfig' {} a -> s {durationInSeconds = a} :: ListVectorEnrichmentJobOutputConfig)

-- | The names of the Vector Enrichment jobs in the list.
listVectorEnrichmentJobOutputConfig_name :: Lens.Lens' ListVectorEnrichmentJobOutputConfig Prelude.Text
listVectorEnrichmentJobOutputConfig_name = Lens.lens (\ListVectorEnrichmentJobOutputConfig' {name} -> name) (\s@ListVectorEnrichmentJobOutputConfig' {} a -> s {name = a} :: ListVectorEnrichmentJobOutputConfig)

-- | The status of the Vector Enrichment jobs list.
listVectorEnrichmentJobOutputConfig_status :: Lens.Lens' ListVectorEnrichmentJobOutputConfig VectorEnrichmentJobStatus
listVectorEnrichmentJobOutputConfig_status = Lens.lens (\ListVectorEnrichmentJobOutputConfig' {status} -> status) (\s@ListVectorEnrichmentJobOutputConfig' {} a -> s {status = a} :: ListVectorEnrichmentJobOutputConfig)

-- | The type of the list of Vector Enrichment jobs.
listVectorEnrichmentJobOutputConfig_type :: Lens.Lens' ListVectorEnrichmentJobOutputConfig VectorEnrichmentJobType
listVectorEnrichmentJobOutputConfig_type = Lens.lens (\ListVectorEnrichmentJobOutputConfig' {type'} -> type') (\s@ListVectorEnrichmentJobOutputConfig' {} a -> s {type' = a} :: ListVectorEnrichmentJobOutputConfig)

instance
  Data.FromJSON
    ListVectorEnrichmentJobOutputConfig
  where
  parseJSON =
    Data.withObject
      "ListVectorEnrichmentJobOutputConfig"
      ( \x ->
          ListVectorEnrichmentJobOutputConfig'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "DurationInSeconds")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "Status")
            Prelude.<*> (x Data..: "Type")
      )

instance
  Prelude.Hashable
    ListVectorEnrichmentJobOutputConfig
  where
  hashWithSalt
    _salt
    ListVectorEnrichmentJobOutputConfig' {..} =
      _salt
        `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` durationInSeconds
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` type'

instance
  Prelude.NFData
    ListVectorEnrichmentJobOutputConfig
  where
  rnf ListVectorEnrichmentJobOutputConfig' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf durationInSeconds
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf type'
