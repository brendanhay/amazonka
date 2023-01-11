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
-- Module      : Amazonka.SageMakerGeoSpatial.Types.ListEarthObservationJobOutputConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMakerGeoSpatial.Types.ListEarthObservationJobOutputConfig where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMakerGeoSpatial.Types.EarthObservationJobStatus

-- | An object containing information about the output file.
--
-- /See:/ 'newListEarthObservationJobOutputConfig' smart constructor.
data ListEarthObservationJobOutputConfig = ListEarthObservationJobOutputConfig'
  { -- | Each tag consists of a key and a value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the list of the Earth Observation
    -- jobs.
    arn :: Prelude.Text,
    -- | The creation time.
    creationTime :: Data.POSIX,
    -- | The duration of the session, in seconds.
    durationInSeconds :: Prelude.Int,
    -- | The names of the Earth Observation jobs in the list.
    name :: Prelude.Text,
    operationType :: Prelude.Text,
    -- | The status of the list of the Earth Observation jobs.
    status :: EarthObservationJobStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEarthObservationJobOutputConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'listEarthObservationJobOutputConfig_tags' - Each tag consists of a key and a value.
--
-- 'arn', 'listEarthObservationJobOutputConfig_arn' - The Amazon Resource Name (ARN) of the list of the Earth Observation
-- jobs.
--
-- 'creationTime', 'listEarthObservationJobOutputConfig_creationTime' - The creation time.
--
-- 'durationInSeconds', 'listEarthObservationJobOutputConfig_durationInSeconds' - The duration of the session, in seconds.
--
-- 'name', 'listEarthObservationJobOutputConfig_name' - The names of the Earth Observation jobs in the list.
--
-- 'operationType', 'listEarthObservationJobOutputConfig_operationType' -
--
-- 'status', 'listEarthObservationJobOutputConfig_status' - The status of the list of the Earth Observation jobs.
newListEarthObservationJobOutputConfig ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'creationTime'
  Prelude.UTCTime ->
  -- | 'durationInSeconds'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  -- | 'operationType'
  Prelude.Text ->
  -- | 'status'
  EarthObservationJobStatus ->
  ListEarthObservationJobOutputConfig
newListEarthObservationJobOutputConfig
  pArn_
  pCreationTime_
  pDurationInSeconds_
  pName_
  pOperationType_
  pStatus_ =
    ListEarthObservationJobOutputConfig'
      { tags =
          Prelude.Nothing,
        arn = pArn_,
        creationTime =
          Data._Time Lens.# pCreationTime_,
        durationInSeconds =
          pDurationInSeconds_,
        name = pName_,
        operationType = pOperationType_,
        status = pStatus_
      }

-- | Each tag consists of a key and a value.
listEarthObservationJobOutputConfig_tags :: Lens.Lens' ListEarthObservationJobOutputConfig (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
listEarthObservationJobOutputConfig_tags = Lens.lens (\ListEarthObservationJobOutputConfig' {tags} -> tags) (\s@ListEarthObservationJobOutputConfig' {} a -> s {tags = a} :: ListEarthObservationJobOutputConfig) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the list of the Earth Observation
-- jobs.
listEarthObservationJobOutputConfig_arn :: Lens.Lens' ListEarthObservationJobOutputConfig Prelude.Text
listEarthObservationJobOutputConfig_arn = Lens.lens (\ListEarthObservationJobOutputConfig' {arn} -> arn) (\s@ListEarthObservationJobOutputConfig' {} a -> s {arn = a} :: ListEarthObservationJobOutputConfig)

-- | The creation time.
listEarthObservationJobOutputConfig_creationTime :: Lens.Lens' ListEarthObservationJobOutputConfig Prelude.UTCTime
listEarthObservationJobOutputConfig_creationTime = Lens.lens (\ListEarthObservationJobOutputConfig' {creationTime} -> creationTime) (\s@ListEarthObservationJobOutputConfig' {} a -> s {creationTime = a} :: ListEarthObservationJobOutputConfig) Prelude.. Data._Time

-- | The duration of the session, in seconds.
listEarthObservationJobOutputConfig_durationInSeconds :: Lens.Lens' ListEarthObservationJobOutputConfig Prelude.Int
listEarthObservationJobOutputConfig_durationInSeconds = Lens.lens (\ListEarthObservationJobOutputConfig' {durationInSeconds} -> durationInSeconds) (\s@ListEarthObservationJobOutputConfig' {} a -> s {durationInSeconds = a} :: ListEarthObservationJobOutputConfig)

-- | The names of the Earth Observation jobs in the list.
listEarthObservationJobOutputConfig_name :: Lens.Lens' ListEarthObservationJobOutputConfig Prelude.Text
listEarthObservationJobOutputConfig_name = Lens.lens (\ListEarthObservationJobOutputConfig' {name} -> name) (\s@ListEarthObservationJobOutputConfig' {} a -> s {name = a} :: ListEarthObservationJobOutputConfig)

-- |
listEarthObservationJobOutputConfig_operationType :: Lens.Lens' ListEarthObservationJobOutputConfig Prelude.Text
listEarthObservationJobOutputConfig_operationType = Lens.lens (\ListEarthObservationJobOutputConfig' {operationType} -> operationType) (\s@ListEarthObservationJobOutputConfig' {} a -> s {operationType = a} :: ListEarthObservationJobOutputConfig)

-- | The status of the list of the Earth Observation jobs.
listEarthObservationJobOutputConfig_status :: Lens.Lens' ListEarthObservationJobOutputConfig EarthObservationJobStatus
listEarthObservationJobOutputConfig_status = Lens.lens (\ListEarthObservationJobOutputConfig' {status} -> status) (\s@ListEarthObservationJobOutputConfig' {} a -> s {status = a} :: ListEarthObservationJobOutputConfig)

instance
  Data.FromJSON
    ListEarthObservationJobOutputConfig
  where
  parseJSON =
    Data.withObject
      "ListEarthObservationJobOutputConfig"
      ( \x ->
          ListEarthObservationJobOutputConfig'
            Prelude.<$> (x Data..:? "Tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "Arn")
            Prelude.<*> (x Data..: "CreationTime")
            Prelude.<*> (x Data..: "DurationInSeconds")
            Prelude.<*> (x Data..: "Name")
            Prelude.<*> (x Data..: "OperationType")
            Prelude.<*> (x Data..: "Status")
      )

instance
  Prelude.Hashable
    ListEarthObservationJobOutputConfig
  where
  hashWithSalt
    _salt
    ListEarthObservationJobOutputConfig' {..} =
      _salt `Prelude.hashWithSalt` tags
        `Prelude.hashWithSalt` arn
        `Prelude.hashWithSalt` creationTime
        `Prelude.hashWithSalt` durationInSeconds
        `Prelude.hashWithSalt` name
        `Prelude.hashWithSalt` operationType
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    ListEarthObservationJobOutputConfig
  where
  rnf ListEarthObservationJobOutputConfig' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf durationInSeconds
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf operationType
      `Prelude.seq` Prelude.rnf status
