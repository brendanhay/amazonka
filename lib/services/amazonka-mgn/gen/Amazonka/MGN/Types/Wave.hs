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
-- Module      : Amazonka.MGN.Types.Wave
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.Wave where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.WaveAggregatedStatus
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newWave' smart constructor.
data Wave = Wave'
  { -- | Wave ARN.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Wave creation dateTime.
    creationDateTime :: Prelude.Maybe Prelude.Text,
    -- | Wave description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Wave archival status.
    isArchived :: Prelude.Maybe Prelude.Bool,
    -- | Wave last modified dateTime.
    lastModifiedDateTime :: Prelude.Maybe Prelude.Text,
    -- | Wave name.
    name :: Prelude.Maybe Prelude.Text,
    -- | Wave tags.
    tags :: Prelude.Maybe (Data.Sensitive (Prelude.HashMap Prelude.Text Prelude.Text)),
    -- | Wave aggregated status.
    waveAggregatedStatus :: Prelude.Maybe WaveAggregatedStatus,
    -- | Wave ID.
    waveID :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Wave' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'wave_arn' - Wave ARN.
--
-- 'creationDateTime', 'wave_creationDateTime' - Wave creation dateTime.
--
-- 'description', 'wave_description' - Wave description.
--
-- 'isArchived', 'wave_isArchived' - Wave archival status.
--
-- 'lastModifiedDateTime', 'wave_lastModifiedDateTime' - Wave last modified dateTime.
--
-- 'name', 'wave_name' - Wave name.
--
-- 'tags', 'wave_tags' - Wave tags.
--
-- 'waveAggregatedStatus', 'wave_waveAggregatedStatus' - Wave aggregated status.
--
-- 'waveID', 'wave_waveID' - Wave ID.
newWave ::
  Wave
newWave =
  Wave'
    { arn = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      description = Prelude.Nothing,
      isArchived = Prelude.Nothing,
      lastModifiedDateTime = Prelude.Nothing,
      name = Prelude.Nothing,
      tags = Prelude.Nothing,
      waveAggregatedStatus = Prelude.Nothing,
      waveID = Prelude.Nothing
    }

-- | Wave ARN.
wave_arn :: Lens.Lens' Wave (Prelude.Maybe Prelude.Text)
wave_arn = Lens.lens (\Wave' {arn} -> arn) (\s@Wave' {} a -> s {arn = a} :: Wave)

-- | Wave creation dateTime.
wave_creationDateTime :: Lens.Lens' Wave (Prelude.Maybe Prelude.Text)
wave_creationDateTime = Lens.lens (\Wave' {creationDateTime} -> creationDateTime) (\s@Wave' {} a -> s {creationDateTime = a} :: Wave)

-- | Wave description.
wave_description :: Lens.Lens' Wave (Prelude.Maybe Prelude.Text)
wave_description = Lens.lens (\Wave' {description} -> description) (\s@Wave' {} a -> s {description = a} :: Wave)

-- | Wave archival status.
wave_isArchived :: Lens.Lens' Wave (Prelude.Maybe Prelude.Bool)
wave_isArchived = Lens.lens (\Wave' {isArchived} -> isArchived) (\s@Wave' {} a -> s {isArchived = a} :: Wave)

-- | Wave last modified dateTime.
wave_lastModifiedDateTime :: Lens.Lens' Wave (Prelude.Maybe Prelude.Text)
wave_lastModifiedDateTime = Lens.lens (\Wave' {lastModifiedDateTime} -> lastModifiedDateTime) (\s@Wave' {} a -> s {lastModifiedDateTime = a} :: Wave)

-- | Wave name.
wave_name :: Lens.Lens' Wave (Prelude.Maybe Prelude.Text)
wave_name = Lens.lens (\Wave' {name} -> name) (\s@Wave' {} a -> s {name = a} :: Wave)

-- | Wave tags.
wave_tags :: Lens.Lens' Wave (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
wave_tags = Lens.lens (\Wave' {tags} -> tags) (\s@Wave' {} a -> s {tags = a} :: Wave) Prelude.. Lens.mapping (Data._Sensitive Prelude.. Lens.coerced)

-- | Wave aggregated status.
wave_waveAggregatedStatus :: Lens.Lens' Wave (Prelude.Maybe WaveAggregatedStatus)
wave_waveAggregatedStatus = Lens.lens (\Wave' {waveAggregatedStatus} -> waveAggregatedStatus) (\s@Wave' {} a -> s {waveAggregatedStatus = a} :: Wave)

-- | Wave ID.
wave_waveID :: Lens.Lens' Wave (Prelude.Maybe Prelude.Text)
wave_waveID = Lens.lens (\Wave' {waveID} -> waveID) (\s@Wave' {} a -> s {waveID = a} :: Wave)

instance Data.FromJSON Wave where
  parseJSON =
    Data.withObject
      "Wave"
      ( \x ->
          Wave'
            Prelude.<$> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "creationDateTime")
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "isArchived")
            Prelude.<*> (x Data..:? "lastModifiedDateTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "waveAggregatedStatus")
            Prelude.<*> (x Data..:? "waveID")
      )

instance Prelude.Hashable Wave where
  hashWithSalt _salt Wave' {..} =
    _salt `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` isArchived
      `Prelude.hashWithSalt` lastModifiedDateTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` waveAggregatedStatus
      `Prelude.hashWithSalt` waveID

instance Prelude.NFData Wave where
  rnf Wave' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf isArchived
      `Prelude.seq` Prelude.rnf lastModifiedDateTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf waveAggregatedStatus
      `Prelude.seq` Prelude.rnf waveID
