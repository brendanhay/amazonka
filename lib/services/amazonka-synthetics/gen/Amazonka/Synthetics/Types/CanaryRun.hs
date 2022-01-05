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
-- Module      : Amazonka.Synthetics.Types.CanaryRun
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Synthetics.Types.CanaryRun where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Synthetics.Types.CanaryRunStatus
import Amazonka.Synthetics.Types.CanaryRunTimeline

-- | This structure contains the details about one run of one canary.
--
-- /See:/ 'newCanaryRun' smart constructor.
data CanaryRun = CanaryRun'
  { -- | The status of this run.
    status :: Prelude.Maybe CanaryRunStatus,
    -- | The location where the canary stored artifacts from the run. Artifacts
    -- include the log file, screenshots, and HAR files.
    artifactS3Location :: Prelude.Maybe Prelude.Text,
    -- | The name of the canary.
    name :: Prelude.Maybe Prelude.Text,
    -- | A unique ID that identifies this canary run.
    id :: Prelude.Maybe Prelude.Text,
    -- | A structure that contains the start and end times of this run.
    timeline :: Prelude.Maybe CanaryRunTimeline
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CanaryRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'canaryRun_status' - The status of this run.
--
-- 'artifactS3Location', 'canaryRun_artifactS3Location' - The location where the canary stored artifacts from the run. Artifacts
-- include the log file, screenshots, and HAR files.
--
-- 'name', 'canaryRun_name' - The name of the canary.
--
-- 'id', 'canaryRun_id' - A unique ID that identifies this canary run.
--
-- 'timeline', 'canaryRun_timeline' - A structure that contains the start and end times of this run.
newCanaryRun ::
  CanaryRun
newCanaryRun =
  CanaryRun'
    { status = Prelude.Nothing,
      artifactS3Location = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      timeline = Prelude.Nothing
    }

-- | The status of this run.
canaryRun_status :: Lens.Lens' CanaryRun (Prelude.Maybe CanaryRunStatus)
canaryRun_status = Lens.lens (\CanaryRun' {status} -> status) (\s@CanaryRun' {} a -> s {status = a} :: CanaryRun)

-- | The location where the canary stored artifacts from the run. Artifacts
-- include the log file, screenshots, and HAR files.
canaryRun_artifactS3Location :: Lens.Lens' CanaryRun (Prelude.Maybe Prelude.Text)
canaryRun_artifactS3Location = Lens.lens (\CanaryRun' {artifactS3Location} -> artifactS3Location) (\s@CanaryRun' {} a -> s {artifactS3Location = a} :: CanaryRun)

-- | The name of the canary.
canaryRun_name :: Lens.Lens' CanaryRun (Prelude.Maybe Prelude.Text)
canaryRun_name = Lens.lens (\CanaryRun' {name} -> name) (\s@CanaryRun' {} a -> s {name = a} :: CanaryRun)

-- | A unique ID that identifies this canary run.
canaryRun_id :: Lens.Lens' CanaryRun (Prelude.Maybe Prelude.Text)
canaryRun_id = Lens.lens (\CanaryRun' {id} -> id) (\s@CanaryRun' {} a -> s {id = a} :: CanaryRun)

-- | A structure that contains the start and end times of this run.
canaryRun_timeline :: Lens.Lens' CanaryRun (Prelude.Maybe CanaryRunTimeline)
canaryRun_timeline = Lens.lens (\CanaryRun' {timeline} -> timeline) (\s@CanaryRun' {} a -> s {timeline = a} :: CanaryRun)

instance Core.FromJSON CanaryRun where
  parseJSON =
    Core.withObject
      "CanaryRun"
      ( \x ->
          CanaryRun'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "ArtifactS3Location")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "Timeline")
      )

instance Prelude.Hashable CanaryRun where
  hashWithSalt _salt CanaryRun' {..} =
    _salt `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` artifactS3Location
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` timeline

instance Prelude.NFData CanaryRun where
  rnf CanaryRun' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf artifactS3Location
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf timeline
