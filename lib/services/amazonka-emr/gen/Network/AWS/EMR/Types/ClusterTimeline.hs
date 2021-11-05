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
-- Module      : Amazonka.EMR.Types.ClusterTimeline
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EMR.Types.ClusterTimeline where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Represents the timeline of the cluster\'s lifecycle.
--
-- /See:/ 'newClusterTimeline' smart constructor.
data ClusterTimeline = ClusterTimeline'
  { -- | The date and time when the cluster was ready to run steps.
    readyDateTime :: Prelude.Maybe Core.POSIX,
    -- | The creation date and time of the cluster.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The date and time when the cluster was terminated.
    endDateTime :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ClusterTimeline' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'readyDateTime', 'clusterTimeline_readyDateTime' - The date and time when the cluster was ready to run steps.
--
-- 'creationDateTime', 'clusterTimeline_creationDateTime' - The creation date and time of the cluster.
--
-- 'endDateTime', 'clusterTimeline_endDateTime' - The date and time when the cluster was terminated.
newClusterTimeline ::
  ClusterTimeline
newClusterTimeline =
  ClusterTimeline'
    { readyDateTime = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      endDateTime = Prelude.Nothing
    }

-- | The date and time when the cluster was ready to run steps.
clusterTimeline_readyDateTime :: Lens.Lens' ClusterTimeline (Prelude.Maybe Prelude.UTCTime)
clusterTimeline_readyDateTime = Lens.lens (\ClusterTimeline' {readyDateTime} -> readyDateTime) (\s@ClusterTimeline' {} a -> s {readyDateTime = a} :: ClusterTimeline) Prelude.. Lens.mapping Core._Time

-- | The creation date and time of the cluster.
clusterTimeline_creationDateTime :: Lens.Lens' ClusterTimeline (Prelude.Maybe Prelude.UTCTime)
clusterTimeline_creationDateTime = Lens.lens (\ClusterTimeline' {creationDateTime} -> creationDateTime) (\s@ClusterTimeline' {} a -> s {creationDateTime = a} :: ClusterTimeline) Prelude.. Lens.mapping Core._Time

-- | The date and time when the cluster was terminated.
clusterTimeline_endDateTime :: Lens.Lens' ClusterTimeline (Prelude.Maybe Prelude.UTCTime)
clusterTimeline_endDateTime = Lens.lens (\ClusterTimeline' {endDateTime} -> endDateTime) (\s@ClusterTimeline' {} a -> s {endDateTime = a} :: ClusterTimeline) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ClusterTimeline where
  parseJSON =
    Core.withObject
      "ClusterTimeline"
      ( \x ->
          ClusterTimeline'
            Prelude.<$> (x Core..:? "ReadyDateTime")
            Prelude.<*> (x Core..:? "CreationDateTime")
            Prelude.<*> (x Core..:? "EndDateTime")
      )

instance Prelude.Hashable ClusterTimeline

instance Prelude.NFData ClusterTimeline
