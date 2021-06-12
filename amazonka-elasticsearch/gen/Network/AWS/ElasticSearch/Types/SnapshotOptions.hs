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
-- Module      : Network.AWS.ElasticSearch.Types.SnapshotOptions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.SnapshotOptions where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the time, in UTC format, when the service takes a daily
-- automated snapshot of the specified Elasticsearch domain. Default value
-- is @0@ hours.
--
-- /See:/ 'newSnapshotOptions' smart constructor.
data SnapshotOptions = SnapshotOptions'
  { -- | Specifies the time, in UTC format, when the service takes a daily
    -- automated snapshot of the specified Elasticsearch domain. Default value
    -- is @0@ hours.
    automatedSnapshotStartHour :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SnapshotOptions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'automatedSnapshotStartHour', 'snapshotOptions_automatedSnapshotStartHour' - Specifies the time, in UTC format, when the service takes a daily
-- automated snapshot of the specified Elasticsearch domain. Default value
-- is @0@ hours.
newSnapshotOptions ::
  SnapshotOptions
newSnapshotOptions =
  SnapshotOptions'
    { automatedSnapshotStartHour =
        Core.Nothing
    }

-- | Specifies the time, in UTC format, when the service takes a daily
-- automated snapshot of the specified Elasticsearch domain. Default value
-- is @0@ hours.
snapshotOptions_automatedSnapshotStartHour :: Lens.Lens' SnapshotOptions (Core.Maybe Core.Int)
snapshotOptions_automatedSnapshotStartHour = Lens.lens (\SnapshotOptions' {automatedSnapshotStartHour} -> automatedSnapshotStartHour) (\s@SnapshotOptions' {} a -> s {automatedSnapshotStartHour = a} :: SnapshotOptions)

instance Core.FromJSON SnapshotOptions where
  parseJSON =
    Core.withObject
      "SnapshotOptions"
      ( \x ->
          SnapshotOptions'
            Core.<$> (x Core..:? "AutomatedSnapshotStartHour")
      )

instance Core.Hashable SnapshotOptions

instance Core.NFData SnapshotOptions

instance Core.ToJSON SnapshotOptions where
  toJSON SnapshotOptions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AutomatedSnapshotStartHour" Core..=)
              Core.<$> automatedSnapshotStartHour
          ]
      )
