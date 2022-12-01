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
-- Module      : Amazonka.IoTEventsData.Types.DetectorStateSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEventsData.Types.DetectorStateSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about the detector state.
--
-- /See:/ 'newDetectorStateSummary' smart constructor.
data DetectorStateSummary = DetectorStateSummary'
  { -- | The name of the state.
    stateName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DetectorStateSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stateName', 'detectorStateSummary_stateName' - The name of the state.
newDetectorStateSummary ::
  DetectorStateSummary
newDetectorStateSummary =
  DetectorStateSummary' {stateName = Prelude.Nothing}

-- | The name of the state.
detectorStateSummary_stateName :: Lens.Lens' DetectorStateSummary (Prelude.Maybe Prelude.Text)
detectorStateSummary_stateName = Lens.lens (\DetectorStateSummary' {stateName} -> stateName) (\s@DetectorStateSummary' {} a -> s {stateName = a} :: DetectorStateSummary)

instance Core.FromJSON DetectorStateSummary where
  parseJSON =
    Core.withObject
      "DetectorStateSummary"
      ( \x ->
          DetectorStateSummary'
            Prelude.<$> (x Core..:? "stateName")
      )

instance Prelude.Hashable DetectorStateSummary where
  hashWithSalt _salt DetectorStateSummary' {..} =
    _salt `Prelude.hashWithSalt` stateName

instance Prelude.NFData DetectorStateSummary where
  rnf DetectorStateSummary' {..} = Prelude.rnf stateName
