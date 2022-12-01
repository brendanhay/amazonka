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
-- Module      : Amazonka.OpenSearch.Types.AutoTuneDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpenSearch.Types.AutoTuneDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpenSearch.Types.ScheduledAutoTuneDetails
import qualified Amazonka.Prelude as Prelude

-- | Specifies details about a scheduled Auto-Tune action. For more
-- information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/auto-tune.html Auto-Tune for Amazon OpenSearch Service>.
--
-- /See:/ 'newAutoTuneDetails' smart constructor.
data AutoTuneDetails = AutoTuneDetails'
  { -- | Container for details about a scheduled Auto-Tune action.
    scheduledAutoTuneDetails :: Prelude.Maybe ScheduledAutoTuneDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AutoTuneDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'scheduledAutoTuneDetails', 'autoTuneDetails_scheduledAutoTuneDetails' - Container for details about a scheduled Auto-Tune action.
newAutoTuneDetails ::
  AutoTuneDetails
newAutoTuneDetails =
  AutoTuneDetails'
    { scheduledAutoTuneDetails =
        Prelude.Nothing
    }

-- | Container for details about a scheduled Auto-Tune action.
autoTuneDetails_scheduledAutoTuneDetails :: Lens.Lens' AutoTuneDetails (Prelude.Maybe ScheduledAutoTuneDetails)
autoTuneDetails_scheduledAutoTuneDetails = Lens.lens (\AutoTuneDetails' {scheduledAutoTuneDetails} -> scheduledAutoTuneDetails) (\s@AutoTuneDetails' {} a -> s {scheduledAutoTuneDetails = a} :: AutoTuneDetails)

instance Core.FromJSON AutoTuneDetails where
  parseJSON =
    Core.withObject
      "AutoTuneDetails"
      ( \x ->
          AutoTuneDetails'
            Prelude.<$> (x Core..:? "ScheduledAutoTuneDetails")
      )

instance Prelude.Hashable AutoTuneDetails where
  hashWithSalt _salt AutoTuneDetails' {..} =
    _salt
      `Prelude.hashWithSalt` scheduledAutoTuneDetails

instance Prelude.NFData AutoTuneDetails where
  rnf AutoTuneDetails' {..} =
    Prelude.rnf scheduledAutoTuneDetails
