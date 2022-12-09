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
-- Module      : Amazonka.IoTAnalytics.Types.ReprocessingSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTAnalytics.Types.ReprocessingSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTAnalytics.Types.ReprocessingStatus
import qualified Amazonka.Prelude as Prelude

-- | Information about pipeline reprocessing.
--
-- /See:/ 'newReprocessingSummary' smart constructor.
data ReprocessingSummary = ReprocessingSummary'
  { -- | The time the pipeline reprocessing was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The @reprocessingId@ returned by @StartPipelineReprocessing@.
    id :: Prelude.Maybe Prelude.Text,
    -- | The status of the pipeline reprocessing.
    status :: Prelude.Maybe ReprocessingStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ReprocessingSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'reprocessingSummary_creationTime' - The time the pipeline reprocessing was created.
--
-- 'id', 'reprocessingSummary_id' - The @reprocessingId@ returned by @StartPipelineReprocessing@.
--
-- 'status', 'reprocessingSummary_status' - The status of the pipeline reprocessing.
newReprocessingSummary ::
  ReprocessingSummary
newReprocessingSummary =
  ReprocessingSummary'
    { creationTime =
        Prelude.Nothing,
      id = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The time the pipeline reprocessing was created.
reprocessingSummary_creationTime :: Lens.Lens' ReprocessingSummary (Prelude.Maybe Prelude.UTCTime)
reprocessingSummary_creationTime = Lens.lens (\ReprocessingSummary' {creationTime} -> creationTime) (\s@ReprocessingSummary' {} a -> s {creationTime = a} :: ReprocessingSummary) Prelude.. Lens.mapping Data._Time

-- | The @reprocessingId@ returned by @StartPipelineReprocessing@.
reprocessingSummary_id :: Lens.Lens' ReprocessingSummary (Prelude.Maybe Prelude.Text)
reprocessingSummary_id = Lens.lens (\ReprocessingSummary' {id} -> id) (\s@ReprocessingSummary' {} a -> s {id = a} :: ReprocessingSummary)

-- | The status of the pipeline reprocessing.
reprocessingSummary_status :: Lens.Lens' ReprocessingSummary (Prelude.Maybe ReprocessingStatus)
reprocessingSummary_status = Lens.lens (\ReprocessingSummary' {status} -> status) (\s@ReprocessingSummary' {} a -> s {status = a} :: ReprocessingSummary)

instance Data.FromJSON ReprocessingSummary where
  parseJSON =
    Data.withObject
      "ReprocessingSummary"
      ( \x ->
          ReprocessingSummary'
            Prelude.<$> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "id")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable ReprocessingSummary where
  hashWithSalt _salt ReprocessingSummary' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` status

instance Prelude.NFData ReprocessingSummary where
  rnf ReprocessingSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf status
