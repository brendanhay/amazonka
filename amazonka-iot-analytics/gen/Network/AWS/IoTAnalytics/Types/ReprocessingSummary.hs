{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.IoTAnalytics.Types.ReprocessingSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTAnalytics.Types.ReprocessingSummary where

import Network.AWS.IoTAnalytics.Types.ReprocessingStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about pipeline reprocessing.
--
-- /See:/ 'newReprocessingSummary' smart constructor.
data ReprocessingSummary = ReprocessingSummary'
  { -- | The status of the pipeline reprocessing.
    status :: Prelude.Maybe ReprocessingStatus,
    -- | The time the pipeline reprocessing was created.
    creationTime :: Prelude.Maybe Prelude.POSIX,
    -- | The @reprocessingId@ returned by @StartPipelineReprocessing@.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ReprocessingSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'reprocessingSummary_status' - The status of the pipeline reprocessing.
--
-- 'creationTime', 'reprocessingSummary_creationTime' - The time the pipeline reprocessing was created.
--
-- 'id', 'reprocessingSummary_id' - The @reprocessingId@ returned by @StartPipelineReprocessing@.
newReprocessingSummary ::
  ReprocessingSummary
newReprocessingSummary =
  ReprocessingSummary'
    { status = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The status of the pipeline reprocessing.
reprocessingSummary_status :: Lens.Lens' ReprocessingSummary (Prelude.Maybe ReprocessingStatus)
reprocessingSummary_status = Lens.lens (\ReprocessingSummary' {status} -> status) (\s@ReprocessingSummary' {} a -> s {status = a} :: ReprocessingSummary)

-- | The time the pipeline reprocessing was created.
reprocessingSummary_creationTime :: Lens.Lens' ReprocessingSummary (Prelude.Maybe Prelude.UTCTime)
reprocessingSummary_creationTime = Lens.lens (\ReprocessingSummary' {creationTime} -> creationTime) (\s@ReprocessingSummary' {} a -> s {creationTime = a} :: ReprocessingSummary) Prelude.. Lens.mapping Prelude._Time

-- | The @reprocessingId@ returned by @StartPipelineReprocessing@.
reprocessingSummary_id :: Lens.Lens' ReprocessingSummary (Prelude.Maybe Prelude.Text)
reprocessingSummary_id = Lens.lens (\ReprocessingSummary' {id} -> id) (\s@ReprocessingSummary' {} a -> s {id = a} :: ReprocessingSummary)

instance Prelude.FromJSON ReprocessingSummary where
  parseJSON =
    Prelude.withObject
      "ReprocessingSummary"
      ( \x ->
          ReprocessingSummary'
            Prelude.<$> (x Prelude..:? "status")
            Prelude.<*> (x Prelude..:? "creationTime")
            Prelude.<*> (x Prelude..:? "id")
      )

instance Prelude.Hashable ReprocessingSummary

instance Prelude.NFData ReprocessingSummary
