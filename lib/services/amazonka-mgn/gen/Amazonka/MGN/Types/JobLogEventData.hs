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
-- Module      : Amazonka.MGN.Types.JobLogEventData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.JobLogEventData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Job log data
--
-- /See:/ 'newJobLogEventData' smart constructor.
data JobLogEventData = JobLogEventData'
  { -- | Job Event Target instance ID.
    targetInstanceID :: Prelude.Maybe Prelude.Text,
    -- | Job error.
    rawError :: Prelude.Maybe Prelude.Text,
    -- | Job Event conversion Server ID.
    conversionServerID :: Prelude.Maybe Prelude.Text,
    -- | Job Event Source Server ID.
    sourceServerID :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobLogEventData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetInstanceID', 'jobLogEventData_targetInstanceID' - Job Event Target instance ID.
--
-- 'rawError', 'jobLogEventData_rawError' - Job error.
--
-- 'conversionServerID', 'jobLogEventData_conversionServerID' - Job Event conversion Server ID.
--
-- 'sourceServerID', 'jobLogEventData_sourceServerID' - Job Event Source Server ID.
newJobLogEventData ::
  JobLogEventData
newJobLogEventData =
  JobLogEventData'
    { targetInstanceID =
        Prelude.Nothing,
      rawError = Prelude.Nothing,
      conversionServerID = Prelude.Nothing,
      sourceServerID = Prelude.Nothing
    }

-- | Job Event Target instance ID.
jobLogEventData_targetInstanceID :: Lens.Lens' JobLogEventData (Prelude.Maybe Prelude.Text)
jobLogEventData_targetInstanceID = Lens.lens (\JobLogEventData' {targetInstanceID} -> targetInstanceID) (\s@JobLogEventData' {} a -> s {targetInstanceID = a} :: JobLogEventData)

-- | Job error.
jobLogEventData_rawError :: Lens.Lens' JobLogEventData (Prelude.Maybe Prelude.Text)
jobLogEventData_rawError = Lens.lens (\JobLogEventData' {rawError} -> rawError) (\s@JobLogEventData' {} a -> s {rawError = a} :: JobLogEventData)

-- | Job Event conversion Server ID.
jobLogEventData_conversionServerID :: Lens.Lens' JobLogEventData (Prelude.Maybe Prelude.Text)
jobLogEventData_conversionServerID = Lens.lens (\JobLogEventData' {conversionServerID} -> conversionServerID) (\s@JobLogEventData' {} a -> s {conversionServerID = a} :: JobLogEventData)

-- | Job Event Source Server ID.
jobLogEventData_sourceServerID :: Lens.Lens' JobLogEventData (Prelude.Maybe Prelude.Text)
jobLogEventData_sourceServerID = Lens.lens (\JobLogEventData' {sourceServerID} -> sourceServerID) (\s@JobLogEventData' {} a -> s {sourceServerID = a} :: JobLogEventData)

instance Data.FromJSON JobLogEventData where
  parseJSON =
    Data.withObject
      "JobLogEventData"
      ( \x ->
          JobLogEventData'
            Prelude.<$> (x Data..:? "targetInstanceID")
            Prelude.<*> (x Data..:? "rawError")
            Prelude.<*> (x Data..:? "conversionServerID")
            Prelude.<*> (x Data..:? "sourceServerID")
      )

instance Prelude.Hashable JobLogEventData where
  hashWithSalt _salt JobLogEventData' {..} =
    _salt `Prelude.hashWithSalt` targetInstanceID
      `Prelude.hashWithSalt` rawError
      `Prelude.hashWithSalt` conversionServerID
      `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData JobLogEventData where
  rnf JobLogEventData' {..} =
    Prelude.rnf targetInstanceID
      `Prelude.seq` Prelude.rnf rawError
      `Prelude.seq` Prelude.rnf conversionServerID
      `Prelude.seq` Prelude.rnf sourceServerID
