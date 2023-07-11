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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
  { -- | Job Event conversion Server ID.
    conversionServerID :: Prelude.Maybe Prelude.Text,
    -- | Job error.
    rawError :: Prelude.Maybe Prelude.Text,
    -- | Job Event Source Server ID.
    sourceServerID :: Prelude.Maybe Prelude.Text,
    -- | Job Event Target instance ID.
    targetInstanceID :: Prelude.Maybe Prelude.Text
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
-- 'conversionServerID', 'jobLogEventData_conversionServerID' - Job Event conversion Server ID.
--
-- 'rawError', 'jobLogEventData_rawError' - Job error.
--
-- 'sourceServerID', 'jobLogEventData_sourceServerID' - Job Event Source Server ID.
--
-- 'targetInstanceID', 'jobLogEventData_targetInstanceID' - Job Event Target instance ID.
newJobLogEventData ::
  JobLogEventData
newJobLogEventData =
  JobLogEventData'
    { conversionServerID =
        Prelude.Nothing,
      rawError = Prelude.Nothing,
      sourceServerID = Prelude.Nothing,
      targetInstanceID = Prelude.Nothing
    }

-- | Job Event conversion Server ID.
jobLogEventData_conversionServerID :: Lens.Lens' JobLogEventData (Prelude.Maybe Prelude.Text)
jobLogEventData_conversionServerID = Lens.lens (\JobLogEventData' {conversionServerID} -> conversionServerID) (\s@JobLogEventData' {} a -> s {conversionServerID = a} :: JobLogEventData)

-- | Job error.
jobLogEventData_rawError :: Lens.Lens' JobLogEventData (Prelude.Maybe Prelude.Text)
jobLogEventData_rawError = Lens.lens (\JobLogEventData' {rawError} -> rawError) (\s@JobLogEventData' {} a -> s {rawError = a} :: JobLogEventData)

-- | Job Event Source Server ID.
jobLogEventData_sourceServerID :: Lens.Lens' JobLogEventData (Prelude.Maybe Prelude.Text)
jobLogEventData_sourceServerID = Lens.lens (\JobLogEventData' {sourceServerID} -> sourceServerID) (\s@JobLogEventData' {} a -> s {sourceServerID = a} :: JobLogEventData)

-- | Job Event Target instance ID.
jobLogEventData_targetInstanceID :: Lens.Lens' JobLogEventData (Prelude.Maybe Prelude.Text)
jobLogEventData_targetInstanceID = Lens.lens (\JobLogEventData' {targetInstanceID} -> targetInstanceID) (\s@JobLogEventData' {} a -> s {targetInstanceID = a} :: JobLogEventData)

instance Data.FromJSON JobLogEventData where
  parseJSON =
    Data.withObject
      "JobLogEventData"
      ( \x ->
          JobLogEventData'
            Prelude.<$> (x Data..:? "conversionServerID")
            Prelude.<*> (x Data..:? "rawError")
            Prelude.<*> (x Data..:? "sourceServerID")
            Prelude.<*> (x Data..:? "targetInstanceID")
      )

instance Prelude.Hashable JobLogEventData where
  hashWithSalt _salt JobLogEventData' {..} =
    _salt
      `Prelude.hashWithSalt` conversionServerID
      `Prelude.hashWithSalt` rawError
      `Prelude.hashWithSalt` sourceServerID
      `Prelude.hashWithSalt` targetInstanceID

instance Prelude.NFData JobLogEventData where
  rnf JobLogEventData' {..} =
    Prelude.rnf conversionServerID
      `Prelude.seq` Prelude.rnf rawError
      `Prelude.seq` Prelude.rnf sourceServerID
      `Prelude.seq` Prelude.rnf targetInstanceID
