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
-- Module      : Amazonka.DrS.Types.JobLogEventData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.JobLogEventData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DrS.Types.ConversionProperties
import qualified Amazonka.Prelude as Prelude

-- | Metadata associated with a Job log.
--
-- /See:/ 'newJobLogEventData' smart constructor.
data JobLogEventData = JobLogEventData'
  { -- | The ID of a Recovery Instance.
    targetInstanceID :: Prelude.Maybe Prelude.Text,
    -- | A string representing a job error.
    rawError :: Prelude.Maybe Prelude.Text,
    -- | The ID of a conversion server.
    conversionServerID :: Prelude.Maybe Prelude.Text,
    -- | Properties of a conversion job
    conversionProperties :: Prelude.Maybe ConversionProperties,
    -- | The ID of a Source Server.
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
-- 'targetInstanceID', 'jobLogEventData_targetInstanceID' - The ID of a Recovery Instance.
--
-- 'rawError', 'jobLogEventData_rawError' - A string representing a job error.
--
-- 'conversionServerID', 'jobLogEventData_conversionServerID' - The ID of a conversion server.
--
-- 'conversionProperties', 'jobLogEventData_conversionProperties' - Properties of a conversion job
--
-- 'sourceServerID', 'jobLogEventData_sourceServerID' - The ID of a Source Server.
newJobLogEventData ::
  JobLogEventData
newJobLogEventData =
  JobLogEventData'
    { targetInstanceID =
        Prelude.Nothing,
      rawError = Prelude.Nothing,
      conversionServerID = Prelude.Nothing,
      conversionProperties = Prelude.Nothing,
      sourceServerID = Prelude.Nothing
    }

-- | The ID of a Recovery Instance.
jobLogEventData_targetInstanceID :: Lens.Lens' JobLogEventData (Prelude.Maybe Prelude.Text)
jobLogEventData_targetInstanceID = Lens.lens (\JobLogEventData' {targetInstanceID} -> targetInstanceID) (\s@JobLogEventData' {} a -> s {targetInstanceID = a} :: JobLogEventData)

-- | A string representing a job error.
jobLogEventData_rawError :: Lens.Lens' JobLogEventData (Prelude.Maybe Prelude.Text)
jobLogEventData_rawError = Lens.lens (\JobLogEventData' {rawError} -> rawError) (\s@JobLogEventData' {} a -> s {rawError = a} :: JobLogEventData)

-- | The ID of a conversion server.
jobLogEventData_conversionServerID :: Lens.Lens' JobLogEventData (Prelude.Maybe Prelude.Text)
jobLogEventData_conversionServerID = Lens.lens (\JobLogEventData' {conversionServerID} -> conversionServerID) (\s@JobLogEventData' {} a -> s {conversionServerID = a} :: JobLogEventData)

-- | Properties of a conversion job
jobLogEventData_conversionProperties :: Lens.Lens' JobLogEventData (Prelude.Maybe ConversionProperties)
jobLogEventData_conversionProperties = Lens.lens (\JobLogEventData' {conversionProperties} -> conversionProperties) (\s@JobLogEventData' {} a -> s {conversionProperties = a} :: JobLogEventData)

-- | The ID of a Source Server.
jobLogEventData_sourceServerID :: Lens.Lens' JobLogEventData (Prelude.Maybe Prelude.Text)
jobLogEventData_sourceServerID = Lens.lens (\JobLogEventData' {sourceServerID} -> sourceServerID) (\s@JobLogEventData' {} a -> s {sourceServerID = a} :: JobLogEventData)

instance Core.FromJSON JobLogEventData where
  parseJSON =
    Core.withObject
      "JobLogEventData"
      ( \x ->
          JobLogEventData'
            Prelude.<$> (x Core..:? "targetInstanceID")
            Prelude.<*> (x Core..:? "rawError")
            Prelude.<*> (x Core..:? "conversionServerID")
            Prelude.<*> (x Core..:? "conversionProperties")
            Prelude.<*> (x Core..:? "sourceServerID")
      )

instance Prelude.Hashable JobLogEventData where
  hashWithSalt _salt JobLogEventData' {..} =
    _salt `Prelude.hashWithSalt` targetInstanceID
      `Prelude.hashWithSalt` rawError
      `Prelude.hashWithSalt` conversionServerID
      `Prelude.hashWithSalt` conversionProperties
      `Prelude.hashWithSalt` sourceServerID

instance Prelude.NFData JobLogEventData where
  rnf JobLogEventData' {..} =
    Prelude.rnf targetInstanceID
      `Prelude.seq` Prelude.rnf rawError
      `Prelude.seq` Prelude.rnf conversionServerID
      `Prelude.seq` Prelude.rnf conversionProperties
      `Prelude.seq` Prelude.rnf sourceServerID
