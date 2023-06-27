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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.JobLogEventData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.ConversionProperties
import Amazonka.DrS.Types.EventResourceData
import qualified Amazonka.Prelude as Prelude

-- | Metadata associated with a Job log.
--
-- /See:/ 'newJobLogEventData' smart constructor.
data JobLogEventData = JobLogEventData'
  { -- | Properties of a conversion job
    conversionProperties :: Prelude.Maybe ConversionProperties,
    -- | The ID of a conversion server.
    conversionServerID :: Prelude.Maybe Prelude.Text,
    -- | Properties of resource related to a job event.
    eventResourceData :: Prelude.Maybe EventResourceData,
    -- | A string representing a job error.
    rawError :: Prelude.Maybe Prelude.Text,
    -- | The ID of a Source Server.
    sourceServerID :: Prelude.Maybe Prelude.Text,
    -- | The ID of a Recovery Instance.
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
-- 'conversionProperties', 'jobLogEventData_conversionProperties' - Properties of a conversion job
--
-- 'conversionServerID', 'jobLogEventData_conversionServerID' - The ID of a conversion server.
--
-- 'eventResourceData', 'jobLogEventData_eventResourceData' - Properties of resource related to a job event.
--
-- 'rawError', 'jobLogEventData_rawError' - A string representing a job error.
--
-- 'sourceServerID', 'jobLogEventData_sourceServerID' - The ID of a Source Server.
--
-- 'targetInstanceID', 'jobLogEventData_targetInstanceID' - The ID of a Recovery Instance.
newJobLogEventData ::
  JobLogEventData
newJobLogEventData =
  JobLogEventData'
    { conversionProperties =
        Prelude.Nothing,
      conversionServerID = Prelude.Nothing,
      eventResourceData = Prelude.Nothing,
      rawError = Prelude.Nothing,
      sourceServerID = Prelude.Nothing,
      targetInstanceID = Prelude.Nothing
    }

-- | Properties of a conversion job
jobLogEventData_conversionProperties :: Lens.Lens' JobLogEventData (Prelude.Maybe ConversionProperties)
jobLogEventData_conversionProperties = Lens.lens (\JobLogEventData' {conversionProperties} -> conversionProperties) (\s@JobLogEventData' {} a -> s {conversionProperties = a} :: JobLogEventData)

-- | The ID of a conversion server.
jobLogEventData_conversionServerID :: Lens.Lens' JobLogEventData (Prelude.Maybe Prelude.Text)
jobLogEventData_conversionServerID = Lens.lens (\JobLogEventData' {conversionServerID} -> conversionServerID) (\s@JobLogEventData' {} a -> s {conversionServerID = a} :: JobLogEventData)

-- | Properties of resource related to a job event.
jobLogEventData_eventResourceData :: Lens.Lens' JobLogEventData (Prelude.Maybe EventResourceData)
jobLogEventData_eventResourceData = Lens.lens (\JobLogEventData' {eventResourceData} -> eventResourceData) (\s@JobLogEventData' {} a -> s {eventResourceData = a} :: JobLogEventData)

-- | A string representing a job error.
jobLogEventData_rawError :: Lens.Lens' JobLogEventData (Prelude.Maybe Prelude.Text)
jobLogEventData_rawError = Lens.lens (\JobLogEventData' {rawError} -> rawError) (\s@JobLogEventData' {} a -> s {rawError = a} :: JobLogEventData)

-- | The ID of a Source Server.
jobLogEventData_sourceServerID :: Lens.Lens' JobLogEventData (Prelude.Maybe Prelude.Text)
jobLogEventData_sourceServerID = Lens.lens (\JobLogEventData' {sourceServerID} -> sourceServerID) (\s@JobLogEventData' {} a -> s {sourceServerID = a} :: JobLogEventData)

-- | The ID of a Recovery Instance.
jobLogEventData_targetInstanceID :: Lens.Lens' JobLogEventData (Prelude.Maybe Prelude.Text)
jobLogEventData_targetInstanceID = Lens.lens (\JobLogEventData' {targetInstanceID} -> targetInstanceID) (\s@JobLogEventData' {} a -> s {targetInstanceID = a} :: JobLogEventData)

instance Data.FromJSON JobLogEventData where
  parseJSON =
    Data.withObject
      "JobLogEventData"
      ( \x ->
          JobLogEventData'
            Prelude.<$> (x Data..:? "conversionProperties")
            Prelude.<*> (x Data..:? "conversionServerID")
            Prelude.<*> (x Data..:? "eventResourceData")
            Prelude.<*> (x Data..:? "rawError")
            Prelude.<*> (x Data..:? "sourceServerID")
            Prelude.<*> (x Data..:? "targetInstanceID")
      )

instance Prelude.Hashable JobLogEventData where
  hashWithSalt _salt JobLogEventData' {..} =
    _salt
      `Prelude.hashWithSalt` conversionProperties
      `Prelude.hashWithSalt` conversionServerID
      `Prelude.hashWithSalt` eventResourceData
      `Prelude.hashWithSalt` rawError
      `Prelude.hashWithSalt` sourceServerID
      `Prelude.hashWithSalt` targetInstanceID

instance Prelude.NFData JobLogEventData where
  rnf JobLogEventData' {..} =
    Prelude.rnf conversionProperties
      `Prelude.seq` Prelude.rnf conversionServerID
      `Prelude.seq` Prelude.rnf eventResourceData
      `Prelude.seq` Prelude.rnf rawError
      `Prelude.seq` Prelude.rnf sourceServerID
      `Prelude.seq` Prelude.rnf targetInstanceID
