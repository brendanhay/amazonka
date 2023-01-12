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
-- Module      : Amazonka.MacieV2.Types.LastRunErrorStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.LastRunErrorStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.LastRunErrorStatusCode
import qualified Amazonka.Prelude as Prelude

-- | Specifies whether any account- or bucket-level access errors occurred
-- when a classification job ran. For information about using logging data
-- to investigate these errors, see
-- <https://docs.aws.amazon.com/macie/latest/user/discovery-jobs-monitor-cw-logs.html Monitoring sensitive data discovery jobs>
-- in the /Amazon Macie User Guide/.
--
-- /See:/ 'newLastRunErrorStatus' smart constructor.
data LastRunErrorStatus = LastRunErrorStatus'
  { -- | Specifies whether any account- or bucket-level access errors occurred
    -- when the job ran. For a recurring job, this value indicates the error
    -- status of the job\'s most recent run. Possible values are:
    --
    -- -   ERROR - One or more errors occurred. Amazon Macie didn\'t process
    --     all the data specified for the job.
    --
    -- -   NONE - No errors occurred. Macie processed all the data specified
    --     for the job.
    code :: Prelude.Maybe LastRunErrorStatusCode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LastRunErrorStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'code', 'lastRunErrorStatus_code' - Specifies whether any account- or bucket-level access errors occurred
-- when the job ran. For a recurring job, this value indicates the error
-- status of the job\'s most recent run. Possible values are:
--
-- -   ERROR - One or more errors occurred. Amazon Macie didn\'t process
--     all the data specified for the job.
--
-- -   NONE - No errors occurred. Macie processed all the data specified
--     for the job.
newLastRunErrorStatus ::
  LastRunErrorStatus
newLastRunErrorStatus =
  LastRunErrorStatus' {code = Prelude.Nothing}

-- | Specifies whether any account- or bucket-level access errors occurred
-- when the job ran. For a recurring job, this value indicates the error
-- status of the job\'s most recent run. Possible values are:
--
-- -   ERROR - One or more errors occurred. Amazon Macie didn\'t process
--     all the data specified for the job.
--
-- -   NONE - No errors occurred. Macie processed all the data specified
--     for the job.
lastRunErrorStatus_code :: Lens.Lens' LastRunErrorStatus (Prelude.Maybe LastRunErrorStatusCode)
lastRunErrorStatus_code = Lens.lens (\LastRunErrorStatus' {code} -> code) (\s@LastRunErrorStatus' {} a -> s {code = a} :: LastRunErrorStatus)

instance Data.FromJSON LastRunErrorStatus where
  parseJSON =
    Data.withObject
      "LastRunErrorStatus"
      ( \x ->
          LastRunErrorStatus' Prelude.<$> (x Data..:? "code")
      )

instance Prelude.Hashable LastRunErrorStatus where
  hashWithSalt _salt LastRunErrorStatus' {..} =
    _salt `Prelude.hashWithSalt` code

instance Prelude.NFData LastRunErrorStatus where
  rnf LastRunErrorStatus' {..} = Prelude.rnf code
