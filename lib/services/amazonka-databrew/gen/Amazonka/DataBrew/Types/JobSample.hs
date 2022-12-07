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
-- Module      : Amazonka.DataBrew.Types.JobSample
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataBrew.Types.JobSample where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types.SampleMode
import qualified Amazonka.Prelude as Prelude

-- | A sample configuration for profile jobs only, which determines the
-- number of rows on which the profile job is run. If a @JobSample@ value
-- isn\'t provided, the default is used. The default value is CUSTOM_ROWS
-- for the mode parameter and 20,000 for the size parameter.
--
-- /See:/ 'newJobSample' smart constructor.
data JobSample = JobSample'
  { -- | The @Size@ parameter is only required when the mode is CUSTOM_ROWS. The
    -- profile job is run on the specified number of rows. The maximum value
    -- for size is Long.MAX_VALUE.
    --
    -- Long.MAX_VALUE = 9223372036854775807
    size :: Prelude.Maybe Prelude.Integer,
    -- | A value that determines whether the profile job is run on the entire
    -- dataset or a specified number of rows. This value must be one of the
    -- following:
    --
    -- -   FULL_DATASET - The profile job is run on the entire dataset.
    --
    -- -   CUSTOM_ROWS - The profile job is run on the number of rows specified
    --     in the @Size@ parameter.
    mode :: Prelude.Maybe SampleMode
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobSample' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'size', 'jobSample_size' - The @Size@ parameter is only required when the mode is CUSTOM_ROWS. The
-- profile job is run on the specified number of rows. The maximum value
-- for size is Long.MAX_VALUE.
--
-- Long.MAX_VALUE = 9223372036854775807
--
-- 'mode', 'jobSample_mode' - A value that determines whether the profile job is run on the entire
-- dataset or a specified number of rows. This value must be one of the
-- following:
--
-- -   FULL_DATASET - The profile job is run on the entire dataset.
--
-- -   CUSTOM_ROWS - The profile job is run on the number of rows specified
--     in the @Size@ parameter.
newJobSample ::
  JobSample
newJobSample =
  JobSample'
    { size = Prelude.Nothing,
      mode = Prelude.Nothing
    }

-- | The @Size@ parameter is only required when the mode is CUSTOM_ROWS. The
-- profile job is run on the specified number of rows. The maximum value
-- for size is Long.MAX_VALUE.
--
-- Long.MAX_VALUE = 9223372036854775807
jobSample_size :: Lens.Lens' JobSample (Prelude.Maybe Prelude.Integer)
jobSample_size = Lens.lens (\JobSample' {size} -> size) (\s@JobSample' {} a -> s {size = a} :: JobSample)

-- | A value that determines whether the profile job is run on the entire
-- dataset or a specified number of rows. This value must be one of the
-- following:
--
-- -   FULL_DATASET - The profile job is run on the entire dataset.
--
-- -   CUSTOM_ROWS - The profile job is run on the number of rows specified
--     in the @Size@ parameter.
jobSample_mode :: Lens.Lens' JobSample (Prelude.Maybe SampleMode)
jobSample_mode = Lens.lens (\JobSample' {mode} -> mode) (\s@JobSample' {} a -> s {mode = a} :: JobSample)

instance Data.FromJSON JobSample where
  parseJSON =
    Data.withObject
      "JobSample"
      ( \x ->
          JobSample'
            Prelude.<$> (x Data..:? "Size") Prelude.<*> (x Data..:? "Mode")
      )

instance Prelude.Hashable JobSample where
  hashWithSalt _salt JobSample' {..} =
    _salt `Prelude.hashWithSalt` size
      `Prelude.hashWithSalt` mode

instance Prelude.NFData JobSample where
  rnf JobSample' {..} =
    Prelude.rnf size `Prelude.seq` Prelude.rnf mode

instance Data.ToJSON JobSample where
  toJSON JobSample' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Size" Data..=) Prelude.<$> size,
            ("Mode" Data..=) Prelude.<$> mode
          ]
      )
