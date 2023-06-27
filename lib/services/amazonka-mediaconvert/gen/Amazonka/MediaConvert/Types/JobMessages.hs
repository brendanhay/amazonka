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
-- Module      : Amazonka.MediaConvert.Types.JobMessages
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MediaConvert.Types.JobMessages where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides messages from the service about jobs that you have already
-- successfully submitted.
--
-- /See:/ 'newJobMessages' smart constructor.
data JobMessages = JobMessages'
  { -- | List of messages that are informational only and don\'t indicate a
    -- problem with your job.
    info :: Prelude.Maybe [Prelude.Text],
    -- | List of messages that warn about conditions that might cause your job
    -- not to run or to fail.
    warning :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'JobMessages' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'info', 'jobMessages_info' - List of messages that are informational only and don\'t indicate a
-- problem with your job.
--
-- 'warning', 'jobMessages_warning' - List of messages that warn about conditions that might cause your job
-- not to run or to fail.
newJobMessages ::
  JobMessages
newJobMessages =
  JobMessages'
    { info = Prelude.Nothing,
      warning = Prelude.Nothing
    }

-- | List of messages that are informational only and don\'t indicate a
-- problem with your job.
jobMessages_info :: Lens.Lens' JobMessages (Prelude.Maybe [Prelude.Text])
jobMessages_info = Lens.lens (\JobMessages' {info} -> info) (\s@JobMessages' {} a -> s {info = a} :: JobMessages) Prelude.. Lens.mapping Lens.coerced

-- | List of messages that warn about conditions that might cause your job
-- not to run or to fail.
jobMessages_warning :: Lens.Lens' JobMessages (Prelude.Maybe [Prelude.Text])
jobMessages_warning = Lens.lens (\JobMessages' {warning} -> warning) (\s@JobMessages' {} a -> s {warning = a} :: JobMessages) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON JobMessages where
  parseJSON =
    Data.withObject
      "JobMessages"
      ( \x ->
          JobMessages'
            Prelude.<$> (x Data..:? "info" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "warning" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable JobMessages where
  hashWithSalt _salt JobMessages' {..} =
    _salt
      `Prelude.hashWithSalt` info
      `Prelude.hashWithSalt` warning

instance Prelude.NFData JobMessages where
  rnf JobMessages' {..} =
    Prelude.rnf info `Prelude.seq` Prelude.rnf warning
