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
-- Module      : Network.AWS.MediaConvert.Types.JobMessages
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MediaConvert.Types.JobMessages where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
jobMessages_info = Lens.lens (\JobMessages' {info} -> info) (\s@JobMessages' {} a -> s {info = a} :: JobMessages) Prelude.. Lens.mapping Prelude._Coerce

-- | List of messages that warn about conditions that might cause your job
-- not to run or to fail.
jobMessages_warning :: Lens.Lens' JobMessages (Prelude.Maybe [Prelude.Text])
jobMessages_warning = Lens.lens (\JobMessages' {warning} -> warning) (\s@JobMessages' {} a -> s {warning = a} :: JobMessages) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.FromJSON JobMessages where
  parseJSON =
    Prelude.withObject
      "JobMessages"
      ( \x ->
          JobMessages'
            Prelude.<$> (x Prelude..:? "info" Prelude..!= Prelude.mempty)
            Prelude.<*> (x Prelude..:? "warning" Prelude..!= Prelude.mempty)
      )

instance Prelude.Hashable JobMessages

instance Prelude.NFData JobMessages
