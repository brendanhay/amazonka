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
-- Module      : Amazonka.DrS.Types.LifeCycleLastLaunchInitiated
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DrS.Types.LifeCycleLastLaunchInitiated where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DrS.Types.LastLaunchType
import qualified Amazonka.Prelude as Prelude

-- | An object containing information regarding the initiation of the last
-- launch of a Source Server.
--
-- /See:/ 'newLifeCycleLastLaunchInitiated' smart constructor.
data LifeCycleLastLaunchInitiated = LifeCycleLastLaunchInitiated'
  { -- | The date and time the last Source Server launch was initiated.
    apiCallDateTime :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Job that was used to last launch the Source Server.
    jobID :: Prelude.Maybe Prelude.Text,
    -- | The Job type that was used to last launch the Source Server.
    type' :: Prelude.Maybe LastLaunchType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifeCycleLastLaunchInitiated' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiCallDateTime', 'lifeCycleLastLaunchInitiated_apiCallDateTime' - The date and time the last Source Server launch was initiated.
--
-- 'jobID', 'lifeCycleLastLaunchInitiated_jobID' - The ID of the Job that was used to last launch the Source Server.
--
-- 'type'', 'lifeCycleLastLaunchInitiated_type' - The Job type that was used to last launch the Source Server.
newLifeCycleLastLaunchInitiated ::
  LifeCycleLastLaunchInitiated
newLifeCycleLastLaunchInitiated =
  LifeCycleLastLaunchInitiated'
    { apiCallDateTime =
        Prelude.Nothing,
      jobID = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The date and time the last Source Server launch was initiated.
lifeCycleLastLaunchInitiated_apiCallDateTime :: Lens.Lens' LifeCycleLastLaunchInitiated (Prelude.Maybe Prelude.Text)
lifeCycleLastLaunchInitiated_apiCallDateTime = Lens.lens (\LifeCycleLastLaunchInitiated' {apiCallDateTime} -> apiCallDateTime) (\s@LifeCycleLastLaunchInitiated' {} a -> s {apiCallDateTime = a} :: LifeCycleLastLaunchInitiated)

-- | The ID of the Job that was used to last launch the Source Server.
lifeCycleLastLaunchInitiated_jobID :: Lens.Lens' LifeCycleLastLaunchInitiated (Prelude.Maybe Prelude.Text)
lifeCycleLastLaunchInitiated_jobID = Lens.lens (\LifeCycleLastLaunchInitiated' {jobID} -> jobID) (\s@LifeCycleLastLaunchInitiated' {} a -> s {jobID = a} :: LifeCycleLastLaunchInitiated)

-- | The Job type that was used to last launch the Source Server.
lifeCycleLastLaunchInitiated_type :: Lens.Lens' LifeCycleLastLaunchInitiated (Prelude.Maybe LastLaunchType)
lifeCycleLastLaunchInitiated_type = Lens.lens (\LifeCycleLastLaunchInitiated' {type'} -> type') (\s@LifeCycleLastLaunchInitiated' {} a -> s {type' = a} :: LifeCycleLastLaunchInitiated)

instance Data.FromJSON LifeCycleLastLaunchInitiated where
  parseJSON =
    Data.withObject
      "LifeCycleLastLaunchInitiated"
      ( \x ->
          LifeCycleLastLaunchInitiated'
            Prelude.<$> (x Data..:? "apiCallDateTime")
            Prelude.<*> (x Data..:? "jobID")
            Prelude.<*> (x Data..:? "type")
      )

instance
  Prelude.Hashable
    LifeCycleLastLaunchInitiated
  where
  hashWithSalt _salt LifeCycleLastLaunchInitiated' {..} =
    _salt
      `Prelude.hashWithSalt` apiCallDateTime
      `Prelude.hashWithSalt` jobID
      `Prelude.hashWithSalt` type'

instance Prelude.NFData LifeCycleLastLaunchInitiated where
  rnf LifeCycleLastLaunchInitiated' {..} =
    Prelude.rnf apiCallDateTime `Prelude.seq`
      Prelude.rnf jobID `Prelude.seq`
        Prelude.rnf type'
