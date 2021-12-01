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
-- Module      : Amazonka.MGN.Types.LifeCycleLastCutoverInitiated
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.LifeCycleLastCutoverInitiated where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Lifecycle last Cutover initiated.
--
-- /See:/ 'newLifeCycleLastCutoverInitiated' smart constructor.
data LifeCycleLastCutoverInitiated = LifeCycleLastCutoverInitiated'
  { -- | Lifecycle last Cutover initiated by Job ID.
    jobID :: Prelude.Maybe Prelude.Text,
    apiCallDateTime :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifeCycleLastCutoverInitiated' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'jobID', 'lifeCycleLastCutoverInitiated_jobID' - Lifecycle last Cutover initiated by Job ID.
--
-- 'apiCallDateTime', 'lifeCycleLastCutoverInitiated_apiCallDateTime' -
newLifeCycleLastCutoverInitiated ::
  LifeCycleLastCutoverInitiated
newLifeCycleLastCutoverInitiated =
  LifeCycleLastCutoverInitiated'
    { jobID =
        Prelude.Nothing,
      apiCallDateTime = Prelude.Nothing
    }

-- | Lifecycle last Cutover initiated by Job ID.
lifeCycleLastCutoverInitiated_jobID :: Lens.Lens' LifeCycleLastCutoverInitiated (Prelude.Maybe Prelude.Text)
lifeCycleLastCutoverInitiated_jobID = Lens.lens (\LifeCycleLastCutoverInitiated' {jobID} -> jobID) (\s@LifeCycleLastCutoverInitiated' {} a -> s {jobID = a} :: LifeCycleLastCutoverInitiated)

-- |
lifeCycleLastCutoverInitiated_apiCallDateTime :: Lens.Lens' LifeCycleLastCutoverInitiated (Prelude.Maybe Prelude.Text)
lifeCycleLastCutoverInitiated_apiCallDateTime = Lens.lens (\LifeCycleLastCutoverInitiated' {apiCallDateTime} -> apiCallDateTime) (\s@LifeCycleLastCutoverInitiated' {} a -> s {apiCallDateTime = a} :: LifeCycleLastCutoverInitiated)

instance Core.FromJSON LifeCycleLastCutoverInitiated where
  parseJSON =
    Core.withObject
      "LifeCycleLastCutoverInitiated"
      ( \x ->
          LifeCycleLastCutoverInitiated'
            Prelude.<$> (x Core..:? "jobID")
            Prelude.<*> (x Core..:? "apiCallDateTime")
      )

instance
  Prelude.Hashable
    LifeCycleLastCutoverInitiated
  where
  hashWithSalt salt' LifeCycleLastCutoverInitiated' {..} =
    salt' `Prelude.hashWithSalt` apiCallDateTime
      `Prelude.hashWithSalt` jobID

instance Prelude.NFData LifeCycleLastCutoverInitiated where
  rnf LifeCycleLastCutoverInitiated' {..} =
    Prelude.rnf jobID
      `Prelude.seq` Prelude.rnf apiCallDateTime
