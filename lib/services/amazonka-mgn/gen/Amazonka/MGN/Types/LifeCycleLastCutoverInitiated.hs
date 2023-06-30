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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.LifeCycleLastCutoverInitiated where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Lifecycle last Cutover initiated.
--
-- /See:/ 'newLifeCycleLastCutoverInitiated' smart constructor.
data LifeCycleLastCutoverInitiated = LifeCycleLastCutoverInitiated'
  { apiCallDateTime :: Prelude.Maybe Prelude.Text,
    -- | Lifecycle last Cutover initiated by Job ID.
    jobID :: Prelude.Maybe Prelude.Text
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
-- 'apiCallDateTime', 'lifeCycleLastCutoverInitiated_apiCallDateTime' -
--
-- 'jobID', 'lifeCycleLastCutoverInitiated_jobID' - Lifecycle last Cutover initiated by Job ID.
newLifeCycleLastCutoverInitiated ::
  LifeCycleLastCutoverInitiated
newLifeCycleLastCutoverInitiated =
  LifeCycleLastCutoverInitiated'
    { apiCallDateTime =
        Prelude.Nothing,
      jobID = Prelude.Nothing
    }

lifeCycleLastCutoverInitiated_apiCallDateTime :: Lens.Lens' LifeCycleLastCutoverInitiated (Prelude.Maybe Prelude.Text)
lifeCycleLastCutoverInitiated_apiCallDateTime = Lens.lens (\LifeCycleLastCutoverInitiated' {apiCallDateTime} -> apiCallDateTime) (\s@LifeCycleLastCutoverInitiated' {} a -> s {apiCallDateTime = a} :: LifeCycleLastCutoverInitiated)

-- | Lifecycle last Cutover initiated by Job ID.
lifeCycleLastCutoverInitiated_jobID :: Lens.Lens' LifeCycleLastCutoverInitiated (Prelude.Maybe Prelude.Text)
lifeCycleLastCutoverInitiated_jobID = Lens.lens (\LifeCycleLastCutoverInitiated' {jobID} -> jobID) (\s@LifeCycleLastCutoverInitiated' {} a -> s {jobID = a} :: LifeCycleLastCutoverInitiated)

instance Data.FromJSON LifeCycleLastCutoverInitiated where
  parseJSON =
    Data.withObject
      "LifeCycleLastCutoverInitiated"
      ( \x ->
          LifeCycleLastCutoverInitiated'
            Prelude.<$> (x Data..:? "apiCallDateTime")
            Prelude.<*> (x Data..:? "jobID")
      )

instance
  Prelude.Hashable
    LifeCycleLastCutoverInitiated
  where
  hashWithSalt _salt LifeCycleLastCutoverInitiated' {..} =
    _salt
      `Prelude.hashWithSalt` apiCallDateTime
      `Prelude.hashWithSalt` jobID

instance Prelude.NFData LifeCycleLastCutoverInitiated where
  rnf LifeCycleLastCutoverInitiated' {..} =
    Prelude.rnf apiCallDateTime
      `Prelude.seq` Prelude.rnf jobID
