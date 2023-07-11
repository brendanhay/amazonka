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
-- Module      : Amazonka.MGN.Types.LifeCycleLastTestInitiated
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.LifeCycleLastTestInitiated where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Lifecycle last Test initiated.
--
-- /See:/ 'newLifeCycleLastTestInitiated' smart constructor.
data LifeCycleLastTestInitiated = LifeCycleLastTestInitiated'
  { -- | Lifecycle last Test initiated API call date and time.
    apiCallDateTime :: Prelude.Maybe Prelude.Text,
    -- | Lifecycle last Test initiated Job ID.
    jobID :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LifeCycleLastTestInitiated' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'apiCallDateTime', 'lifeCycleLastTestInitiated_apiCallDateTime' - Lifecycle last Test initiated API call date and time.
--
-- 'jobID', 'lifeCycleLastTestInitiated_jobID' - Lifecycle last Test initiated Job ID.
newLifeCycleLastTestInitiated ::
  LifeCycleLastTestInitiated
newLifeCycleLastTestInitiated =
  LifeCycleLastTestInitiated'
    { apiCallDateTime =
        Prelude.Nothing,
      jobID = Prelude.Nothing
    }

-- | Lifecycle last Test initiated API call date and time.
lifeCycleLastTestInitiated_apiCallDateTime :: Lens.Lens' LifeCycleLastTestInitiated (Prelude.Maybe Prelude.Text)
lifeCycleLastTestInitiated_apiCallDateTime = Lens.lens (\LifeCycleLastTestInitiated' {apiCallDateTime} -> apiCallDateTime) (\s@LifeCycleLastTestInitiated' {} a -> s {apiCallDateTime = a} :: LifeCycleLastTestInitiated)

-- | Lifecycle last Test initiated Job ID.
lifeCycleLastTestInitiated_jobID :: Lens.Lens' LifeCycleLastTestInitiated (Prelude.Maybe Prelude.Text)
lifeCycleLastTestInitiated_jobID = Lens.lens (\LifeCycleLastTestInitiated' {jobID} -> jobID) (\s@LifeCycleLastTestInitiated' {} a -> s {jobID = a} :: LifeCycleLastTestInitiated)

instance Data.FromJSON LifeCycleLastTestInitiated where
  parseJSON =
    Data.withObject
      "LifeCycleLastTestInitiated"
      ( \x ->
          LifeCycleLastTestInitiated'
            Prelude.<$> (x Data..:? "apiCallDateTime")
            Prelude.<*> (x Data..:? "jobID")
      )

instance Prelude.Hashable LifeCycleLastTestInitiated where
  hashWithSalt _salt LifeCycleLastTestInitiated' {..} =
    _salt
      `Prelude.hashWithSalt` apiCallDateTime
      `Prelude.hashWithSalt` jobID

instance Prelude.NFData LifeCycleLastTestInitiated where
  rnf LifeCycleLastTestInitiated' {..} =
    Prelude.rnf apiCallDateTime
      `Prelude.seq` Prelude.rnf jobID
