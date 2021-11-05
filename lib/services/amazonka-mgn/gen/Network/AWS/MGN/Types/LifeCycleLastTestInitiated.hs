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
-- Module      : Network.AWS.MGN.Types.LifeCycleLastTestInitiated
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.MGN.Types.LifeCycleLastTestInitiated where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Lifecycle last Test initiated.
--
-- /See:/ 'newLifeCycleLastTestInitiated' smart constructor.
data LifeCycleLastTestInitiated = LifeCycleLastTestInitiated'
  { -- | Lifecycle last Test initiated Job ID.
    jobID :: Prelude.Maybe Prelude.Text,
    -- | Lifecycle last Test initiated API call date and time.
    apiCallDateTime :: Prelude.Maybe Prelude.Text
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
-- 'jobID', 'lifeCycleLastTestInitiated_jobID' - Lifecycle last Test initiated Job ID.
--
-- 'apiCallDateTime', 'lifeCycleLastTestInitiated_apiCallDateTime' - Lifecycle last Test initiated API call date and time.
newLifeCycleLastTestInitiated ::
  LifeCycleLastTestInitiated
newLifeCycleLastTestInitiated =
  LifeCycleLastTestInitiated'
    { jobID =
        Prelude.Nothing,
      apiCallDateTime = Prelude.Nothing
    }

-- | Lifecycle last Test initiated Job ID.
lifeCycleLastTestInitiated_jobID :: Lens.Lens' LifeCycleLastTestInitiated (Prelude.Maybe Prelude.Text)
lifeCycleLastTestInitiated_jobID = Lens.lens (\LifeCycleLastTestInitiated' {jobID} -> jobID) (\s@LifeCycleLastTestInitiated' {} a -> s {jobID = a} :: LifeCycleLastTestInitiated)

-- | Lifecycle last Test initiated API call date and time.
lifeCycleLastTestInitiated_apiCallDateTime :: Lens.Lens' LifeCycleLastTestInitiated (Prelude.Maybe Prelude.Text)
lifeCycleLastTestInitiated_apiCallDateTime = Lens.lens (\LifeCycleLastTestInitiated' {apiCallDateTime} -> apiCallDateTime) (\s@LifeCycleLastTestInitiated' {} a -> s {apiCallDateTime = a} :: LifeCycleLastTestInitiated)

instance Core.FromJSON LifeCycleLastTestInitiated where
  parseJSON =
    Core.withObject
      "LifeCycleLastTestInitiated"
      ( \x ->
          LifeCycleLastTestInitiated'
            Prelude.<$> (x Core..:? "jobID")
            Prelude.<*> (x Core..:? "apiCallDateTime")
      )

instance Prelude.Hashable LifeCycleLastTestInitiated

instance Prelude.NFData LifeCycleLastTestInitiated
