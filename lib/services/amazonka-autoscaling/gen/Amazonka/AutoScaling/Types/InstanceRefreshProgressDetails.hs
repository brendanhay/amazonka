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
-- Module      : Amazonka.AutoScaling.Types.InstanceRefreshProgressDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AutoScaling.Types.InstanceRefreshProgressDetails where

import Amazonka.AutoScaling.Types.InstanceRefreshLivePoolProgress
import Amazonka.AutoScaling.Types.InstanceRefreshWarmPoolProgress
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Reports the progress of an instance refresh on an Auto Scaling group
-- that has a warm pool. This includes separate details for instances in
-- the warm pool and instances in the Auto Scaling group (the live pool).
--
-- /See:/ 'newInstanceRefreshProgressDetails' smart constructor.
data InstanceRefreshProgressDetails = InstanceRefreshProgressDetails'
  { -- | Indicates the progress of an instance refresh on instances that are in
    -- the warm pool.
    warmPoolProgress :: Prelude.Maybe InstanceRefreshWarmPoolProgress,
    -- | Indicates the progress of an instance refresh on instances that are in
    -- the Auto Scaling group.
    livePoolProgress :: Prelude.Maybe InstanceRefreshLivePoolProgress
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceRefreshProgressDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'warmPoolProgress', 'instanceRefreshProgressDetails_warmPoolProgress' - Indicates the progress of an instance refresh on instances that are in
-- the warm pool.
--
-- 'livePoolProgress', 'instanceRefreshProgressDetails_livePoolProgress' - Indicates the progress of an instance refresh on instances that are in
-- the Auto Scaling group.
newInstanceRefreshProgressDetails ::
  InstanceRefreshProgressDetails
newInstanceRefreshProgressDetails =
  InstanceRefreshProgressDetails'
    { warmPoolProgress =
        Prelude.Nothing,
      livePoolProgress = Prelude.Nothing
    }

-- | Indicates the progress of an instance refresh on instances that are in
-- the warm pool.
instanceRefreshProgressDetails_warmPoolProgress :: Lens.Lens' InstanceRefreshProgressDetails (Prelude.Maybe InstanceRefreshWarmPoolProgress)
instanceRefreshProgressDetails_warmPoolProgress = Lens.lens (\InstanceRefreshProgressDetails' {warmPoolProgress} -> warmPoolProgress) (\s@InstanceRefreshProgressDetails' {} a -> s {warmPoolProgress = a} :: InstanceRefreshProgressDetails)

-- | Indicates the progress of an instance refresh on instances that are in
-- the Auto Scaling group.
instanceRefreshProgressDetails_livePoolProgress :: Lens.Lens' InstanceRefreshProgressDetails (Prelude.Maybe InstanceRefreshLivePoolProgress)
instanceRefreshProgressDetails_livePoolProgress = Lens.lens (\InstanceRefreshProgressDetails' {livePoolProgress} -> livePoolProgress) (\s@InstanceRefreshProgressDetails' {} a -> s {livePoolProgress = a} :: InstanceRefreshProgressDetails)

instance Core.FromXML InstanceRefreshProgressDetails where
  parseXML x =
    InstanceRefreshProgressDetails'
      Prelude.<$> (x Core..@? "WarmPoolProgress")
      Prelude.<*> (x Core..@? "LivePoolProgress")

instance
  Prelude.Hashable
    InstanceRefreshProgressDetails
  where
  hashWithSalt
    _salt
    InstanceRefreshProgressDetails' {..} =
      _salt `Prelude.hashWithSalt` warmPoolProgress
        `Prelude.hashWithSalt` livePoolProgress

instance
  Prelude.NFData
    InstanceRefreshProgressDetails
  where
  rnf InstanceRefreshProgressDetails' {..} =
    Prelude.rnf warmPoolProgress
      `Prelude.seq` Prelude.rnf livePoolProgress
