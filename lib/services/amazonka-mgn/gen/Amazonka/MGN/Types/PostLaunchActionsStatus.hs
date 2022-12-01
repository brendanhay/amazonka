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
-- Module      : Amazonka.MGN.Types.PostLaunchActionsStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.PostLaunchActionsStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MGN.Types.JobPostLaunchActionsLaunchStatus
import qualified Amazonka.Prelude as Prelude

-- | Server participating in Job.
--
-- /See:/ 'newPostLaunchActionsStatus' smart constructor.
data PostLaunchActionsStatus = PostLaunchActionsStatus'
  { -- | Server participating in Job.
    ssmAgentDiscoveryDatetime :: Prelude.Maybe Prelude.Text,
    -- | Server participating in Job.
    postLaunchActionsLaunchStatusList :: Prelude.Maybe [JobPostLaunchActionsLaunchStatus]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PostLaunchActionsStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ssmAgentDiscoveryDatetime', 'postLaunchActionsStatus_ssmAgentDiscoveryDatetime' - Server participating in Job.
--
-- 'postLaunchActionsLaunchStatusList', 'postLaunchActionsStatus_postLaunchActionsLaunchStatusList' - Server participating in Job.
newPostLaunchActionsStatus ::
  PostLaunchActionsStatus
newPostLaunchActionsStatus =
  PostLaunchActionsStatus'
    { ssmAgentDiscoveryDatetime =
        Prelude.Nothing,
      postLaunchActionsLaunchStatusList =
        Prelude.Nothing
    }

-- | Server participating in Job.
postLaunchActionsStatus_ssmAgentDiscoveryDatetime :: Lens.Lens' PostLaunchActionsStatus (Prelude.Maybe Prelude.Text)
postLaunchActionsStatus_ssmAgentDiscoveryDatetime = Lens.lens (\PostLaunchActionsStatus' {ssmAgentDiscoveryDatetime} -> ssmAgentDiscoveryDatetime) (\s@PostLaunchActionsStatus' {} a -> s {ssmAgentDiscoveryDatetime = a} :: PostLaunchActionsStatus)

-- | Server participating in Job.
postLaunchActionsStatus_postLaunchActionsLaunchStatusList :: Lens.Lens' PostLaunchActionsStatus (Prelude.Maybe [JobPostLaunchActionsLaunchStatus])
postLaunchActionsStatus_postLaunchActionsLaunchStatusList = Lens.lens (\PostLaunchActionsStatus' {postLaunchActionsLaunchStatusList} -> postLaunchActionsLaunchStatusList) (\s@PostLaunchActionsStatus' {} a -> s {postLaunchActionsLaunchStatusList = a} :: PostLaunchActionsStatus) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON PostLaunchActionsStatus where
  parseJSON =
    Core.withObject
      "PostLaunchActionsStatus"
      ( \x ->
          PostLaunchActionsStatus'
            Prelude.<$> (x Core..:? "ssmAgentDiscoveryDatetime")
            Prelude.<*> ( x Core..:? "postLaunchActionsLaunchStatusList"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable PostLaunchActionsStatus where
  hashWithSalt _salt PostLaunchActionsStatus' {..} =
    _salt
      `Prelude.hashWithSalt` ssmAgentDiscoveryDatetime
      `Prelude.hashWithSalt` postLaunchActionsLaunchStatusList

instance Prelude.NFData PostLaunchActionsStatus where
  rnf PostLaunchActionsStatus' {..} =
    Prelude.rnf ssmAgentDiscoveryDatetime
      `Prelude.seq` Prelude.rnf postLaunchActionsLaunchStatusList
