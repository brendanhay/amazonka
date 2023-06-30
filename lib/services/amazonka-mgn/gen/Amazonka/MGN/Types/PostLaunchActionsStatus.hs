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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MGN.Types.PostLaunchActionsStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MGN.Types.JobPostLaunchActionsLaunchStatus
import qualified Amazonka.Prelude as Prelude

-- | Status of the Post Launch Actions running on the Test or Cutover
-- instance.
--
-- /See:/ 'newPostLaunchActionsStatus' smart constructor.
data PostLaunchActionsStatus = PostLaunchActionsStatus'
  { -- | List of Post Launch Action status.
    postLaunchActionsLaunchStatusList :: Prelude.Maybe [JobPostLaunchActionsLaunchStatus],
    -- | Time where the AWS Systems Manager was detected as running on the Test
    -- or Cutover instance.
    ssmAgentDiscoveryDatetime :: Prelude.Maybe Prelude.Text
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
-- 'postLaunchActionsLaunchStatusList', 'postLaunchActionsStatus_postLaunchActionsLaunchStatusList' - List of Post Launch Action status.
--
-- 'ssmAgentDiscoveryDatetime', 'postLaunchActionsStatus_ssmAgentDiscoveryDatetime' - Time where the AWS Systems Manager was detected as running on the Test
-- or Cutover instance.
newPostLaunchActionsStatus ::
  PostLaunchActionsStatus
newPostLaunchActionsStatus =
  PostLaunchActionsStatus'
    { postLaunchActionsLaunchStatusList =
        Prelude.Nothing,
      ssmAgentDiscoveryDatetime = Prelude.Nothing
    }

-- | List of Post Launch Action status.
postLaunchActionsStatus_postLaunchActionsLaunchStatusList :: Lens.Lens' PostLaunchActionsStatus (Prelude.Maybe [JobPostLaunchActionsLaunchStatus])
postLaunchActionsStatus_postLaunchActionsLaunchStatusList = Lens.lens (\PostLaunchActionsStatus' {postLaunchActionsLaunchStatusList} -> postLaunchActionsLaunchStatusList) (\s@PostLaunchActionsStatus' {} a -> s {postLaunchActionsLaunchStatusList = a} :: PostLaunchActionsStatus) Prelude.. Lens.mapping Lens.coerced

-- | Time where the AWS Systems Manager was detected as running on the Test
-- or Cutover instance.
postLaunchActionsStatus_ssmAgentDiscoveryDatetime :: Lens.Lens' PostLaunchActionsStatus (Prelude.Maybe Prelude.Text)
postLaunchActionsStatus_ssmAgentDiscoveryDatetime = Lens.lens (\PostLaunchActionsStatus' {ssmAgentDiscoveryDatetime} -> ssmAgentDiscoveryDatetime) (\s@PostLaunchActionsStatus' {} a -> s {ssmAgentDiscoveryDatetime = a} :: PostLaunchActionsStatus)

instance Data.FromJSON PostLaunchActionsStatus where
  parseJSON =
    Data.withObject
      "PostLaunchActionsStatus"
      ( \x ->
          PostLaunchActionsStatus'
            Prelude.<$> ( x
                            Data..:? "postLaunchActionsLaunchStatusList"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "ssmAgentDiscoveryDatetime")
      )

instance Prelude.Hashable PostLaunchActionsStatus where
  hashWithSalt _salt PostLaunchActionsStatus' {..} =
    _salt
      `Prelude.hashWithSalt` postLaunchActionsLaunchStatusList
      `Prelude.hashWithSalt` ssmAgentDiscoveryDatetime

instance Prelude.NFData PostLaunchActionsStatus where
  rnf PostLaunchActionsStatus' {..} =
    Prelude.rnf postLaunchActionsLaunchStatusList
      `Prelude.seq` Prelude.rnf ssmAgentDiscoveryDatetime
