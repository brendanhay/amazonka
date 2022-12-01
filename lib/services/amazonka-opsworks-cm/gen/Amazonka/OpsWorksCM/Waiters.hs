{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.OpsWorksCM.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.OpsWorksCM.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.OpsWorksCM.DescribeNodeAssociationStatus
import Amazonka.OpsWorksCM.Lens
import Amazonka.OpsWorksCM.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.OpsWorksCM.DescribeNodeAssociationStatus' every 15 seconds until a successful state is reached. An error is returned after 15 failed checks.
newNodeAssociated :: Core.Wait DescribeNodeAssociationStatus
newNodeAssociated =
  Core.Wait
    { Core.name = "NodeAssociated",
      Core.attempts = 15,
      Core.delay = 15,
      Core.acceptors =
        [ Core.matchAll
            "SUCCESS"
            Core.AcceptSuccess
            ( describeNodeAssociationStatusResponse_nodeAssociationStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( describeNodeAssociationStatusResponse_nodeAssociationStatus
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }
