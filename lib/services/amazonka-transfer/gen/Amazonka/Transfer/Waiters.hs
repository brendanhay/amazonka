{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Transfer.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.DescribeServer
import Amazonka.Transfer.Lens
import Amazonka.Transfer.Types

-- | Polls 'Amazonka.Transfer.DescribeServer' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newServerOnline :: Core.Wait DescribeServer
newServerOnline =
  Core.Wait
    { Core.name = "ServerOnline",
      Core.attempts = 120,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "ONLINE"
            Core.AcceptSuccess
            ( describeServerResponse_server
                Prelude.. describedServer_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "START_FAILED"
            Core.AcceptFailure
            ( describeServerResponse_server
                Prelude.. describedServer_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.Transfer.DescribeServer' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newServerOffline :: Core.Wait DescribeServer
newServerOffline =
  Core.Wait
    { Core.name = "ServerOffline",
      Core.attempts = 120,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "OFFLINE"
            Core.AcceptSuccess
            ( describeServerResponse_server
                Prelude.. describedServer_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "STOP_FAILED"
            Core.AcceptFailure
            ( describeServerResponse_server
                Prelude.. describedServer_state
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }
