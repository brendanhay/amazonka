{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GroundStation.Waiters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GroundStation.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GroundStation.DescribeContact
import Amazonka.GroundStation.Lens
import Amazonka.GroundStation.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.GroundStation.DescribeContact' every 5 seconds until a successful state is reached. An error is returned after 180 failed checks.
newContactScheduled :: Core.Wait DescribeContact
newContactScheduled =
  Core.Wait
    { Core.name = "ContactScheduled",
      Core.attempts = 180,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "FAILED_TO_SCHEDULE"
            Core.AcceptFailure
            ( describeContactResponse_contactStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "SCHEDULED"
            Core.AcceptSuccess
            ( describeContactResponse_contactStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }
