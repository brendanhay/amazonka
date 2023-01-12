{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudControl.Waiters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudControl.Waiters where

import Amazonka.CloudControl.GetResourceRequestStatus
import Amazonka.CloudControl.Lens
import Amazonka.CloudControl.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.CloudControl.GetResourceRequestStatus' every 5 seconds until a successful state is reached. An error is returned after 24 failed checks.
newResourceRequestSuccess :: Core.Wait GetResourceRequestStatus
newResourceRequestSuccess =
  Core.Wait
    { Core.name = "ResourceRequestSuccess",
      Core.attempts = 24,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchAll
            "SUCCESS"
            Core.AcceptSuccess
            ( getResourceRequestStatusResponse_progressEvent
                Prelude.. Lens._Just
                Prelude.. progressEvent_operationStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( getResourceRequestStatusResponse_progressEvent
                Prelude.. Lens._Just
                Prelude.. progressEvent_operationStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "CANCEL_COMPLETE"
            Core.AcceptFailure
            ( getResourceRequestStatusResponse_progressEvent
                Prelude.. Lens._Just
                Prelude.. progressEvent_operationStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }
