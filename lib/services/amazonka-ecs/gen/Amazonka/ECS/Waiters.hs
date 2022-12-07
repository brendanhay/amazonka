{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ECS.Waiters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECS.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ECS.DescribeServices
import Amazonka.ECS.DescribeTasks
import Amazonka.ECS.Lens
import Amazonka.ECS.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.ECS.DescribeServices' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
newServicesInactive :: Core.Wait DescribeServices
newServicesInactive =
  Core.Wait
    { Core.name = "ServicesInactive",
      Core.attempts = 40,
      Core.delay = 15,
      Core.acceptors =
        [ Core.matchAny
            "MISSING"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeServicesResponse_failures
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. failure_reason
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "INACTIVE"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeServicesResponse_services
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. containerService_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.ECS.DescribeTasks' every 6 seconds until a successful state is reached. An error is returned after 100 failed checks.
newTasksRunning :: Core.Wait DescribeTasks
newTasksRunning =
  Core.Wait
    { Core.name = "TasksRunning",
      Core.attempts = 100,
      Core.delay = 6,
      Core.acceptors =
        [ Core.matchAny
            "STOPPED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeTasksResponse_tasks Prelude.. Lens._Just)
                )
                Prelude.. task_lastStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "MISSING"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeTasksResponse_failures
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. failure_reason
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "RUNNING"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeTasksResponse_tasks Prelude.. Lens._Just)
                )
                Prelude.. task_lastStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.ECS.DescribeTasks' every 6 seconds until a successful state is reached. An error is returned after 100 failed checks.
newTasksStopped :: Core.Wait DescribeTasks
newTasksStopped =
  Core.Wait
    { Core.name = "TasksStopped",
      Core.attempts = 100,
      Core.delay = 6,
      Core.acceptors =
        [ Core.matchAll
            "STOPPED"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeTasksResponse_tasks Prelude.. Lens._Just)
                )
                Prelude.. task_lastStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }
