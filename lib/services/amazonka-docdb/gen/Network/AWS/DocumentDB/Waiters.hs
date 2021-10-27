{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DocumentDB.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DocumentDB.Waiters where

import qualified Network.AWS.Core as Core
import Network.AWS.DocumentDB.DescribeDBInstances
import Network.AWS.DocumentDB.Lens
import Network.AWS.DocumentDB.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.DocumentDB.DescribeDBInstances' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBInstanceAvailable :: Core.Wait DescribeDBInstances
newDBInstanceAvailable =
  Core.Wait
    { Core._waitName = "DBInstanceAvailable",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "available"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleted"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "deleting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "failed"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "incompatible-restore"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "incompatible-parameters"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.DocumentDB.DescribeDBInstances' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
newDBInstanceDeleted :: Core.Wait DescribeDBInstances
newDBInstanceDeleted =
  Core.Wait
    { Core._waitName = "DBInstanceDeleted",
      Core._waitAttempts = 60,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "deleted"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "DBInstanceNotFound"
            Core.AcceptSuccess,
          Core.matchAny
            "creating"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "modifying"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "rebooting"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "resetting-master-credentials"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    ( describeDBInstancesResponse_dbInstances
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. dbInstance_dbInstanceStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }
