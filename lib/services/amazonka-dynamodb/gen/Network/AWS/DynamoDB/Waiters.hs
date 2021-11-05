{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DynamoDB.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Waiters where

import qualified Amazonka.Core as Core
import Amazonka.DynamoDB.DescribeTable
import Amazonka.DynamoDB.Lens
import Amazonka.DynamoDB.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.DynamoDB.DescribeTable' every 20 seconds until a successful state is reached. An error is returned after 25 failed checks.
newTableNotExists :: Core.Wait DescribeTable
newTableNotExists =
  Core.Wait
    { Core._waitName = "TableNotExists",
      Core._waitAttempts = 25,
      Core._waitDelay = 20,
      Core._waitAcceptors =
        [ Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess
        ]
    }

-- | Polls 'Amazonka.DynamoDB.DescribeTable' every 20 seconds until a successful state is reached. An error is returned after 25 failed checks.
newTableExists :: Core.Wait DescribeTable
newTableExists =
  Core.Wait
    { Core._waitName = "TableExists",
      Core._waitAttempts = 25,
      Core._waitDelay = 20,
      Core._waitAcceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( describeTableResponse_table Prelude.. Lens._Just
                Prelude.. tableDescription_tableStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ResourceNotFoundException"
            Core.AcceptRetry
        ]
    }
