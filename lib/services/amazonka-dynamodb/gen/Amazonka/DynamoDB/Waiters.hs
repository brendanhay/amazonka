{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.DynamoDB.Waiters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DynamoDB.Waiters where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DynamoDB.DescribeTable
import Amazonka.DynamoDB.Lens
import Amazonka.DynamoDB.Types
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.DynamoDB.DescribeTable' every 20 seconds until a successful state is reached. An error is returned after 25 failed checks.
newTableExists :: Core.Wait DescribeTable
newTableExists =
  Core.Wait
    { Core.name = "TableExists",
      Core.attempts = 25,
      Core.delay = 20,
      Core.acceptors =
        [ Core.matchAll
            "ACTIVE"
            Core.AcceptSuccess
            ( describeTableResponse_table
                Prelude.. Lens._Just
                Prelude.. tableDescription_tableStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "ResourceNotFoundException"
            Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.DynamoDB.DescribeTable' every 20 seconds until a successful state is reached. An error is returned after 25 failed checks.
newTableNotExists :: Core.Wait DescribeTable
newTableNotExists =
  Core.Wait
    { Core.name = "TableNotExists",
      Core.attempts = 25,
      Core.delay = 20,
      Core.acceptors =
        [ Core.matchError
            "ResourceNotFoundException"
            Core.AcceptSuccess
        ]
    }
