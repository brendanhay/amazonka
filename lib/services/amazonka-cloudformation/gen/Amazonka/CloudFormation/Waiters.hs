{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.CloudFormation.Waiters
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudFormation.Waiters where

import Amazonka.CloudFormation.DescribeChangeSet
import Amazonka.CloudFormation.DescribeStacks
import Amazonka.CloudFormation.DescribeTypeRegistration
import Amazonka.CloudFormation.Lens
import Amazonka.CloudFormation.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Polls 'Amazonka.CloudFormation.DescribeChangeSet' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newChangeSetCreateComplete :: Core.Wait DescribeChangeSet
newChangeSetCreateComplete =
  Core.Wait
    { Core.name = "ChangeSetCreateComplete",
      Core.attempts = 120,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "CREATE_COMPLETE"
            Core.AcceptSuccess
            ( describeChangeSetResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( describeChangeSetResponse_status
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "ValidationError"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Amazonka.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newStackCreateComplete :: Core.Wait DescribeStacks
newStackCreateComplete =
  Core.Wait
    { Core.name = "StackCreateComplete",
      Core.attempts = 120,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "CREATE_COMPLETE"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "CREATE_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "DELETE_COMPLETE"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "DELETE_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "ROLLBACK_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "ROLLBACK_COMPLETE"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "ValidationError"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Amazonka.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newStackDeleteComplete :: Core.Wait DescribeStacks
newStackDeleteComplete =
  Core.Wait
    { Core.name = "StackDeleteComplete",
      Core.attempts = 120,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "DELETE_COMPLETE"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError "ValidationError" Core.AcceptSuccess,
          Core.matchAny
            "DELETE_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "CREATE_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "ROLLBACK_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "UPDATE_ROLLBACK_IN_PROGRESS"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "UPDATE_ROLLBACK_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "UPDATE_ROLLBACK_COMPLETE"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "UPDATE_COMPLETE"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }

-- | Polls 'Amazonka.CloudFormation.DescribeStacks' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newStackExists :: Core.Wait DescribeStacks
newStackExists =
  Core.Wait
    { Core.name = "StackExists",
      Core.attempts = 20,
      Core.delay = 5,
      Core.acceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchError "ValidationError" Core.AcceptRetry
        ]
    }

-- | Polls 'Amazonka.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newStackImportComplete :: Core.Wait DescribeStacks
newStackImportComplete =
  Core.Wait
    { Core.name = "StackImportComplete",
      Core.attempts = 120,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "IMPORT_COMPLETE"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "ROLLBACK_COMPLETE"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "ROLLBACK_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "IMPORT_ROLLBACK_IN_PROGRESS"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "IMPORT_ROLLBACK_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "IMPORT_ROLLBACK_COMPLETE"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "ValidationError"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Amazonka.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newStackRollbackComplete :: Core.Wait DescribeStacks
newStackRollbackComplete =
  Core.Wait
    { Core.name = "StackRollbackComplete",
      Core.attempts = 120,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "UPDATE_ROLLBACK_COMPLETE"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "UPDATE_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "UPDATE_ROLLBACK_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "DELETE_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "ValidationError"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Amazonka.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newStackUpdateComplete :: Core.Wait DescribeStacks
newStackUpdateComplete =
  Core.Wait
    { Core.name = "StackUpdateComplete",
      Core.attempts = 120,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "UPDATE_COMPLETE"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "UPDATE_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "UPDATE_ROLLBACK_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAny
            "UPDATE_ROLLBACK_COMPLETE"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchError
            "ValidationError"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Amazonka.CloudFormation.DescribeTypeRegistration' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newTypeRegistrationComplete :: Core.Wait DescribeTypeRegistration
newTypeRegistrationComplete =
  Core.Wait
    { Core.name = "TypeRegistrationComplete",
      Core.attempts = 120,
      Core.delay = 30,
      Core.acceptors =
        [ Core.matchAll
            "COMPLETE"
            Core.AcceptSuccess
            ( describeTypeRegistrationResponse_progressStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( describeTypeRegistrationResponse_progressStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Data.toTextCI
            )
        ]
    }
