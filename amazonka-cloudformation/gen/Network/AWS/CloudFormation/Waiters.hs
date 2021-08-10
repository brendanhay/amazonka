{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Waiters where

import Network.AWS.CloudFormation.DescribeChangeSet
import Network.AWS.CloudFormation.DescribeStacks
import Network.AWS.CloudFormation.DescribeTypeRegistration
import Network.AWS.CloudFormation.Lens
import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Polls 'Network.AWS.CloudFormation.DescribeChangeSet' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newChangeSetCreateComplete :: Core.Wait DescribeChangeSet
newChangeSetCreateComplete =
  Core.Wait
    { Core._waitName =
        "ChangeSetCreateComplete",
      Core._waitAttempts = 120,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "CREATE_COMPLETE"
            Core.AcceptSuccess
            ( describeChangeSetResponse_status
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( describeChangeSetResponse_status
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ValidationError"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newStackRollbackComplete :: Core.Wait DescribeStacks
newStackRollbackComplete =
  Core.Wait
    { Core._waitName = "StackRollbackComplete",
      Core._waitAttempts = 120,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "UPDATE_ROLLBACK_COMPLETE"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "UPDATE_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "UPDATE_ROLLBACK_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "DELETE_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ValidationError"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newStackDeleteComplete :: Core.Wait DescribeStacks
newStackDeleteComplete =
  Core.Wait
    { Core._waitName = "StackDeleteComplete",
      Core._waitAttempts = 120,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "DELETE_COMPLETE"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
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
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "CREATE_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "ROLLBACK_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "UPDATE_ROLLBACK_IN_PROGRESS"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "UPDATE_ROLLBACK_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "UPDATE_ROLLBACK_COMPLETE"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newStackExists :: Core.Wait DescribeStacks
newStackExists =
  Core.Wait
    { Core._waitName = "StackExists",
      Core._waitAttempts = 20,
      Core._waitDelay = 5,
      Core._waitAcceptors =
        [ Core.matchStatus 200 Core.AcceptSuccess,
          Core.matchError "ValidationError" Core.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newStackUpdateComplete :: Core.Wait DescribeStacks
newStackUpdateComplete =
  Core.Wait
    { Core._waitName = "StackUpdateComplete",
      Core._waitAttempts = 120,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "UPDATE_COMPLETE"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "UPDATE_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "UPDATE_ROLLBACK_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "UPDATE_ROLLBACK_COMPLETE"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ValidationError"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newStackCreateComplete :: Core.Wait DescribeStacks
newStackCreateComplete =
  Core.Wait
    { Core._waitName = "StackCreateComplete",
      Core._waitAttempts = 120,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "CREATE_COMPLETE"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "CREATE_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "DELETE_COMPLETE"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "DELETE_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "ROLLBACK_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "ROLLBACK_COMPLETE"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ValidationError"
            Core.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeTypeRegistration' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newTypeRegistrationComplete :: Core.Wait DescribeTypeRegistration
newTypeRegistrationComplete =
  Core.Wait
    { Core._waitName =
        "TypeRegistrationComplete",
      Core._waitAttempts = 120,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "COMPLETE"
            Core.AcceptSuccess
            ( describeTypeRegistrationResponse_progressStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAll
            "FAILED"
            Core.AcceptFailure
            ( describeTypeRegistrationResponse_progressStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Core.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newStackImportComplete :: Core.Wait DescribeStacks
newStackImportComplete =
  Core.Wait
    { Core._waitName = "StackImportComplete",
      Core._waitAttempts = 120,
      Core._waitDelay = 30,
      Core._waitAcceptors =
        [ Core.matchAll
            "IMPORT_COMPLETE"
            Core.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "ROLLBACK_COMPLETE"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "ROLLBACK_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "IMPORT_ROLLBACK_IN_PROGRESS"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "IMPORT_ROLLBACK_FAILED"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchAny
            "IMPORT_ROLLBACK_COMPLETE"
            Core.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Core.toTextCI
            ),
          Core.matchError
            "ValidationError"
            Core.AcceptFailure
        ]
    }
