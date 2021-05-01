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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.CloudFormation.DescribeChangeSet' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newChangeSetCreateComplete :: Waiter.Wait DescribeChangeSet
newChangeSetCreateComplete =
  Waiter.Wait
    { Waiter._waitName =
        "ChangeSetCreateComplete",
      Waiter._waitAttempts = 120,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "CREATE_COMPLETE"
            Waiter.AcceptSuccess
            ( describeChangeSetResponse_status
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "FAILED"
            Waiter.AcceptFailure
            ( describeChangeSetResponse_status
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ValidationError"
            Waiter.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newStackRollbackComplete :: Waiter.Wait DescribeStacks
newStackRollbackComplete =
  Waiter.Wait
    { Waiter._waitName =
        "StackRollbackComplete",
      Waiter._waitAttempts = 120,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "UPDATE_ROLLBACK_COMPLETE"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "UPDATE_FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "UPDATE_ROLLBACK_FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "DELETE_FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ValidationError"
            Waiter.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newStackDeleteComplete :: Waiter.Wait DescribeStacks
newStackDeleteComplete =
  Waiter.Wait
    { Waiter._waitName =
        "StackDeleteComplete",
      Waiter._waitAttempts = 120,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "DELETE_COMPLETE"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ValidationError"
            Waiter.AcceptSuccess,
          Waiter.matchAny
            "DELETE_FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "CREATE_FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "ROLLBACK_FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "UPDATE_ROLLBACK_IN_PROGRESS"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "UPDATE_ROLLBACK_FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "UPDATE_ROLLBACK_COMPLETE"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
newStackExists :: Waiter.Wait DescribeStacks
newStackExists =
  Waiter.Wait
    { Waiter._waitName = "StackExists",
      Waiter._waitAttempts = 20,
      Waiter._waitDelay = 5,
      Waiter._waitAcceptors =
        [ Waiter.matchStatus 200 Waiter.AcceptSuccess,
          Waiter.matchError
            "ValidationError"
            Waiter.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newStackUpdateComplete :: Waiter.Wait DescribeStacks
newStackUpdateComplete =
  Waiter.Wait
    { Waiter._waitName =
        "StackUpdateComplete",
      Waiter._waitAttempts = 120,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "UPDATE_COMPLETE"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "UPDATE_FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "UPDATE_ROLLBACK_FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "UPDATE_ROLLBACK_COMPLETE"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ValidationError"
            Waiter.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newStackCreateComplete :: Waiter.Wait DescribeStacks
newStackCreateComplete =
  Waiter.Wait
    { Waiter._waitName =
        "StackCreateComplete",
      Waiter._waitAttempts = 120,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "CREATE_COMPLETE"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "CREATE_FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "DELETE_COMPLETE"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "DELETE_FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "ROLLBACK_FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "ROLLBACK_COMPLETE"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ValidationError"
            Waiter.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeTypeRegistration' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newTypeRegistrationComplete :: Waiter.Wait DescribeTypeRegistration
newTypeRegistrationComplete =
  Waiter.Wait
    { Waiter._waitName =
        "TypeRegistrationComplete",
      Waiter._waitAttempts = 120,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "COMPLETE"
            Waiter.AcceptSuccess
            ( describeTypeRegistrationResponse_progressStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "FAILED"
            Waiter.AcceptFailure
            ( describeTypeRegistrationResponse_progressStatus
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
newStackImportComplete :: Waiter.Wait DescribeStacks
newStackImportComplete =
  Waiter.Wait
    { Waiter._waitName =
        "StackImportComplete",
      Waiter._waitAttempts = 120,
      Waiter._waitDelay = 30,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "IMPORT_COMPLETE"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "ROLLBACK_COMPLETE"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "ROLLBACK_FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "IMPORT_ROLLBACK_IN_PROGRESS"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "IMPORT_ROLLBACK_FAILED"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAny
            "IMPORT_ROLLBACK_COMPLETE"
            Waiter.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (describeStacksResponse_stacks Prelude.. Lens._Just)
                )
                Prelude.. stack_stackStatus
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchError
            "ValidationError"
            Waiter.AcceptFailure
        ]
    }
