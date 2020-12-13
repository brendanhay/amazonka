{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Waiters
  ( -- * StackImportComplete
    mkStackImportComplete,

    -- * StackCreateComplete
    mkStackCreateComplete,

    -- * StackRollbackComplete
    mkStackRollbackComplete,

    -- * TypeRegistrationComplete
    mkTypeRegistrationComplete,

    -- * StackUpdateComplete
    mkStackUpdateComplete,

    -- * StackExists
    mkStackExists,

    -- * StackDeleteComplete
    mkStackDeleteComplete,

    -- * ChangeSetCreateComplete
    mkChangeSetCreateComplete,
  )
where

import Network.AWS.CloudFormation.DescribeChangeSet
import Network.AWS.CloudFormation.DescribeStacks
import Network.AWS.CloudFormation.DescribeTypeRegistration
import Network.AWS.CloudFormation.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkStackImportComplete :: Wait.Wait DescribeStacks
mkStackImportComplete =
  Wait.Wait
    { Wait._waitName = "StackImportComplete",
      Wait._waitAttempts = 120,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "IMPORT_COMPLETE"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "ROLLBACK_COMPLETE"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "ROLLBACK_FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "IMPORT_ROLLBACK_IN_PROGRESS"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "IMPORT_ROLLBACK_FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "IMPORT_ROLLBACK_COMPLETE"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "ValidationError" Wait.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkStackCreateComplete :: Wait.Wait DescribeStacks
mkStackCreateComplete =
  Wait.Wait
    { Wait._waitName = "StackCreateComplete",
      Wait._waitAttempts = 120,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "CREATE_COMPLETE"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "CREATE_FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "DELETE_COMPLETE"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "DELETE_FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "ROLLBACK_FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "ROLLBACK_COMPLETE"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "ValidationError" Wait.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkStackRollbackComplete :: Wait.Wait DescribeStacks
mkStackRollbackComplete =
  Wait.Wait
    { Wait._waitName = "StackRollbackComplete",
      Wait._waitAttempts = 120,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "UPDATE_ROLLBACK_COMPLETE"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "UPDATE_FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "UPDATE_ROLLBACK_FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "DELETE_FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "ValidationError" Wait.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeTypeRegistration' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkTypeRegistrationComplete :: Wait.Wait DescribeTypeRegistration
mkTypeRegistrationComplete =
  Wait.Wait
    { Wait._waitName = "TypeRegistrationComplete",
      Wait._waitAttempts = 120,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "COMPLETE"
            Wait.AcceptSuccess
            (dtrrsProgressStatus Lude.. Lens._Just Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "FAILED"
            Wait.AcceptFailure
            ( dtrrsProgressStatus Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkStackUpdateComplete :: Wait.Wait DescribeStacks
mkStackUpdateComplete =
  Wait.Wait
    { Wait._waitName = "StackUpdateComplete",
      Wait._waitAttempts = 120,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "UPDATE_COMPLETE"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "UPDATE_FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "UPDATE_ROLLBACK_FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "UPDATE_ROLLBACK_COMPLETE"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "ValidationError" Wait.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkStackExists :: Wait.Wait DescribeStacks
mkStackExists =
  Wait.Wait
    { Wait._waitName = "StackExists",
      Wait._waitAttempts = 20,
      Wait._waitDelay = 5,
      Wait._waitAcceptors =
        [ Wait.matchStatus 200 Wait.AcceptSuccess,
          Wait.matchError "ValidationError" Wait.AcceptRetry
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkStackDeleteComplete :: Wait.Wait DescribeStacks
mkStackDeleteComplete =
  Wait.Wait
    { Wait._waitName = "StackDeleteComplete",
      Wait._waitAttempts = 120,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "DELETE_COMPLETE"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "ValidationError" Wait.AcceptSuccess,
          Wait.matchAny
            "DELETE_FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "CREATE_FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "ROLLBACK_FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "UPDATE_ROLLBACK_IN_PROGRESS"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "UPDATE_ROLLBACK_FAILED"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "UPDATE_ROLLBACK_COMPLETE"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dsrsStacks Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. sfStackStatus
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.CloudFormation.DescribeChangeSet' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkChangeSetCreateComplete :: Wait.Wait DescribeChangeSet
mkChangeSetCreateComplete =
  Wait.Wait
    { Wait._waitName = "ChangeSetCreateComplete",
      Wait._waitAttempts = 120,
      Wait._waitDelay = 30,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "CREATE_COMPLETE"
            Wait.AcceptSuccess
            (dcsfrsStatus Lude.. Lens.to Lude.toText),
          Wait.matchAll
            "FAILED"
            Wait.AcceptFailure
            (dcsfrsStatus Lude.. Lens.to Lude.toText),
          Wait.matchError "ValidationError" Wait.AcceptFailure
        ]
    }
