{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-deprecations #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Waiters
  (
    -- * StackImportComplete
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
  ) where

import Network.AWS.CloudFormation.DescribeChangeSet
import Network.AWS.CloudFormation.DescribeStacks
import Network.AWS.CloudFormation.DescribeTypeRegistration
import qualified Network.AWS.CloudFormation.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkStackImportComplete :: Waiter.Wait DescribeStacks
mkStackImportComplete
  = Waiter.Wait{Waiter._waitName = "StackImportComplete",
                Waiter._waitAttempts = 120, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "IMPORT_COMPLETE" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "ROLLBACK_COMPLETE" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "ROLLBACK_FAILED" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "IMPORT_ROLLBACK_IN_PROGRESS" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "IMPORT_ROLLBACK_FAILED" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "IMPORT_ROLLBACK_COMPLETE" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchError "ValidationError" Waiter.AcceptFailure]}

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkStackCreateComplete :: Waiter.Wait DescribeStacks
mkStackCreateComplete
  = Waiter.Wait{Waiter._waitName = "StackCreateComplete",
                Waiter._waitAttempts = 120, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "CREATE_COMPLETE" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "CREATE_FAILED" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "DELETE_COMPLETE" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "DELETE_FAILED" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "ROLLBACK_FAILED" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "ROLLBACK_COMPLETE" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchError "ValidationError" Waiter.AcceptFailure]}

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkStackRollbackComplete :: Waiter.Wait DescribeStacks
mkStackRollbackComplete
  = Waiter.Wait{Waiter._waitName = "StackRollbackComplete",
                Waiter._waitAttempts = 120, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "UPDATE_ROLLBACK_COMPLETE" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "UPDATE_FAILED" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "UPDATE_ROLLBACK_FAILED" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "DELETE_FAILED" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchError "ValidationError" Waiter.AcceptFailure]}

-- | Polls 'Network.AWS.CloudFormation.DescribeTypeRegistration' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkTypeRegistrationComplete :: Waiter.Wait DescribeTypeRegistration
mkTypeRegistrationComplete
  = Waiter.Wait{Waiter._waitName = "TypeRegistrationComplete",
                Waiter._waitAttempts = 120, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "COMPLETE" Waiter.AcceptSuccess
                     (Lens.field @"progressStatus" Core.. Lens._Just),
                   Waiter.matchAll "FAILED" Waiter.AcceptFailure
                     (Lens.field @"progressStatus" Core.. Lens._Just)]}

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkStackUpdateComplete :: Waiter.Wait DescribeStacks
mkStackUpdateComplete
  = Waiter.Wait{Waiter._waitName = "StackUpdateComplete",
                Waiter._waitAttempts = 120, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "UPDATE_COMPLETE" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "UPDATE_FAILED" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "UPDATE_ROLLBACK_FAILED" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "UPDATE_ROLLBACK_COMPLETE" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchError "ValidationError" Waiter.AcceptFailure]}

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkStackExists :: Waiter.Wait DescribeStacks
mkStackExists
  = Waiter.Wait{Waiter._waitName = "StackExists",
                Waiter._waitAttempts = 20, Waiter._waitDelay = 5,
                Waiter._waitAcceptors =
                  [Waiter.matchStatus 200 Waiter.AcceptSuccess,
                   Waiter.matchError "ValidationError" Waiter.AcceptRetry]}

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkStackDeleteComplete :: Waiter.Wait DescribeStacks
mkStackDeleteComplete
  = Waiter.Wait{Waiter._waitName = "StackDeleteComplete",
                Waiter._waitAttempts = 120, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "DELETE_COMPLETE" Waiter.AcceptSuccess
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchError "ValidationError" Waiter.AcceptSuccess,
                   Waiter.matchAny "DELETE_FAILED" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "CREATE_FAILED" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "ROLLBACK_FAILED" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "UPDATE_ROLLBACK_IN_PROGRESS" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "UPDATE_ROLLBACK_FAILED" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus"),
                   Waiter.matchAny "UPDATE_ROLLBACK_COMPLETE" Waiter.AcceptFailure
                     (Lens.folding
                        (Lens.concatOf
                           (Lens.field @"stacks" Core.. Lens._Just Core..
                              Lens.to Core.toList))
                        Core.. Lens.field @"stackStatus")]}

-- | Polls 'Network.AWS.CloudFormation.DescribeChangeSet' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
mkChangeSetCreateComplete :: Waiter.Wait DescribeChangeSet
mkChangeSetCreateComplete
  = Waiter.Wait{Waiter._waitName = "ChangeSetCreateComplete",
                Waiter._waitAttempts = 120, Waiter._waitDelay = 30,
                Waiter._waitAcceptors =
                  [Waiter.matchAll "CREATE_COMPLETE" Waiter.AcceptSuccess
                     (Lens.field @"status"),
                   Waiter.matchAll "FAILED" Waiter.AcceptFailure
                     (Lens.field @"status"),
                   Waiter.matchError "ValidationError" Waiter.AcceptFailure]}
