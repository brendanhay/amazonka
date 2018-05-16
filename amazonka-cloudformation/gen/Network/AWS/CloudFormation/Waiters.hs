{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Waiters where

import Network.AWS.CloudFormation.DescribeChangeSet
import Network.AWS.CloudFormation.DescribeStacks
import Network.AWS.CloudFormation.DescribeStacks
import Network.AWS.CloudFormation.DescribeStacks
import Network.AWS.CloudFormation.DescribeStacks
import Network.AWS.CloudFormation.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
stackCreateComplete :: Wait DescribeStacks
stackCreateComplete =
  Wait
    { _waitName = "StackCreateComplete"
    , _waitAttempts = 120
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll
            "CREATE_COMPLETE"
            AcceptSuccess
            (folding (concatOf dsrsStacks) . sStackStatus . to toTextCI)
        , matchAny
            "CREATE_FAILED"
            AcceptFailure
            (folding (concatOf dsrsStacks) . sStackStatus . to toTextCI)
        , matchAny
            "DELETE_COMPLETE"
            AcceptFailure
            (folding (concatOf dsrsStacks) . sStackStatus . to toTextCI)
        , matchAny
            "DELETE_FAILED"
            AcceptFailure
            (folding (concatOf dsrsStacks) . sStackStatus . to toTextCI)
        , matchAny
            "ROLLBACK_FAILED"
            AcceptFailure
            (folding (concatOf dsrsStacks) . sStackStatus . to toTextCI)
        , matchAny
            "ROLLBACK_COMPLETE"
            AcceptFailure
            (folding (concatOf dsrsStacks) . sStackStatus . to toTextCI)
        , matchError "ValidationError" AcceptFailure
        ]
    }


-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
stackUpdateComplete :: Wait DescribeStacks
stackUpdateComplete =
  Wait
    { _waitName = "StackUpdateComplete"
    , _waitAttempts = 120
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll
            "UPDATE_COMPLETE"
            AcceptSuccess
            (folding (concatOf dsrsStacks) . sStackStatus . to toTextCI)
        , matchAny
            "UPDATE_FAILED"
            AcceptFailure
            (folding (concatOf dsrsStacks) . sStackStatus . to toTextCI)
        , matchAny
            "UPDATE_ROLLBACK_FAILED"
            AcceptFailure
            (folding (concatOf dsrsStacks) . sStackStatus . to toTextCI)
        , matchAny
            "UPDATE_ROLLBACK_COMPLETE"
            AcceptFailure
            (folding (concatOf dsrsStacks) . sStackStatus . to toTextCI)
        , matchError "ValidationError" AcceptFailure
        ]
    }


-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 5 seconds until a successful state is reached. An error is returned after 20 failed checks.
stackExists :: Wait DescribeStacks
stackExists =
  Wait
    { _waitName = "StackExists"
    , _waitAttempts = 20
    , _waitDelay = 5
    , _waitAcceptors =
        [ matchStatus 200 AcceptSuccess
        , matchError "ValidationError" AcceptRetry
        ]
    }


-- | Polls 'Network.AWS.CloudFormation.DescribeStacks' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
stackDeleteComplete :: Wait DescribeStacks
stackDeleteComplete =
  Wait
    { _waitName = "StackDeleteComplete"
    , _waitAttempts = 120
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll
            "DELETE_COMPLETE"
            AcceptSuccess
            (folding (concatOf dsrsStacks) . sStackStatus . to toTextCI)
        , matchError "ValidationError" AcceptSuccess
        , matchAny
            "DELETE_FAILED"
            AcceptFailure
            (folding (concatOf dsrsStacks) . sStackStatus . to toTextCI)
        , matchAny
            "CREATE_FAILED"
            AcceptFailure
            (folding (concatOf dsrsStacks) . sStackStatus . to toTextCI)
        , matchAny
            "ROLLBACK_FAILED"
            AcceptFailure
            (folding (concatOf dsrsStacks) . sStackStatus . to toTextCI)
        , matchAny
            "UPDATE_ROLLBACK_FAILED"
            AcceptFailure
            (folding (concatOf dsrsStacks) . sStackStatus . to toTextCI)
        , matchAny
            "UPDATE_ROLLBACK_IN_PROGRESS"
            AcceptFailure
            (folding (concatOf dsrsStacks) . sStackStatus . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.CloudFormation.DescribeChangeSet' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
changeSetCreateComplete :: Wait DescribeChangeSet
changeSetCreateComplete =
  Wait
    { _waitName = "ChangeSetCreateComplete"
    , _waitAttempts = 120
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll "CREATE_COMPLETE" AcceptSuccess (desrsStatus . to toTextCI)
        , matchAll "FAILED" AcceptFailure (desrsStatus . to toTextCI)
        , matchError "ValidationError" AcceptFailure
        ]
    }

