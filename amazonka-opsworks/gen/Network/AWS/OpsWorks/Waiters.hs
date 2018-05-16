{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.OpsWorks.Waiters where

import Network.AWS.Lens
import Network.AWS.OpsWorks.DescribeApps
import Network.AWS.OpsWorks.DescribeDeployments
import Network.AWS.OpsWorks.DescribeInstances
import Network.AWS.OpsWorks.DescribeInstances
import Network.AWS.OpsWorks.DescribeInstances
import Network.AWS.OpsWorks.DescribeInstances
import Network.AWS.OpsWorks.Types
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
instanceTerminated :: Wait DescribeInstances
instanceTerminated =
  Wait
    { _waitName = "InstanceTerminated"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "terminated"
            AcceptSuccess
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchError "ResourceNotFoundException" AcceptSuccess
        , matchAny
            "booting"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "online"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "pending"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "rebooting"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "requested"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "running_setup"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "setup_failed"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "start_failed"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.OpsWorks.DescribeDeployments' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
deploymentSuccessful :: Wait DescribeDeployments
deploymentSuccessful =
  Wait
    { _waitName = "DeploymentSuccessful"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "successful"
            AcceptSuccess
            (folding (concatOf ddrsDeployments) . dStatus . _Just . to toTextCI)
        , matchAny
            "failed"
            AcceptFailure
            (folding (concatOf ddrsDeployments) . dStatus . _Just . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
instanceStopped :: Wait DescribeInstances
instanceStopped =
  Wait
    { _waitName = "InstanceStopped"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "stopped"
            AcceptSuccess
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "booting"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "pending"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "rebooting"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "requested"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "running_setup"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "setup_failed"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "start_failed"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "stop_failed"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
instanceOnline :: Wait DescribeInstances
instanceOnline =
  Wait
    { _waitName = "InstanceOnline"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "online"
            AcceptSuccess
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "setup_failed"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "shutting_down"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "start_failed"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "stopped"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "stopping"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "terminating"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "terminated"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "stop_failed"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.OpsWorks.DescribeApps' every 1 seconds until a successful state is reached. An error is returned after 40 failed checks.
appExists :: Wait DescribeApps
appExists =
  Wait
    { _waitName = "AppExists"
    , _waitAttempts = 40
    , _waitDelay = 1
    , _waitAcceptors =
        [matchStatus 200 AcceptSuccess, matchStatus 400 AcceptFailure]
    }


-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
instanceRegistered :: Wait DescribeInstances
instanceRegistered =
  Wait
    { _waitName = "InstanceRegistered"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors =
        [ matchAll
            "registered"
            AcceptSuccess
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "setup_failed"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "shutting_down"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "stopped"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "stopping"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "terminating"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "terminated"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        , matchAny
            "stop_failed"
            AcceptFailure
            (folding (concatOf dirsInstances) . iStatus . _Just . to toTextCI)
        ]
    }

