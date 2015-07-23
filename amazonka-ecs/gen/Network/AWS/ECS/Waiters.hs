{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Waiters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ECS.Waiters where

import           Network.AWS.ECS.DescribeServices
import           Network.AWS.ECS.DescribeTasks
import           Network.AWS.ECS.DescribeTasks
import           Network.AWS.ECS.Types
import           Network.AWS.Prelude
import           Network.AWS.Waiter

servicesInactive :: Wait DescribeServices
servicesInactive =
    Wait
    { _waitName = "ServicesInactive"
    , _waitAttempts = 40
    , _waitDelay = 15
    , _waitAcceptors = [ matchAny
                             "MISSING"
                             AcceptFailure
                             (folding (concatOf dssrsFailures) .
                              fReason . _Just . to toText)
                       , matchAny
                             "INACTIVE"
                             AcceptSuccess
                             (folding (concatOf dssrsServices) .
                              csStatus . _Just . to toText)]
    }

tasksRunning :: Wait DescribeTasks
tasksRunning =
    Wait
    { _waitName = "TasksRunning"
    , _waitAttempts = 100
    , _waitDelay = 6
    , _waitAcceptors = [ matchAny
                             "STOPPED"
                             AcceptFailure
                             (folding (concatOf dtrsTasks) .
                              tLastStatus . _Just . to toText)
                       , matchAny
                             "MISSING"
                             AcceptFailure
                             (folding (concatOf dtrsFailures) .
                              fReason . _Just . to toText)
                       , matchAll
                             "RUNNING"
                             AcceptSuccess
                             (folding (concatOf dtrsTasks) .
                              tLastStatus . _Just . to toText)]
    }

tasksStopped :: Wait DescribeTasks
tasksStopped =
    Wait
    { _waitName = "TasksStopped"
    , _waitAttempts = 100
    , _waitDelay = 6
    , _waitAcceptors = [ matchAll
                             "STOPPED"
                             AcceptSuccess
                             (folding (concatOf dtrsTasks) .
                              tLastStatus . _Just . to toText)]
    }
