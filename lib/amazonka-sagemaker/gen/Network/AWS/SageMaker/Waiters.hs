{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies      #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Waiters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SageMaker.Waiters where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.SageMaker.DescribeEndpoint
import Network.AWS.SageMaker.DescribeEndpoint
import Network.AWS.SageMaker.DescribeNotebookInstance
import Network.AWS.SageMaker.DescribeNotebookInstance
import Network.AWS.SageMaker.DescribeNotebookInstance
import Network.AWS.SageMaker.DescribeTrainingJob
import Network.AWS.SageMaker.Types
import Network.AWS.Waiter

-- | Polls 'Network.AWS.SageMaker.DescribeNotebookInstance' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
notebookInstanceDeleted :: Wait DescribeNotebookInstance
notebookInstanceDeleted =
  Wait
    { _waitName = "NotebookInstanceDeleted"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchError "ValidationException" AcceptSuccess
        , matchAll
            "Failed"
            AcceptFailure
            (dnirsNotebookInstanceStatus . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.SageMaker.DescribeEndpoint' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
endpointDeleted :: Wait DescribeEndpoint
endpointDeleted =
  Wait
    { _waitName = "EndpointDeleted"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchError "ValidationException" AcceptSuccess
        , matchAll "Failed" AcceptFailure (dersEndpointStatus . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.SageMaker.DescribeEndpoint' every 30 seconds until a successful state is reached. An error is returned after 120 failed checks.
endpointInService :: Wait DescribeEndpoint
endpointInService =
  Wait
    { _waitName = "EndpointInService"
    , _waitAttempts = 120
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll "InService" AcceptSuccess (dersEndpointStatus . to toTextCI)
        , matchAll "Failed" AcceptFailure (dersEndpointStatus . to toTextCI)
        , matchError "ValidationException" AcceptFailure
        ]
    }


-- | Polls 'Network.AWS.SageMaker.DescribeNotebookInstance' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
notebookInstanceInService :: Wait DescribeNotebookInstance
notebookInstanceInService =
  Wait
    { _waitName = "NotebookInstanceInService"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll
            "InService"
            AcceptSuccess
            (dnirsNotebookInstanceStatus . to toTextCI)
        , matchAll
            "Failed"
            AcceptFailure
            (dnirsNotebookInstanceStatus . to toTextCI)
        ]
    }


-- | Polls 'Network.AWS.SageMaker.DescribeTrainingJob' every 120 seconds until a successful state is reached. An error is returned after 180 failed checks.
trainingJobCompletedOrStopped :: Wait DescribeTrainingJob
trainingJobCompletedOrStopped =
  Wait
    { _waitName = "TrainingJobCompletedOrStopped"
    , _waitAttempts = 180
    , _waitDelay = 120
    , _waitAcceptors =
        [ matchAll
            "Completed"
            AcceptSuccess
            (dtjrsTrainingJobStatus . to toTextCI)
        , matchAll
            "Stopped"
            AcceptSuccess
            (dtjrsTrainingJobStatus . to toTextCI)
        , matchAll "Failed" AcceptFailure (dtjrsTrainingJobStatus . to toTextCI)
        , matchError "ValidationException" AcceptFailure
        ]
    }


-- | Polls 'Network.AWS.SageMaker.DescribeNotebookInstance' every 30 seconds until a successful state is reached. An error is returned after 60 failed checks.
notebookInstanceStopped :: Wait DescribeNotebookInstance
notebookInstanceStopped =
  Wait
    { _waitName = "NotebookInstanceStopped"
    , _waitAttempts = 60
    , _waitDelay = 30
    , _waitAcceptors =
        [ matchAll
            "Stopped"
            AcceptSuccess
            (dnirsNotebookInstanceStatus . to toTextCI)
        , matchAll
            "Failed"
            AcceptFailure
            (dnirsNotebookInstanceStatus . to toTextCI)
        ]
    }

