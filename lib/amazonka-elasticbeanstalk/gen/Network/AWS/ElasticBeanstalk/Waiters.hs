{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Waiters where

import Network.AWS.ElasticBeanstalk.DescribeEnvironments
import Network.AWS.ElasticBeanstalk.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Waiter

-- | Polls 'Network.AWS.ElasticBeanstalk.DescribeEnvironments' every 20 seconds until a successful state is reached. An error is returned after 20 failed checks.
environmentExists :: Wait DescribeEnvironments
environmentExists =
  Wait
    { _waitName = "EnvironmentExists",
      _waitAttempts = 20,
      _waitDelay = 20,
      _waitAcceptors =
        [ matchAll
            "Ready"
            AcceptSuccess
            ( folding (concatOf (edmEnvironments . to toList)) . eStatus . _Just
                . to toTextCI
            ),
          matchAll
            "Launching"
            AcceptRetry
            ( folding (concatOf (edmEnvironments . to toList)) . eStatus . _Just
                . to toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.ElasticBeanstalk.DescribeEnvironments' every 20 seconds until a successful state is reached. An error is returned after 20 failed checks.
environmentUpdated :: Wait DescribeEnvironments
environmentUpdated =
  Wait
    { _waitName = "EnvironmentUpdated",
      _waitAttempts = 20,
      _waitDelay = 20,
      _waitAcceptors =
        [ matchAll
            "Ready"
            AcceptSuccess
            ( folding (concatOf (edmEnvironments . to toList)) . eStatus . _Just
                . to toTextCI
            ),
          matchAll
            "Updating"
            AcceptRetry
            ( folding (concatOf (edmEnvironments . to toList)) . eStatus . _Just
                . to toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.ElasticBeanstalk.DescribeEnvironments' every 20 seconds until a successful state is reached. An error is returned after 20 failed checks.
environmentTerminated :: Wait DescribeEnvironments
environmentTerminated =
  Wait
    { _waitName = "EnvironmentTerminated",
      _waitAttempts = 20,
      _waitDelay = 20,
      _waitAcceptors =
        [ matchAll
            "Terminated"
            AcceptSuccess
            ( folding (concatOf (edmEnvironments . to toList)) . eStatus . _Just
                . to toTextCI
            ),
          matchAll
            "Terminating"
            AcceptRetry
            ( folding (concatOf (edmEnvironments . to toList)) . eStatus . _Just
                . to toTextCI
            )
        ]
    }
