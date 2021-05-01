{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Waiters
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Waiters where

import Network.AWS.ElasticBeanstalk.DescribeEnvironments
import Network.AWS.ElasticBeanstalk.Lens
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Waiter as Waiter

-- | Polls 'Network.AWS.ElasticBeanstalk.DescribeEnvironments' every 20 seconds until a successful state is reached. An error is returned after 20 failed checks.
newEnvironmentTerminated :: Waiter.Wait DescribeEnvironments
newEnvironmentTerminated =
  Waiter.Wait
    { Waiter._waitName =
        "EnvironmentTerminated",
      Waiter._waitAttempts = 20,
      Waiter._waitDelay = 20,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Terminated"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( environmentDescriptionsMessage_environments
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. environmentDescription_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Terminating"
            Waiter.AcceptRetry
            ( Lens.folding
                ( Lens.concatOf
                    ( environmentDescriptionsMessage_environments
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. environmentDescription_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.ElasticBeanstalk.DescribeEnvironments' every 20 seconds until a successful state is reached. An error is returned after 20 failed checks.
newEnvironmentUpdated :: Waiter.Wait DescribeEnvironments
newEnvironmentUpdated =
  Waiter.Wait
    { Waiter._waitName =
        "EnvironmentUpdated",
      Waiter._waitAttempts = 20,
      Waiter._waitDelay = 20,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Ready"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( environmentDescriptionsMessage_environments
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. environmentDescription_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Updating"
            Waiter.AcceptRetry
            ( Lens.folding
                ( Lens.concatOf
                    ( environmentDescriptionsMessage_environments
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. environmentDescription_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }

-- | Polls 'Network.AWS.ElasticBeanstalk.DescribeEnvironments' every 20 seconds until a successful state is reached. An error is returned after 20 failed checks.
newEnvironmentExists :: Waiter.Wait DescribeEnvironments
newEnvironmentExists =
  Waiter.Wait
    { Waiter._waitName = "EnvironmentExists",
      Waiter._waitAttempts = 20,
      Waiter._waitDelay = 20,
      Waiter._waitAcceptors =
        [ Waiter.matchAll
            "Ready"
            Waiter.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    ( environmentDescriptionsMessage_environments
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. environmentDescription_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            ),
          Waiter.matchAll
            "Launching"
            Waiter.AcceptRetry
            ( Lens.folding
                ( Lens.concatOf
                    ( environmentDescriptionsMessage_environments
                        Prelude.. Lens._Just
                    )
                )
                Prelude.. environmentDescription_status
                Prelude.. Lens._Just
                Prelude.. Lens.to Prelude.toTextCI
            )
        ]
    }
