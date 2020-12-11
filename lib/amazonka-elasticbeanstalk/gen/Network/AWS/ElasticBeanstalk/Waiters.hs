{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticBeanstalk.Waiters
  ( -- * EnvironmentExists
    mkEnvironmentExists,

    -- * EnvironmentUpdated
    mkEnvironmentUpdated,

    -- * EnvironmentTerminated
    mkEnvironmentTerminated,
  )
where

import Network.AWS.ElasticBeanstalk.DescribeEnvironments
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.ElasticBeanstalk.DescribeEnvironments' every 20 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkEnvironmentExists :: Wait.Wait DescribeEnvironments
mkEnvironmentExists =
  Wait.Wait
    { Wait._waitName = "EnvironmentExists",
      Wait._waitAttempts = 20,
      Wait._waitDelay = 20,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "Ready"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (edmEnvironments Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. eStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAll
            "Launching"
            Wait.AcceptRetry
            ( Lens.folding
                ( Lens.concatOf
                    (edmEnvironments Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. eStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.ElasticBeanstalk.DescribeEnvironments' every 20 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkEnvironmentUpdated :: Wait.Wait DescribeEnvironments
mkEnvironmentUpdated =
  Wait.Wait
    { Wait._waitName = "EnvironmentUpdated",
      Wait._waitAttempts = 20,
      Wait._waitDelay = 20,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "Ready"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (edmEnvironments Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. eStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAll
            "Updating"
            Wait.AcceptRetry
            ( Lens.folding
                ( Lens.concatOf
                    (edmEnvironments Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. eStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.ElasticBeanstalk.DescribeEnvironments' every 20 seconds until a successful state is reached. An error is returned after 20 failed checks.
mkEnvironmentTerminated :: Wait.Wait DescribeEnvironments
mkEnvironmentTerminated =
  Wait.Wait
    { Wait._waitName = "EnvironmentTerminated",
      Wait._waitAttempts = 20,
      Wait._waitDelay = 20,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "Terminated"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (edmEnvironments Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. eStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAll
            "Terminating"
            Wait.AcceptRetry
            ( Lens.folding
                ( Lens.concatOf
                    (edmEnvironments Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. eStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }
