{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Waiters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Waiters
  ( -- * InstanceTerminated
    mkInstanceTerminated,

    -- * DeploymentSuccessful
    mkDeploymentSuccessful,

    -- * InstanceStopped
    mkInstanceStopped,

    -- * InstanceOnline
    mkInstanceOnline,

    -- * AppExists
    mkAppExists,

    -- * InstanceRegistered
    mkInstanceRegistered,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.DescribeApps
import Network.AWS.OpsWorks.DescribeDeployments
import Network.AWS.OpsWorks.DescribeInstances
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Waiter as Wait

-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceTerminated :: Wait.Wait DescribeInstances
mkInstanceTerminated =
  Wait.Wait
    { Wait._waitName = "InstanceTerminated",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "terminated"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchError "ResourceNotFoundException" Wait.AcceptSuccess,
          Wait.matchAny
            "booting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "online"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "pending"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "rebooting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "requested"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "running_setup"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "setup_failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "start_failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.OpsWorks.DescribeDeployments' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkDeploymentSuccessful :: Wait.Wait DescribeDeployments
mkDeploymentSuccessful =
  Wait.Wait
    { Wait._waitName = "DeploymentSuccessful",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "successful"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (ddrsDeployments Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. dStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (ddrsDeployments Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. dStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceStopped :: Wait.Wait DescribeInstances
mkInstanceStopped =
  Wait.Wait
    { Wait._waitName = "InstanceStopped",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "stopped"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "booting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "pending"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "rebooting"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "requested"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "running_setup"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "setup_failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "start_failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "stop_failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceOnline :: Wait.Wait DescribeInstances
mkInstanceOnline =
  Wait.Wait
    { Wait._waitName = "InstanceOnline",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "online"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "setup_failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "shutting_down"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "start_failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "stopped"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "stopping"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "terminating"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "terminated"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "stop_failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }

-- | Polls 'Network.AWS.OpsWorks.DescribeApps' every 1 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkAppExists :: Wait.Wait DescribeApps
mkAppExists =
  Wait.Wait
    { Wait._waitName = "AppExists",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 1,
      Wait._waitAcceptors =
        [ Wait.matchStatus 200 Wait.AcceptSuccess,
          Wait.matchStatus 400 Wait.AcceptFailure
        ]
    }

-- | Polls 'Network.AWS.OpsWorks.DescribeInstances' every 15 seconds until a successful state is reached. An error is returned after 40 failed checks.
mkInstanceRegistered :: Wait.Wait DescribeInstances
mkInstanceRegistered =
  Wait.Wait
    { Wait._waitName = "InstanceRegistered",
      Wait._waitAttempts = 40,
      Wait._waitDelay = 15,
      Wait._waitAcceptors =
        [ Wait.matchAll
            "registered"
            Wait.AcceptSuccess
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "setup_failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "shutting_down"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "stopped"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "stopping"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "terminating"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "terminated"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            ),
          Wait.matchAny
            "stop_failed"
            Wait.AcceptFailure
            ( Lens.folding
                ( Lens.concatOf
                    (dirsInstances Lude.. Lens._Just Lude.. Lens.to Lude.toList)
                )
                Lude.. iStatus
                Lude.. Lens._Just
                Lude.. Lens.to Lude.toText
            )
        ]
    }
