{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackDriftInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackDriftInformation
  ( StackDriftInformation (..),

    -- * Smart constructor
    mkStackDriftInformation,

    -- * Lenses
    sdiLastCheckTimestamp,
    sdiStackDriftStatus,
  )
where

import Network.AWS.CloudFormation.Types.StackDriftStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about whether the stack's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. A stack is considered to have drifted if one or more of its resources have drifted.
--
-- /See:/ 'mkStackDriftInformation' smart constructor.
data StackDriftInformation = StackDriftInformation'
  { lastCheckTimestamp ::
      Lude.Maybe Lude.DateTime,
    stackDriftStatus :: StackDriftStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackDriftInformation' with the minimum fields required to make a request.
--
-- * 'lastCheckTimestamp' - Most recent time when a drift detection operation was initiated on the stack, or any of its individual resources that support drift detection.
-- * 'stackDriftStatus' - Status of the stack's actual configuration compared to its expected template configuration.
--
--
--     * @DRIFTED@ : The stack differs from its expected template configuration. A stack is considered to have drifted if one or more of its resources have drifted.
--
--
--     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack differs from its expected template configuration.
--
--
--     * @IN_SYNC@ : The stack's actual configuration matches its expected template configuration.
--
--
--     * @UNKNOWN@ : This value is reserved for future use.
mkStackDriftInformation ::
  -- | 'stackDriftStatus'
  StackDriftStatus ->
  StackDriftInformation
mkStackDriftInformation pStackDriftStatus_ =
  StackDriftInformation'
    { lastCheckTimestamp = Lude.Nothing,
      stackDriftStatus = pStackDriftStatus_
    }

-- | Most recent time when a drift detection operation was initiated on the stack, or any of its individual resources that support drift detection.
--
-- /Note:/ Consider using 'lastCheckTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdiLastCheckTimestamp :: Lens.Lens' StackDriftInformation (Lude.Maybe Lude.DateTime)
sdiLastCheckTimestamp = Lens.lens (lastCheckTimestamp :: StackDriftInformation -> Lude.Maybe Lude.DateTime) (\s a -> s {lastCheckTimestamp = a} :: StackDriftInformation)
{-# DEPRECATED sdiLastCheckTimestamp "Use generic-lens or generic-optics with 'lastCheckTimestamp' instead." #-}

-- | Status of the stack's actual configuration compared to its expected template configuration.
--
--
--     * @DRIFTED@ : The stack differs from its expected template configuration. A stack is considered to have drifted if one or more of its resources have drifted.
--
--
--     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the stack differs from its expected template configuration.
--
--
--     * @IN_SYNC@ : The stack's actual configuration matches its expected template configuration.
--
--
--     * @UNKNOWN@ : This value is reserved for future use.
--
--
--
-- /Note:/ Consider using 'stackDriftStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdiStackDriftStatus :: Lens.Lens' StackDriftInformation StackDriftStatus
sdiStackDriftStatus = Lens.lens (stackDriftStatus :: StackDriftInformation -> StackDriftStatus) (\s a -> s {stackDriftStatus = a} :: StackDriftInformation)
{-# DEPRECATED sdiStackDriftStatus "Use generic-lens or generic-optics with 'stackDriftStatus' instead." #-}

instance Lude.FromXML StackDriftInformation where
  parseXML x =
    StackDriftInformation'
      Lude.<$> (x Lude..@? "LastCheckTimestamp")
      Lude.<*> (x Lude..@ "StackDriftStatus")
