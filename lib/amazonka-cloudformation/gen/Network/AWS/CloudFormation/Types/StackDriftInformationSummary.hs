{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackDriftInformationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackDriftInformationSummary
  ( StackDriftInformationSummary (..),

    -- * Smart constructor
    mkStackDriftInformationSummary,

    -- * Lenses
    sdisStackDriftStatus,
    sdisLastCheckTimestamp,
  )
where

import Network.AWS.CloudFormation.Types.StackDriftStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about whether the stack's actual configuration differs, or has /drifted/ , from its expected configuration, as defined in the stack template and any values specified as template parameters. A stack is considered to have drifted if one or more of its resources have drifted.
--
-- /See:/ 'mkStackDriftInformationSummary' smart constructor.
data StackDriftInformationSummary = StackDriftInformationSummary'
  { -- | Status of the stack's actual configuration compared to its expected template configuration.
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
    stackDriftStatus :: StackDriftStatus,
    -- | Most recent time when a drift detection operation was initiated on the stack, or any of its individual resources that support drift detection.
    lastCheckTimestamp :: Lude.Maybe Lude.DateTime
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackDriftInformationSummary' with the minimum fields required to make a request.
--
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
--
--
-- * 'lastCheckTimestamp' - Most recent time when a drift detection operation was initiated on the stack, or any of its individual resources that support drift detection.
mkStackDriftInformationSummary ::
  -- | 'stackDriftStatus'
  StackDriftStatus ->
  StackDriftInformationSummary
mkStackDriftInformationSummary pStackDriftStatus_ =
  StackDriftInformationSummary'
    { stackDriftStatus =
        pStackDriftStatus_,
      lastCheckTimestamp = Lude.Nothing
    }

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
sdisStackDriftStatus :: Lens.Lens' StackDriftInformationSummary StackDriftStatus
sdisStackDriftStatus = Lens.lens (stackDriftStatus :: StackDriftInformationSummary -> StackDriftStatus) (\s a -> s {stackDriftStatus = a} :: StackDriftInformationSummary)
{-# DEPRECATED sdisStackDriftStatus "Use generic-lens or generic-optics with 'stackDriftStatus' instead." #-}

-- | Most recent time when a drift detection operation was initiated on the stack, or any of its individual resources that support drift detection.
--
-- /Note:/ Consider using 'lastCheckTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sdisLastCheckTimestamp :: Lens.Lens' StackDriftInformationSummary (Lude.Maybe Lude.DateTime)
sdisLastCheckTimestamp = Lens.lens (lastCheckTimestamp :: StackDriftInformationSummary -> Lude.Maybe Lude.DateTime) (\s a -> s {lastCheckTimestamp = a} :: StackDriftInformationSummary)
{-# DEPRECATED sdisLastCheckTimestamp "Use generic-lens or generic-optics with 'lastCheckTimestamp' instead." #-}

instance Lude.FromXML StackDriftInformationSummary where
  parseXML x =
    StackDriftInformationSummary'
      Lude.<$> (x Lude..@ "StackDriftStatus")
      Lude.<*> (x Lude..@? "LastCheckTimestamp")
