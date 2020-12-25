{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackResourceDriftInformation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResourceDriftInformation
  ( StackResourceDriftInformation (..),

    -- * Smart constructor
    mkStackResourceDriftInformation,

    -- * Lenses
    srdiStackResourceDriftStatus,
    srdiLastCheckTimestamp,
  )
where

import qualified Network.AWS.CloudFormation.Types.StackResourceDriftStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration.
--
-- /See:/ 'mkStackResourceDriftInformation' smart constructor.
data StackResourceDriftInformation = StackResourceDriftInformation'
  { -- | Status of the resource's actual configuration compared to its expected configuration
    --
    --
    --     * @DELETED@ : The resource differs from its expected configuration in that it has been deleted.
    --
    --
    --     * @MODIFIED@ : The resource differs from its expected configuration.
    --
    --
    --     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the resource differs from its expected configuration.
    -- Any resources that do not currently support drift detection have a status of @NOT_CHECKED@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
    --
    --
    --     * @IN_SYNC@ : The resources's actual configuration matches its expected configuration.
    stackResourceDriftStatus :: Types.StackResourceDriftStatus,
    -- | When AWS CloudFormation last checked if the resource had drifted from its expected configuration.
    lastCheckTimestamp :: Core.Maybe Core.UTCTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'StackResourceDriftInformation' value with any optional fields omitted.
mkStackResourceDriftInformation ::
  -- | 'stackResourceDriftStatus'
  Types.StackResourceDriftStatus ->
  StackResourceDriftInformation
mkStackResourceDriftInformation stackResourceDriftStatus =
  StackResourceDriftInformation'
    { stackResourceDriftStatus,
      lastCheckTimestamp = Core.Nothing
    }

-- | Status of the resource's actual configuration compared to its expected configuration
--
--
--     * @DELETED@ : The resource differs from its expected configuration in that it has been deleted.
--
--
--     * @MODIFIED@ : The resource differs from its expected configuration.
--
--
--     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the resource differs from its expected configuration.
-- Any resources that do not currently support drift detection have a status of @NOT_CHECKED@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
--
--
--     * @IN_SYNC@ : The resources's actual configuration matches its expected configuration.
--
--
--
-- /Note:/ Consider using 'stackResourceDriftStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdiStackResourceDriftStatus :: Lens.Lens' StackResourceDriftInformation Types.StackResourceDriftStatus
srdiStackResourceDriftStatus = Lens.field @"stackResourceDriftStatus"
{-# DEPRECATED srdiStackResourceDriftStatus "Use generic-lens or generic-optics with 'stackResourceDriftStatus' instead." #-}

-- | When AWS CloudFormation last checked if the resource had drifted from its expected configuration.
--
-- /Note:/ Consider using 'lastCheckTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdiLastCheckTimestamp :: Lens.Lens' StackResourceDriftInformation (Core.Maybe Core.UTCTime)
srdiLastCheckTimestamp = Lens.field @"lastCheckTimestamp"
{-# DEPRECATED srdiLastCheckTimestamp "Use generic-lens or generic-optics with 'lastCheckTimestamp' instead." #-}

instance Core.FromXML StackResourceDriftInformation where
  parseXML x =
    StackResourceDriftInformation'
      Core.<$> (x Core..@ "StackResourceDriftStatus")
      Core.<*> (x Core..@? "LastCheckTimestamp")
