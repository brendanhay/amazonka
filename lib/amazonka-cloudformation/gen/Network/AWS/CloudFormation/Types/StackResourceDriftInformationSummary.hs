{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackResourceDriftInformationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CloudFormation.Types.StackResourceDriftInformationSummary
  ( StackResourceDriftInformationSummary (..)
  -- * Smart constructor
  , mkStackResourceDriftInformationSummary
  -- * Lenses
  , srdisStackResourceDriftStatus
  , srdisLastCheckTimestamp
  ) where

import qualified Network.AWS.CloudFormation.Types.StackResourceDriftStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Summarizes information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration.
--
-- /See:/ 'mkStackResourceDriftInformationSummary' smart constructor.
data StackResourceDriftInformationSummary = StackResourceDriftInformationSummary'
  { stackResourceDriftStatus :: Types.StackResourceDriftStatus
    -- ^ Status of the resource's actual configuration compared to its expected configuration
--
--
--     * @DELETED@ : The resource differs from its expected configuration in that it has been deleted.
--
--
--     * @MODIFIED@ : The resource differs from its expected configuration.
--
--
--     * @NOT_CHECKED@ : AWS CloudFormation has not checked if the resource differs from its expected configuration.
-- Any resources that do not currently support drift detection have a status of @NOT_CHECKED@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> . If you performed an 'ContinueUpdateRollback' operation on a stack, any resources included in @ResourcesToSkip@ will also have a status of @NOT_CHECKED@ . For more information on skipping resources during rollback operations, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-continueupdaterollback.html Continue Rolling Back an Update> in the AWS CloudFormation User Guide.
--
--
--     * @IN_SYNC@ : The resources's actual configuration matches its expected configuration.
--
--
  , lastCheckTimestamp :: Core.Maybe Core.UTCTime
    -- ^ When AWS CloudFormation last checked if the resource had drifted from its expected configuration.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'StackResourceDriftInformationSummary' value with any optional fields omitted.
mkStackResourceDriftInformationSummary
    :: Types.StackResourceDriftStatus -- ^ 'stackResourceDriftStatus'
    -> StackResourceDriftInformationSummary
mkStackResourceDriftInformationSummary stackResourceDriftStatus
  = StackResourceDriftInformationSummary'{stackResourceDriftStatus,
                                          lastCheckTimestamp = Core.Nothing}

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
-- Any resources that do not currently support drift detection have a status of @NOT_CHECKED@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> . If you performed an 'ContinueUpdateRollback' operation on a stack, any resources included in @ResourcesToSkip@ will also have a status of @NOT_CHECKED@ . For more information on skipping resources during rollback operations, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-updating-stacks-continueupdaterollback.html Continue Rolling Back an Update> in the AWS CloudFormation User Guide.
--
--
--     * @IN_SYNC@ : The resources's actual configuration matches its expected configuration.
--
--
--
-- /Note:/ Consider using 'stackResourceDriftStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdisStackResourceDriftStatus :: Lens.Lens' StackResourceDriftInformationSummary Types.StackResourceDriftStatus
srdisStackResourceDriftStatus = Lens.field @"stackResourceDriftStatus"
{-# INLINEABLE srdisStackResourceDriftStatus #-}
{-# DEPRECATED stackResourceDriftStatus "Use generic-lens or generic-optics with 'stackResourceDriftStatus' instead"  #-}

-- | When AWS CloudFormation last checked if the resource had drifted from its expected configuration.
--
-- /Note:/ Consider using 'lastCheckTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdisLastCheckTimestamp :: Lens.Lens' StackResourceDriftInformationSummary (Core.Maybe Core.UTCTime)
srdisLastCheckTimestamp = Lens.field @"lastCheckTimestamp"
{-# INLINEABLE srdisLastCheckTimestamp #-}
{-# DEPRECATED lastCheckTimestamp "Use generic-lens or generic-optics with 'lastCheckTimestamp' instead"  #-}

instance Core.FromXML StackResourceDriftInformationSummary where
        parseXML x
          = StackResourceDriftInformationSummary' Core.<$>
              (x Core..@ "StackResourceDriftStatus") Core.<*>
                x Core..@? "LastCheckTimestamp"
