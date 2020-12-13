{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudFormation.Types.StackResourceDriftInformationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudFormation.Types.StackResourceDriftInformationSummary
  ( StackResourceDriftInformationSummary (..),

    -- * Smart constructor
    mkStackResourceDriftInformationSummary,

    -- * Lenses
    srdisLastCheckTimestamp,
    srdisStackResourceDriftStatus,
  )
where

import Network.AWS.CloudFormation.Types.StackResourceDriftStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Summarizes information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration.
--
-- /See:/ 'mkStackResourceDriftInformationSummary' smart constructor.
data StackResourceDriftInformationSummary = StackResourceDriftInformationSummary'
  { -- | When AWS CloudFormation last checked if the resource had drifted from its expected configuration.
    lastCheckTimestamp :: Lude.Maybe Lude.DateTime,
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
    stackResourceDriftStatus :: StackResourceDriftStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackResourceDriftInformationSummary' with the minimum fields required to make a request.
--
-- * 'lastCheckTimestamp' - When AWS CloudFormation last checked if the resource had drifted from its expected configuration.
-- * 'stackResourceDriftStatus' - Status of the resource's actual configuration compared to its expected configuration
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
mkStackResourceDriftInformationSummary ::
  -- | 'stackResourceDriftStatus'
  StackResourceDriftStatus ->
  StackResourceDriftInformationSummary
mkStackResourceDriftInformationSummary pStackResourceDriftStatus_ =
  StackResourceDriftInformationSummary'
    { lastCheckTimestamp =
        Lude.Nothing,
      stackResourceDriftStatus = pStackResourceDriftStatus_
    }

-- | When AWS CloudFormation last checked if the resource had drifted from its expected configuration.
--
-- /Note:/ Consider using 'lastCheckTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdisLastCheckTimestamp :: Lens.Lens' StackResourceDriftInformationSummary (Lude.Maybe Lude.DateTime)
srdisLastCheckTimestamp = Lens.lens (lastCheckTimestamp :: StackResourceDriftInformationSummary -> Lude.Maybe Lude.DateTime) (\s a -> s {lastCheckTimestamp = a} :: StackResourceDriftInformationSummary)
{-# DEPRECATED srdisLastCheckTimestamp "Use generic-lens or generic-optics with 'lastCheckTimestamp' instead." #-}

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
srdisStackResourceDriftStatus :: Lens.Lens' StackResourceDriftInformationSummary StackResourceDriftStatus
srdisStackResourceDriftStatus = Lens.lens (stackResourceDriftStatus :: StackResourceDriftInformationSummary -> StackResourceDriftStatus) (\s a -> s {stackResourceDriftStatus = a} :: StackResourceDriftInformationSummary)
{-# DEPRECATED srdisStackResourceDriftStatus "Use generic-lens or generic-optics with 'stackResourceDriftStatus' instead." #-}

instance Lude.FromXML StackResourceDriftInformationSummary where
  parseXML x =
    StackResourceDriftInformationSummary'
      Lude.<$> (x Lude..@? "LastCheckTimestamp")
      Lude.<*> (x Lude..@ "StackResourceDriftStatus")
