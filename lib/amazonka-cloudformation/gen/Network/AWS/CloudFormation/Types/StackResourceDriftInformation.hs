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
    srdiLastCheckTimestamp,
    srdiStackResourceDriftStatus,
  )
where

import Network.AWS.CloudFormation.Types.StackResourceDriftStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about whether the resource's actual configuration differs, or has /drifted/ , from its expected configuration.
--
-- /See:/ 'mkStackResourceDriftInformation' smart constructor.
data StackResourceDriftInformation = StackResourceDriftInformation'
  { lastCheckTimestamp ::
      Lude.Maybe Lude.ISO8601,
    stackResourceDriftStatus ::
      StackResourceDriftStatus
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StackResourceDriftInformation' with the minimum fields required to make a request.
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
-- Any resources that do not currently support drift detection have a status of @NOT_CHECKED@ . For more information, see <https://docs.aws.amazon.com/AWSCloudFormation/latest/UserGuide/using-cfn-stack-drift-resource-list.html Resources that Support Drift Detection> .
--
--
--     * @IN_SYNC@ : The resources's actual configuration matches its expected configuration.
mkStackResourceDriftInformation ::
  -- | 'stackResourceDriftStatus'
  StackResourceDriftStatus ->
  StackResourceDriftInformation
mkStackResourceDriftInformation pStackResourceDriftStatus_ =
  StackResourceDriftInformation'
    { lastCheckTimestamp = Lude.Nothing,
      stackResourceDriftStatus = pStackResourceDriftStatus_
    }

-- | When AWS CloudFormation last checked if the resource had drifted from its expected configuration.
--
-- /Note:/ Consider using 'lastCheckTimestamp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srdiLastCheckTimestamp :: Lens.Lens' StackResourceDriftInformation (Lude.Maybe Lude.ISO8601)
srdiLastCheckTimestamp = Lens.lens (lastCheckTimestamp :: StackResourceDriftInformation -> Lude.Maybe Lude.ISO8601) (\s a -> s {lastCheckTimestamp = a} :: StackResourceDriftInformation)
{-# DEPRECATED srdiLastCheckTimestamp "Use generic-lens or generic-optics with 'lastCheckTimestamp' instead." #-}

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
srdiStackResourceDriftStatus :: Lens.Lens' StackResourceDriftInformation StackResourceDriftStatus
srdiStackResourceDriftStatus = Lens.lens (stackResourceDriftStatus :: StackResourceDriftInformation -> StackResourceDriftStatus) (\s a -> s {stackResourceDriftStatus = a} :: StackResourceDriftInformation)
{-# DEPRECATED srdiStackResourceDriftStatus "Use generic-lens or generic-optics with 'stackResourceDriftStatus' instead." #-}

instance Lude.FromXML StackResourceDriftInformation where
  parseXML x =
    StackResourceDriftInformation'
      Lude.<$> (x Lude..@? "LastCheckTimestamp")
      Lude.<*> (x Lude..@ "StackResourceDriftStatus")
