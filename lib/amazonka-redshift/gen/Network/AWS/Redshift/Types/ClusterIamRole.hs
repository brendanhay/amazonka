{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterIamRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.ClusterIamRole
  ( ClusterIamRole (..)
  -- * Smart constructor
  , mkClusterIamRole
  -- * Lenses
  , cirApplyStatus
  , cirIamRoleArn
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types

-- | An AWS Identity and Access Management (IAM) role that can be used by the associated Amazon Redshift cluster to access other AWS services.
--
-- /See:/ 'mkClusterIamRole' smart constructor.
data ClusterIamRole = ClusterIamRole'
  { applyStatus :: Core.Maybe Core.Text
    -- ^ A value that describes the status of the IAM role's association with an Amazon Redshift cluster.
--
-- The following are possible statuses and descriptions.
--
--     * @in-sync@ : The role is available for use by the cluster.
--
--
--     * @adding@ : The role is in the process of being associated with the cluster.
--
--
--     * @removing@ : The role is in the process of being disassociated with the cluster.
--
--
  , iamRoleArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) of the IAM role, for example, @arn:aws:iam::123456789012:role/RedshiftCopyUnload@ . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ClusterIamRole' value with any optional fields omitted.
mkClusterIamRole
    :: ClusterIamRole
mkClusterIamRole
  = ClusterIamRole'{applyStatus = Core.Nothing,
                    iamRoleArn = Core.Nothing}

-- | A value that describes the status of the IAM role's association with an Amazon Redshift cluster.
--
-- The following are possible statuses and descriptions.
--
--     * @in-sync@ : The role is available for use by the cluster.
--
--
--     * @adding@ : The role is in the process of being associated with the cluster.
--
--
--     * @removing@ : The role is in the process of being disassociated with the cluster.
--
--
--
-- /Note:/ Consider using 'applyStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirApplyStatus :: Lens.Lens' ClusterIamRole (Core.Maybe Core.Text)
cirApplyStatus = Lens.field @"applyStatus"
{-# INLINEABLE cirApplyStatus #-}
{-# DEPRECATED applyStatus "Use generic-lens or generic-optics with 'applyStatus' instead"  #-}

-- | The Amazon Resource Name (ARN) of the IAM role, for example, @arn:aws:iam::123456789012:role/RedshiftCopyUnload@ . 
--
-- /Note:/ Consider using 'iamRoleArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirIamRoleArn :: Lens.Lens' ClusterIamRole (Core.Maybe Core.Text)
cirIamRoleArn = Lens.field @"iamRoleArn"
{-# INLINEABLE cirIamRoleArn #-}
{-# DEPRECATED iamRoleArn "Use generic-lens or generic-optics with 'iamRoleArn' instead"  #-}

instance Core.FromXML ClusterIamRole where
        parseXML x
          = ClusterIamRole' Core.<$>
              (x Core..@? "ApplyStatus") Core.<*> x Core..@? "IamRoleArn"
