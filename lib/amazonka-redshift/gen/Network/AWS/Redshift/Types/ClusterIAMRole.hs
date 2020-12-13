{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.ClusterIAMRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.ClusterIAMRole
  ( ClusterIAMRole (..),

    -- * Smart constructor
    mkClusterIAMRole,

    -- * Lenses
    cirIAMRoleARN,
    cirApplyStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Redshift.Internal

-- | An AWS Identity and Access Management (IAM) role that can be used by the associated Amazon Redshift cluster to access other AWS services.
--
-- /See:/ 'mkClusterIAMRole' smart constructor.
data ClusterIAMRole = ClusterIAMRole'
  { -- | The Amazon Resource Name (ARN) of the IAM role, for example, @arn:aws:iam::123456789012:role/RedshiftCopyUnload@ .
    iamRoleARN :: Lude.Maybe Lude.Text,
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
    applyStatus :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ClusterIAMRole' with the minimum fields required to make a request.
--
-- * 'iamRoleARN' - The Amazon Resource Name (ARN) of the IAM role, for example, @arn:aws:iam::123456789012:role/RedshiftCopyUnload@ .
-- * 'applyStatus' - A value that describes the status of the IAM role's association with an Amazon Redshift cluster.
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
mkClusterIAMRole ::
  ClusterIAMRole
mkClusterIAMRole =
  ClusterIAMRole'
    { iamRoleARN = Lude.Nothing,
      applyStatus = Lude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the IAM role, for example, @arn:aws:iam::123456789012:role/RedshiftCopyUnload@ .
--
-- /Note:/ Consider using 'iamRoleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cirIAMRoleARN :: Lens.Lens' ClusterIAMRole (Lude.Maybe Lude.Text)
cirIAMRoleARN = Lens.lens (iamRoleARN :: ClusterIAMRole -> Lude.Maybe Lude.Text) (\s a -> s {iamRoleARN = a} :: ClusterIAMRole)
{-# DEPRECATED cirIAMRoleARN "Use generic-lens or generic-optics with 'iamRoleARN' instead." #-}

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
cirApplyStatus :: Lens.Lens' ClusterIAMRole (Lude.Maybe Lude.Text)
cirApplyStatus = Lens.lens (applyStatus :: ClusterIAMRole -> Lude.Maybe Lude.Text) (\s a -> s {applyStatus = a} :: ClusterIAMRole)
{-# DEPRECATED cirApplyStatus "Use generic-lens or generic-optics with 'applyStatus' instead." #-}

instance Lude.FromXML ClusterIAMRole where
  parseXML x =
    ClusterIAMRole'
      Lude.<$> (x Lude..@? "IamRoleArn") Lude.<*> (x Lude..@? "ApplyStatus")
