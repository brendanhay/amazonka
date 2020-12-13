{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.Types.DBClusterRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.RDS.Types.DBClusterRole
  ( DBClusterRole (..),

    -- * Smart constructor
    mkDBClusterRole,

    -- * Lenses
    dcrStatus,
    dcrFeatureName,
    dcrRoleARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an AWS Identity and Access Management (IAM) role that is associated with a DB cluster.
--
-- /See:/ 'mkDBClusterRole' smart constructor.
data DBClusterRole = DBClusterRole'
  { -- | Describes the state of association between the IAM role and the DB cluster. The Status property returns one of the following values:
    --
    --
    --     * @ACTIVE@ - the IAM role ARN is associated with the DB cluster and can be used to access other AWS services on your behalf.
    --
    --
    --     * @PENDING@ - the IAM role ARN is being associated with the DB cluster.
    --
    --
    --     * @INVALID@ - the IAM role ARN is associated with the DB cluster, but the DB cluster is unable to assume the IAM role in order to access other AWS services on your behalf.
    status :: Lude.Maybe Lude.Text,
    -- | The name of the feature associated with the AWS Identity and Access Management (IAM) role. For the list of supported feature names, see 'DBEngineVersion' .
    featureName :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role that is associated with the DB cluster.
    roleARN :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DBClusterRole' with the minimum fields required to make a request.
--
-- * 'status' - Describes the state of association between the IAM role and the DB cluster. The Status property returns one of the following values:
--
--
--     * @ACTIVE@ - the IAM role ARN is associated with the DB cluster and can be used to access other AWS services on your behalf.
--
--
--     * @PENDING@ - the IAM role ARN is being associated with the DB cluster.
--
--
--     * @INVALID@ - the IAM role ARN is associated with the DB cluster, but the DB cluster is unable to assume the IAM role in order to access other AWS services on your behalf.
--
--
-- * 'featureName' - The name of the feature associated with the AWS Identity and Access Management (IAM) role. For the list of supported feature names, see 'DBEngineVersion' .
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role that is associated with the DB cluster.
mkDBClusterRole ::
  DBClusterRole
mkDBClusterRole =
  DBClusterRole'
    { status = Lude.Nothing,
      featureName = Lude.Nothing,
      roleARN = Lude.Nothing
    }

-- | Describes the state of association between the IAM role and the DB cluster. The Status property returns one of the following values:
--
--
--     * @ACTIVE@ - the IAM role ARN is associated with the DB cluster and can be used to access other AWS services on your behalf.
--
--
--     * @PENDING@ - the IAM role ARN is being associated with the DB cluster.
--
--
--     * @INVALID@ - the IAM role ARN is associated with the DB cluster, but the DB cluster is unable to assume the IAM role in order to access other AWS services on your behalf.
--
--
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrStatus :: Lens.Lens' DBClusterRole (Lude.Maybe Lude.Text)
dcrStatus = Lens.lens (status :: DBClusterRole -> Lude.Maybe Lude.Text) (\s a -> s {status = a} :: DBClusterRole)
{-# DEPRECATED dcrStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The name of the feature associated with the AWS Identity and Access Management (IAM) role. For the list of supported feature names, see 'DBEngineVersion' .
--
-- /Note:/ Consider using 'featureName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrFeatureName :: Lens.Lens' DBClusterRole (Lude.Maybe Lude.Text)
dcrFeatureName = Lens.lens (featureName :: DBClusterRole -> Lude.Maybe Lude.Text) (\s a -> s {featureName = a} :: DBClusterRole)
{-# DEPRECATED dcrFeatureName "Use generic-lens or generic-optics with 'featureName' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role that is associated with the DB cluster.
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrRoleARN :: Lens.Lens' DBClusterRole (Lude.Maybe Lude.Text)
dcrRoleARN = Lens.lens (roleARN :: DBClusterRole -> Lude.Maybe Lude.Text) (\s a -> s {roleARN = a} :: DBClusterRole)
{-# DEPRECATED dcrRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.FromXML DBClusterRole where
  parseXML x =
    DBClusterRole'
      Lude.<$> (x Lude..@? "Status")
      Lude.<*> (x Lude..@? "FeatureName")
      Lude.<*> (x Lude..@? "RoleArn")
