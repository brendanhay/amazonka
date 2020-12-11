{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.RemoveRoleFromDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociates an AWS Identity and Access Management (IAM) role from a DB instance.
module Network.AWS.RDS.RemoveRoleFromDBInstance
  ( -- * Creating a request
    RemoveRoleFromDBInstance (..),
    mkRemoveRoleFromDBInstance,

    -- ** Request lenses
    rrfdiDBInstanceIdentifier,
    rrfdiRoleARN,
    rrfdiFeatureName,

    -- * Destructuring the response
    RemoveRoleFromDBInstanceResponse (..),
    mkRemoveRoleFromDBInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkRemoveRoleFromDBInstance' smart constructor.
data RemoveRoleFromDBInstance = RemoveRoleFromDBInstance'
  { dbInstanceIdentifier ::
      Lude.Text,
    roleARN :: Lude.Text,
    featureName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveRoleFromDBInstance' with the minimum fields required to make a request.
--
-- * 'dbInstanceIdentifier' - The name of the DB instance to disassociate the IAM role from.
-- * 'featureName' - The name of the feature for the DB instance that the IAM role is to be disassociated from. For the list of supported feature names, see @DBEngineVersion@ .
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role to disassociate from the DB instance, for example @arn:aws:iam::123456789012:role/AccessRole@ .
mkRemoveRoleFromDBInstance ::
  -- | 'dbInstanceIdentifier'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  -- | 'featureName'
  Lude.Text ->
  RemoveRoleFromDBInstance
mkRemoveRoleFromDBInstance
  pDBInstanceIdentifier_
  pRoleARN_
  pFeatureName_ =
    RemoveRoleFromDBInstance'
      { dbInstanceIdentifier =
          pDBInstanceIdentifier_,
        roleARN = pRoleARN_,
        featureName = pFeatureName_
      }

-- | The name of the DB instance to disassociate the IAM role from.
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfdiDBInstanceIdentifier :: Lens.Lens' RemoveRoleFromDBInstance Lude.Text
rrfdiDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: RemoveRoleFromDBInstance -> Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: RemoveRoleFromDBInstance)
{-# DEPRECATED rrfdiDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role to disassociate from the DB instance, for example @arn:aws:iam::123456789012:role/AccessRole@ .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfdiRoleARN :: Lens.Lens' RemoveRoleFromDBInstance Lude.Text
rrfdiRoleARN = Lens.lens (roleARN :: RemoveRoleFromDBInstance -> Lude.Text) (\s a -> s {roleARN = a} :: RemoveRoleFromDBInstance)
{-# DEPRECATED rrfdiRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

-- | The name of the feature for the DB instance that the IAM role is to be disassociated from. For the list of supported feature names, see @DBEngineVersion@ .
--
-- /Note:/ Consider using 'featureName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rrfdiFeatureName :: Lens.Lens' RemoveRoleFromDBInstance Lude.Text
rrfdiFeatureName = Lens.lens (featureName :: RemoveRoleFromDBInstance -> Lude.Text) (\s a -> s {featureName = a} :: RemoveRoleFromDBInstance)
{-# DEPRECATED rrfdiFeatureName "Use generic-lens or generic-optics with 'featureName' instead." #-}

instance Lude.AWSRequest RemoveRoleFromDBInstance where
  type Rs RemoveRoleFromDBInstance = RemoveRoleFromDBInstanceResponse
  request = Req.postQuery rdsService
  response = Res.receiveNull RemoveRoleFromDBInstanceResponse'

instance Lude.ToHeaders RemoveRoleFromDBInstance where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath RemoveRoleFromDBInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery RemoveRoleFromDBInstance where
  toQuery RemoveRoleFromDBInstance' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("RemoveRoleFromDBInstance" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBInstanceIdentifier" Lude.=: dbInstanceIdentifier,
        "RoleArn" Lude.=: roleARN,
        "FeatureName" Lude.=: featureName
      ]

-- | /See:/ 'mkRemoveRoleFromDBInstanceResponse' smart constructor.
data RemoveRoleFromDBInstanceResponse = RemoveRoleFromDBInstanceResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'RemoveRoleFromDBInstanceResponse' with the minimum fields required to make a request.
mkRemoveRoleFromDBInstanceResponse ::
  RemoveRoleFromDBInstanceResponse
mkRemoveRoleFromDBInstanceResponse =
  RemoveRoleFromDBInstanceResponse'
