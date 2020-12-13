{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.RDS.AddRoleToDBInstance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Associates an AWS Identity and Access Management (IAM) role with a DB instance.
module Network.AWS.RDS.AddRoleToDBInstance
  ( -- * Creating a request
    AddRoleToDBInstance (..),
    mkAddRoleToDBInstance,

    -- ** Request lenses
    artdiDBInstanceIdentifier,
    artdiFeatureName,
    artdiRoleARN,

    -- * Destructuring the response
    AddRoleToDBInstanceResponse (..),
    mkAddRoleToDBInstanceResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.RDS.Types
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkAddRoleToDBInstance' smart constructor.
data AddRoleToDBInstance = AddRoleToDBInstance'
  { -- | The name of the DB instance to associate the IAM role with.
    dbInstanceIdentifier :: Lude.Text,
    -- | The name of the feature for the DB instance that the IAM role is to be associated with. For the list of supported feature names, see 'DBEngineVersion' .
    featureName :: Lude.Text,
    -- | The Amazon Resource Name (ARN) of the IAM role to associate with the DB instance, for example @arn:aws:iam::123456789012:role/AccessRole@ .
    roleARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddRoleToDBInstance' with the minimum fields required to make a request.
--
-- * 'dbInstanceIdentifier' - The name of the DB instance to associate the IAM role with.
-- * 'featureName' - The name of the feature for the DB instance that the IAM role is to be associated with. For the list of supported feature names, see 'DBEngineVersion' .
-- * 'roleARN' - The Amazon Resource Name (ARN) of the IAM role to associate with the DB instance, for example @arn:aws:iam::123456789012:role/AccessRole@ .
mkAddRoleToDBInstance ::
  -- | 'dbInstanceIdentifier'
  Lude.Text ->
  -- | 'featureName'
  Lude.Text ->
  -- | 'roleARN'
  Lude.Text ->
  AddRoleToDBInstance
mkAddRoleToDBInstance
  pDBInstanceIdentifier_
  pFeatureName_
  pRoleARN_ =
    AddRoleToDBInstance'
      { dbInstanceIdentifier =
          pDBInstanceIdentifier_,
        featureName = pFeatureName_,
        roleARN = pRoleARN_
      }

-- | The name of the DB instance to associate the IAM role with.
--
-- /Note:/ Consider using 'dbInstanceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artdiDBInstanceIdentifier :: Lens.Lens' AddRoleToDBInstance Lude.Text
artdiDBInstanceIdentifier = Lens.lens (dbInstanceIdentifier :: AddRoleToDBInstance -> Lude.Text) (\s a -> s {dbInstanceIdentifier = a} :: AddRoleToDBInstance)
{-# DEPRECATED artdiDBInstanceIdentifier "Use generic-lens or generic-optics with 'dbInstanceIdentifier' instead." #-}

-- | The name of the feature for the DB instance that the IAM role is to be associated with. For the list of supported feature names, see 'DBEngineVersion' .
--
-- /Note:/ Consider using 'featureName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artdiFeatureName :: Lens.Lens' AddRoleToDBInstance Lude.Text
artdiFeatureName = Lens.lens (featureName :: AddRoleToDBInstance -> Lude.Text) (\s a -> s {featureName = a} :: AddRoleToDBInstance)
{-# DEPRECATED artdiFeatureName "Use generic-lens or generic-optics with 'featureName' instead." #-}

-- | The Amazon Resource Name (ARN) of the IAM role to associate with the DB instance, for example @arn:aws:iam::123456789012:role/AccessRole@ .
--
-- /Note:/ Consider using 'roleARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
artdiRoleARN :: Lens.Lens' AddRoleToDBInstance Lude.Text
artdiRoleARN = Lens.lens (roleARN :: AddRoleToDBInstance -> Lude.Text) (\s a -> s {roleARN = a} :: AddRoleToDBInstance)
{-# DEPRECATED artdiRoleARN "Use generic-lens or generic-optics with 'roleARN' instead." #-}

instance Lude.AWSRequest AddRoleToDBInstance where
  type Rs AddRoleToDBInstance = AddRoleToDBInstanceResponse
  request = Req.postQuery rdsService
  response = Res.receiveNull AddRoleToDBInstanceResponse'

instance Lude.ToHeaders AddRoleToDBInstance where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AddRoleToDBInstance where
  toPath = Lude.const "/"

instance Lude.ToQuery AddRoleToDBInstance where
  toQuery AddRoleToDBInstance' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("AddRoleToDBInstance" :: Lude.ByteString),
        "Version" Lude.=: ("2014-10-31" :: Lude.ByteString),
        "DBInstanceIdentifier" Lude.=: dbInstanceIdentifier,
        "FeatureName" Lude.=: featureName,
        "RoleArn" Lude.=: roleARN
      ]

-- | /See:/ 'mkAddRoleToDBInstanceResponse' smart constructor.
data AddRoleToDBInstanceResponse = AddRoleToDBInstanceResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddRoleToDBInstanceResponse' with the minimum fields required to make a request.
mkAddRoleToDBInstanceResponse ::
  AddRoleToDBInstanceResponse
mkAddRoleToDBInstanceResponse = AddRoleToDBInstanceResponse'
