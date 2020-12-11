{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.AssociateEnvironmentOperationsRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add or change the operations role used by an environment. After this call is made, Elastic Beanstalk uses the associated operations role for permissions to downstream services during subsequent calls acting on this environment. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles> in the /AWS Elastic Beanstalk Developer Guide/ .
module Network.AWS.ElasticBeanstalk.AssociateEnvironmentOperationsRole
  ( -- * Creating a request
    AssociateEnvironmentOperationsRole (..),
    mkAssociateEnvironmentOperationsRole,

    -- ** Request lenses
    aeorEnvironmentName,
    aeorOperationsRole,

    -- * Destructuring the response
    AssociateEnvironmentOperationsRoleResponse (..),
    mkAssociateEnvironmentOperationsRoleResponse,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to add or change the operations role used by an environment.
--
-- /See:/ 'mkAssociateEnvironmentOperationsRole' smart constructor.
data AssociateEnvironmentOperationsRole = AssociateEnvironmentOperationsRole'
  { environmentName ::
      Lude.Text,
    operationsRole ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateEnvironmentOperationsRole' with the minimum fields required to make a request.
--
-- * 'environmentName' - The name of the environment to which to set the operations role.
-- * 'operationsRole' - The Amazon Resource Name (ARN) of an existing IAM role to be used as the environment's operations role.
mkAssociateEnvironmentOperationsRole ::
  -- | 'environmentName'
  Lude.Text ->
  -- | 'operationsRole'
  Lude.Text ->
  AssociateEnvironmentOperationsRole
mkAssociateEnvironmentOperationsRole
  pEnvironmentName_
  pOperationsRole_ =
    AssociateEnvironmentOperationsRole'
      { environmentName =
          pEnvironmentName_,
        operationsRole = pOperationsRole_
      }

-- | The name of the environment to which to set the operations role.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeorEnvironmentName :: Lens.Lens' AssociateEnvironmentOperationsRole Lude.Text
aeorEnvironmentName = Lens.lens (environmentName :: AssociateEnvironmentOperationsRole -> Lude.Text) (\s a -> s {environmentName = a} :: AssociateEnvironmentOperationsRole)
{-# DEPRECATED aeorEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

-- | The Amazon Resource Name (ARN) of an existing IAM role to be used as the environment's operations role.
--
-- /Note:/ Consider using 'operationsRole' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aeorOperationsRole :: Lens.Lens' AssociateEnvironmentOperationsRole Lude.Text
aeorOperationsRole = Lens.lens (operationsRole :: AssociateEnvironmentOperationsRole -> Lude.Text) (\s a -> s {operationsRole = a} :: AssociateEnvironmentOperationsRole)
{-# DEPRECATED aeorOperationsRole "Use generic-lens or generic-optics with 'operationsRole' instead." #-}

instance Lude.AWSRequest AssociateEnvironmentOperationsRole where
  type
    Rs AssociateEnvironmentOperationsRole =
      AssociateEnvironmentOperationsRoleResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveNull AssociateEnvironmentOperationsRoleResponse'

instance Lude.ToHeaders AssociateEnvironmentOperationsRole where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath AssociateEnvironmentOperationsRole where
  toPath = Lude.const "/"

instance Lude.ToQuery AssociateEnvironmentOperationsRole where
  toQuery AssociateEnvironmentOperationsRole' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("AssociateEnvironmentOperationsRole" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "EnvironmentName" Lude.=: environmentName,
        "OperationsRole" Lude.=: operationsRole
      ]

-- | /See:/ 'mkAssociateEnvironmentOperationsRoleResponse' smart constructor.
data AssociateEnvironmentOperationsRoleResponse = AssociateEnvironmentOperationsRoleResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AssociateEnvironmentOperationsRoleResponse' with the minimum fields required to make a request.
mkAssociateEnvironmentOperationsRoleResponse ::
  AssociateEnvironmentOperationsRoleResponse
mkAssociateEnvironmentOperationsRoleResponse =
  AssociateEnvironmentOperationsRoleResponse'
