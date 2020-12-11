{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.DisassociateEnvironmentOperationsRole
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociate the operations role from an environment. After this call is made, Elastic Beanstalk uses the caller's permissions for permissions to downstream services during subsequent calls acting on this environment. For more information, see <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles> in the /AWS Elastic Beanstalk Developer Guide/ .
module Network.AWS.ElasticBeanstalk.DisassociateEnvironmentOperationsRole
  ( -- * Creating a request
    DisassociateEnvironmentOperationsRole (..),
    mkDisassociateEnvironmentOperationsRole,

    -- ** Request lenses
    deorEnvironmentName,

    -- * Destructuring the response
    DisassociateEnvironmentOperationsRoleResponse (..),
    mkDisassociateEnvironmentOperationsRoleResponse,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Request to disassociate the operations role from an environment.
--
-- /See:/ 'mkDisassociateEnvironmentOperationsRole' smart constructor.
newtype DisassociateEnvironmentOperationsRole = DisassociateEnvironmentOperationsRole'
  { environmentName ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DisassociateEnvironmentOperationsRole' with the minimum fields required to make a request.
--
-- * 'environmentName' - The name of the environment from which to disassociate the operations role.
mkDisassociateEnvironmentOperationsRole ::
  -- | 'environmentName'
  Lude.Text ->
  DisassociateEnvironmentOperationsRole
mkDisassociateEnvironmentOperationsRole pEnvironmentName_ =
  DisassociateEnvironmentOperationsRole'
    { environmentName =
        pEnvironmentName_
    }

-- | The name of the environment from which to disassociate the operations role.
--
-- /Note:/ Consider using 'environmentName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
deorEnvironmentName :: Lens.Lens' DisassociateEnvironmentOperationsRole Lude.Text
deorEnvironmentName = Lens.lens (environmentName :: DisassociateEnvironmentOperationsRole -> Lude.Text) (\s a -> s {environmentName = a} :: DisassociateEnvironmentOperationsRole)
{-# DEPRECATED deorEnvironmentName "Use generic-lens or generic-optics with 'environmentName' instead." #-}

instance Lude.AWSRequest DisassociateEnvironmentOperationsRole where
  type
    Rs DisassociateEnvironmentOperationsRole =
      DisassociateEnvironmentOperationsRoleResponse
  request = Req.postQuery elasticBeanstalkService
  response =
    Res.receiveNull DisassociateEnvironmentOperationsRoleResponse'

instance Lude.ToHeaders DisassociateEnvironmentOperationsRole where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DisassociateEnvironmentOperationsRole where
  toPath = Lude.const "/"

instance Lude.ToQuery DisassociateEnvironmentOperationsRole where
  toQuery DisassociateEnvironmentOperationsRole' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DisassociateEnvironmentOperationsRole" :: Lude.ByteString),
        "Version" Lude.=: ("2010-12-01" :: Lude.ByteString),
        "EnvironmentName" Lude.=: environmentName
      ]

-- | /See:/ 'mkDisassociateEnvironmentOperationsRoleResponse' smart constructor.
data DisassociateEnvironmentOperationsRoleResponse = DisassociateEnvironmentOperationsRoleResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DisassociateEnvironmentOperationsRoleResponse' with the minimum fields required to make a request.
mkDisassociateEnvironmentOperationsRoleResponse ::
  DisassociateEnvironmentOperationsRoleResponse
mkDisassociateEnvironmentOperationsRoleResponse =
  DisassociateEnvironmentOperationsRoleResponse'
