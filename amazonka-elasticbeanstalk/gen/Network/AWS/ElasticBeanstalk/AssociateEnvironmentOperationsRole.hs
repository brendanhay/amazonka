{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticBeanstalk.AssociateEnvironmentOperationsRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Add or change the operations role used by an environment. After this
-- call is made, Elastic Beanstalk uses the associated operations role for
-- permissions to downstream services during subsequent calls acting on
-- this environment. For more information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles>
-- in the /AWS Elastic Beanstalk Developer Guide/.
module Network.AWS.ElasticBeanstalk.AssociateEnvironmentOperationsRole
  ( -- * Creating a Request
    AssociateEnvironmentOperationsRole (..),
    newAssociateEnvironmentOperationsRole,

    -- * Request Lenses
    associateEnvironmentOperationsRole_environmentName,
    associateEnvironmentOperationsRole_operationsRole,

    -- * Destructuring the Response
    AssociateEnvironmentOperationsRoleResponse (..),
    newAssociateEnvironmentOperationsRoleResponse,
  )
where

import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to add or change the operations role used by an environment.
--
-- /See:/ 'newAssociateEnvironmentOperationsRole' smart constructor.
data AssociateEnvironmentOperationsRole = AssociateEnvironmentOperationsRole'
  { -- | The name of the environment to which to set the operations role.
    environmentName :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an existing IAM role to be used as the
    -- environment\'s operations role.
    operationsRole :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateEnvironmentOperationsRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentName', 'associateEnvironmentOperationsRole_environmentName' - The name of the environment to which to set the operations role.
--
-- 'operationsRole', 'associateEnvironmentOperationsRole_operationsRole' - The Amazon Resource Name (ARN) of an existing IAM role to be used as the
-- environment\'s operations role.
newAssociateEnvironmentOperationsRole ::
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'operationsRole'
  Prelude.Text ->
  AssociateEnvironmentOperationsRole
newAssociateEnvironmentOperationsRole
  pEnvironmentName_
  pOperationsRole_ =
    AssociateEnvironmentOperationsRole'
      { environmentName =
          pEnvironmentName_,
        operationsRole = pOperationsRole_
      }

-- | The name of the environment to which to set the operations role.
associateEnvironmentOperationsRole_environmentName :: Lens.Lens' AssociateEnvironmentOperationsRole Prelude.Text
associateEnvironmentOperationsRole_environmentName = Lens.lens (\AssociateEnvironmentOperationsRole' {environmentName} -> environmentName) (\s@AssociateEnvironmentOperationsRole' {} a -> s {environmentName = a} :: AssociateEnvironmentOperationsRole)

-- | The Amazon Resource Name (ARN) of an existing IAM role to be used as the
-- environment\'s operations role.
associateEnvironmentOperationsRole_operationsRole :: Lens.Lens' AssociateEnvironmentOperationsRole Prelude.Text
associateEnvironmentOperationsRole_operationsRole = Lens.lens (\AssociateEnvironmentOperationsRole' {operationsRole} -> operationsRole) (\s@AssociateEnvironmentOperationsRole' {} a -> s {operationsRole = a} :: AssociateEnvironmentOperationsRole)

instance
  Prelude.AWSRequest
    AssociateEnvironmentOperationsRole
  where
  type
    Rs AssociateEnvironmentOperationsRole =
      AssociateEnvironmentOperationsRoleResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      AssociateEnvironmentOperationsRoleResponse'

instance
  Prelude.Hashable
    AssociateEnvironmentOperationsRole

instance
  Prelude.NFData
    AssociateEnvironmentOperationsRole

instance
  Prelude.ToHeaders
    AssociateEnvironmentOperationsRole
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Prelude.ToPath
    AssociateEnvironmentOperationsRole
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    AssociateEnvironmentOperationsRole
  where
  toQuery AssociateEnvironmentOperationsRole' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ( "AssociateEnvironmentOperationsRole" ::
                         Prelude.ByteString
                     ),
        "Version"
          Prelude.=: ("2010-12-01" :: Prelude.ByteString),
        "EnvironmentName" Prelude.=: environmentName,
        "OperationsRole" Prelude.=: operationsRole
      ]

-- | /See:/ 'newAssociateEnvironmentOperationsRoleResponse' smart constructor.
data AssociateEnvironmentOperationsRoleResponse = AssociateEnvironmentOperationsRoleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AssociateEnvironmentOperationsRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newAssociateEnvironmentOperationsRoleResponse ::
  AssociateEnvironmentOperationsRoleResponse
newAssociateEnvironmentOperationsRoleResponse =
  AssociateEnvironmentOperationsRoleResponse'

instance
  Prelude.NFData
    AssociateEnvironmentOperationsRoleResponse
