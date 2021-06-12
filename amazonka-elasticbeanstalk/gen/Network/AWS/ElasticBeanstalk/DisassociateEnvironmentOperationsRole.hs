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
-- Module      : Network.AWS.ElasticBeanstalk.DisassociateEnvironmentOperationsRole
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Disassociate the operations role from an environment. After this call is
-- made, Elastic Beanstalk uses the caller\'s permissions for permissions
-- to downstream services during subsequent calls acting on this
-- environment. For more information, see
-- <https://docs.aws.amazon.com/elasticbeanstalk/latest/dg/iam-operationsrole.html Operations roles>
-- in the /AWS Elastic Beanstalk Developer Guide/.
module Network.AWS.ElasticBeanstalk.DisassociateEnvironmentOperationsRole
  ( -- * Creating a Request
    DisassociateEnvironmentOperationsRole (..),
    newDisassociateEnvironmentOperationsRole,

    -- * Request Lenses
    disassociateEnvironmentOperationsRole_environmentName,

    -- * Destructuring the Response
    DisassociateEnvironmentOperationsRoleResponse (..),
    newDisassociateEnvironmentOperationsRoleResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticBeanstalk.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Request to disassociate the operations role from an environment.
--
-- /See:/ 'newDisassociateEnvironmentOperationsRole' smart constructor.
data DisassociateEnvironmentOperationsRole = DisassociateEnvironmentOperationsRole'
  { -- | The name of the environment from which to disassociate the operations
    -- role.
    environmentName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateEnvironmentOperationsRole' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'environmentName', 'disassociateEnvironmentOperationsRole_environmentName' - The name of the environment from which to disassociate the operations
-- role.
newDisassociateEnvironmentOperationsRole ::
  -- | 'environmentName'
  Core.Text ->
  DisassociateEnvironmentOperationsRole
newDisassociateEnvironmentOperationsRole
  pEnvironmentName_ =
    DisassociateEnvironmentOperationsRole'
      { environmentName =
          pEnvironmentName_
      }

-- | The name of the environment from which to disassociate the operations
-- role.
disassociateEnvironmentOperationsRole_environmentName :: Lens.Lens' DisassociateEnvironmentOperationsRole Core.Text
disassociateEnvironmentOperationsRole_environmentName = Lens.lens (\DisassociateEnvironmentOperationsRole' {environmentName} -> environmentName) (\s@DisassociateEnvironmentOperationsRole' {} a -> s {environmentName = a} :: DisassociateEnvironmentOperationsRole)

instance
  Core.AWSRequest
    DisassociateEnvironmentOperationsRole
  where
  type
    AWSResponse
      DisassociateEnvironmentOperationsRole =
      DisassociateEnvironmentOperationsRoleResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull
      DisassociateEnvironmentOperationsRoleResponse'

instance
  Core.Hashable
    DisassociateEnvironmentOperationsRole

instance
  Core.NFData
    DisassociateEnvironmentOperationsRole

instance
  Core.ToHeaders
    DisassociateEnvironmentOperationsRole
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DisassociateEnvironmentOperationsRole
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DisassociateEnvironmentOperationsRole
  where
  toQuery DisassociateEnvironmentOperationsRole' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "DisassociateEnvironmentOperationsRole" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2010-12-01" :: Core.ByteString),
        "EnvironmentName" Core.=: environmentName
      ]

-- | /See:/ 'newDisassociateEnvironmentOperationsRoleResponse' smart constructor.
data DisassociateEnvironmentOperationsRoleResponse = DisassociateEnvironmentOperationsRoleResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DisassociateEnvironmentOperationsRoleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDisassociateEnvironmentOperationsRoleResponse ::
  DisassociateEnvironmentOperationsRoleResponse
newDisassociateEnvironmentOperationsRoleResponse =
  DisassociateEnvironmentOperationsRoleResponse'

instance
  Core.NFData
    DisassociateEnvironmentOperationsRoleResponse
