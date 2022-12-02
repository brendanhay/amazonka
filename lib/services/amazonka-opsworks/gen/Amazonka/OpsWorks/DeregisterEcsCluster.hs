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
-- Module      : Amazonka.OpsWorks.DeregisterEcsCluster
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deregisters a specified Amazon ECS cluster from a stack. For more
-- information, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/workinglayers-ecscluster.html#workinglayers-ecscluster-delete Resource Management>.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Manage permissions level for the stack or an attached policy that
-- explicitly grants permissions. For more information on user permissions,
-- see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html>.
module Amazonka.OpsWorks.DeregisterEcsCluster
  ( -- * Creating a Request
    DeregisterEcsCluster (..),
    newDeregisterEcsCluster,

    -- * Request Lenses
    deregisterEcsCluster_ecsClusterArn,

    -- * Destructuring the Response
    DeregisterEcsClusterResponse (..),
    newDeregisterEcsClusterResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeregisterEcsCluster' smart constructor.
data DeregisterEcsCluster = DeregisterEcsCluster'
  { -- | The cluster\'s Amazon Resource Number (ARN).
    ecsClusterArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterEcsCluster' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ecsClusterArn', 'deregisterEcsCluster_ecsClusterArn' - The cluster\'s Amazon Resource Number (ARN).
newDeregisterEcsCluster ::
  -- | 'ecsClusterArn'
  Prelude.Text ->
  DeregisterEcsCluster
newDeregisterEcsCluster pEcsClusterArn_ =
  DeregisterEcsCluster'
    { ecsClusterArn =
        pEcsClusterArn_
    }

-- | The cluster\'s Amazon Resource Number (ARN).
deregisterEcsCluster_ecsClusterArn :: Lens.Lens' DeregisterEcsCluster Prelude.Text
deregisterEcsCluster_ecsClusterArn = Lens.lens (\DeregisterEcsCluster' {ecsClusterArn} -> ecsClusterArn) (\s@DeregisterEcsCluster' {} a -> s {ecsClusterArn = a} :: DeregisterEcsCluster)

instance Core.AWSRequest DeregisterEcsCluster where
  type
    AWSResponse DeregisterEcsCluster =
      DeregisterEcsClusterResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeregisterEcsClusterResponse'

instance Prelude.Hashable DeregisterEcsCluster where
  hashWithSalt _salt DeregisterEcsCluster' {..} =
    _salt `Prelude.hashWithSalt` ecsClusterArn

instance Prelude.NFData DeregisterEcsCluster where
  rnf DeregisterEcsCluster' {..} =
    Prelude.rnf ecsClusterArn

instance Data.ToHeaders DeregisterEcsCluster where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.DeregisterEcsCluster" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeregisterEcsCluster where
  toJSON DeregisterEcsCluster' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("EcsClusterArn" Data..= ecsClusterArn)
          ]
      )

instance Data.ToPath DeregisterEcsCluster where
  toPath = Prelude.const "/"

instance Data.ToQuery DeregisterEcsCluster where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeregisterEcsClusterResponse' smart constructor.
data DeregisterEcsClusterResponse = DeregisterEcsClusterResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeregisterEcsClusterResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeregisterEcsClusterResponse ::
  DeregisterEcsClusterResponse
newDeregisterEcsClusterResponse =
  DeregisterEcsClusterResponse'

instance Prelude.NFData DeregisterEcsClusterResponse where
  rnf _ = ()
