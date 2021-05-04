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
-- Module      : Network.AWS.ELBv2.DeleteLoadBalancer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Application Load Balancer, Network Load Balancer,
-- or Gateway Load Balancer. Deleting a load balancer also deletes its
-- listeners.
--
-- You can\'t delete a load balancer if deletion protection is enabled. If
-- the load balancer does not exist or has already been deleted, the call
-- succeeds.
--
-- Deleting a load balancer does not affect its registered targets. For
-- example, your EC2 instances continue to run and are still registered to
-- their target groups. If you no longer need these EC2 instances, you can
-- stop or terminate them.
module Network.AWS.ELBv2.DeleteLoadBalancer
  ( -- * Creating a Request
    DeleteLoadBalancer (..),
    newDeleteLoadBalancer,

    -- * Request Lenses
    deleteLoadBalancer_loadBalancerArn,

    -- * Destructuring the Response
    DeleteLoadBalancerResponse (..),
    newDeleteLoadBalancerResponse,

    -- * Response Lenses
    deleteLoadBalancerResponse_httpStatus,
  )
where

import Network.AWS.ELBv2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteLoadBalancer' smart constructor.
data DeleteLoadBalancer = DeleteLoadBalancer'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerArn', 'deleteLoadBalancer_loadBalancerArn' - The Amazon Resource Name (ARN) of the load balancer.
newDeleteLoadBalancer ::
  -- | 'loadBalancerArn'
  Prelude.Text ->
  DeleteLoadBalancer
newDeleteLoadBalancer pLoadBalancerArn_ =
  DeleteLoadBalancer'
    { loadBalancerArn =
        pLoadBalancerArn_
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
deleteLoadBalancer_loadBalancerArn :: Lens.Lens' DeleteLoadBalancer Prelude.Text
deleteLoadBalancer_loadBalancerArn = Lens.lens (\DeleteLoadBalancer' {loadBalancerArn} -> loadBalancerArn) (\s@DeleteLoadBalancer' {} a -> s {loadBalancerArn = a} :: DeleteLoadBalancer)

instance Prelude.AWSRequest DeleteLoadBalancer where
  type
    Rs DeleteLoadBalancer =
      DeleteLoadBalancerResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteLoadBalancerResult"
      ( \s h x ->
          DeleteLoadBalancerResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteLoadBalancer

instance Prelude.NFData DeleteLoadBalancer

instance Prelude.ToHeaders DeleteLoadBalancer where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath DeleteLoadBalancer where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteLoadBalancer where
  toQuery DeleteLoadBalancer' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("DeleteLoadBalancer" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2015-12-01" :: Prelude.ByteString),
        "LoadBalancerArn" Prelude.=: loadBalancerArn
      ]

-- | /See:/ 'newDeleteLoadBalancerResponse' smart constructor.
data DeleteLoadBalancerResponse = DeleteLoadBalancerResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteLoadBalancerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteLoadBalancerResponse_httpStatus' - The response's http status code.
newDeleteLoadBalancerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteLoadBalancerResponse
newDeleteLoadBalancerResponse pHttpStatus_ =
  DeleteLoadBalancerResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteLoadBalancerResponse_httpStatus :: Lens.Lens' DeleteLoadBalancerResponse Prelude.Int
deleteLoadBalancerResponse_httpStatus = Lens.lens (\DeleteLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@DeleteLoadBalancerResponse' {} a -> s {httpStatus = a} :: DeleteLoadBalancerResponse)

instance Prelude.NFData DeleteLoadBalancerResponse
