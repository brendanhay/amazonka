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
-- Module      : Amazonka.Lightsail.AttachInstancesToLoadBalancer
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Attaches one or more Lightsail instances to a load balancer.
--
-- After some time, the instances are attached to the load balancer and the
-- health check status is available.
--
-- The @attach instances to load balancer@ operation supports tag-based
-- access control via resource tags applied to the resource identified by
-- @load balancer name@. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Lightsail Developer Guide>.
module Amazonka.Lightsail.AttachInstancesToLoadBalancer
  ( -- * Creating a Request
    AttachInstancesToLoadBalancer (..),
    newAttachInstancesToLoadBalancer,

    -- * Request Lenses
    attachInstancesToLoadBalancer_loadBalancerName,
    attachInstancesToLoadBalancer_instanceNames,

    -- * Destructuring the Response
    AttachInstancesToLoadBalancerResponse (..),
    newAttachInstancesToLoadBalancerResponse,

    -- * Response Lenses
    attachInstancesToLoadBalancerResponse_operations,
    attachInstancesToLoadBalancerResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newAttachInstancesToLoadBalancer' smart constructor.
data AttachInstancesToLoadBalancer = AttachInstancesToLoadBalancer'
  { -- | The name of the load balancer.
    loadBalancerName :: Prelude.Text,
    -- | An array of strings representing the instance name(s) you want to attach
    -- to your load balancer.
    --
    -- An instance must be @running@ before you can attach it to your load
    -- balancer.
    --
    -- There are no additional limits on the number of instances you can attach
    -- to your load balancer, aside from the limit of Lightsail instances you
    -- can create in your account (20).
    instanceNames :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachInstancesToLoadBalancer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'attachInstancesToLoadBalancer_loadBalancerName' - The name of the load balancer.
--
-- 'instanceNames', 'attachInstancesToLoadBalancer_instanceNames' - An array of strings representing the instance name(s) you want to attach
-- to your load balancer.
--
-- An instance must be @running@ before you can attach it to your load
-- balancer.
--
-- There are no additional limits on the number of instances you can attach
-- to your load balancer, aside from the limit of Lightsail instances you
-- can create in your account (20).
newAttachInstancesToLoadBalancer ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  AttachInstancesToLoadBalancer
newAttachInstancesToLoadBalancer pLoadBalancerName_ =
  AttachInstancesToLoadBalancer'
    { loadBalancerName =
        pLoadBalancerName_,
      instanceNames = Prelude.mempty
    }

-- | The name of the load balancer.
attachInstancesToLoadBalancer_loadBalancerName :: Lens.Lens' AttachInstancesToLoadBalancer Prelude.Text
attachInstancesToLoadBalancer_loadBalancerName = Lens.lens (\AttachInstancesToLoadBalancer' {loadBalancerName} -> loadBalancerName) (\s@AttachInstancesToLoadBalancer' {} a -> s {loadBalancerName = a} :: AttachInstancesToLoadBalancer)

-- | An array of strings representing the instance name(s) you want to attach
-- to your load balancer.
--
-- An instance must be @running@ before you can attach it to your load
-- balancer.
--
-- There are no additional limits on the number of instances you can attach
-- to your load balancer, aside from the limit of Lightsail instances you
-- can create in your account (20).
attachInstancesToLoadBalancer_instanceNames :: Lens.Lens' AttachInstancesToLoadBalancer [Prelude.Text]
attachInstancesToLoadBalancer_instanceNames = Lens.lens (\AttachInstancesToLoadBalancer' {instanceNames} -> instanceNames) (\s@AttachInstancesToLoadBalancer' {} a -> s {instanceNames = a} :: AttachInstancesToLoadBalancer) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    AttachInstancesToLoadBalancer
  where
  type
    AWSResponse AttachInstancesToLoadBalancer =
      AttachInstancesToLoadBalancerResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          AttachInstancesToLoadBalancerResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    AttachInstancesToLoadBalancer
  where
  hashWithSalt _salt AttachInstancesToLoadBalancer' {..} =
    _salt
      `Prelude.hashWithSalt` loadBalancerName
      `Prelude.hashWithSalt` instanceNames

instance Prelude.NFData AttachInstancesToLoadBalancer where
  rnf AttachInstancesToLoadBalancer' {..} =
    Prelude.rnf loadBalancerName `Prelude.seq`
      Prelude.rnf instanceNames

instance Data.ToHeaders AttachInstancesToLoadBalancer where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.AttachInstancesToLoadBalancer" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON AttachInstancesToLoadBalancer where
  toJSON AttachInstancesToLoadBalancer' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("loadBalancerName" Data..= loadBalancerName),
            Prelude.Just
              ("instanceNames" Data..= instanceNames)
          ]
      )

instance Data.ToPath AttachInstancesToLoadBalancer where
  toPath = Prelude.const "/"

instance Data.ToQuery AttachInstancesToLoadBalancer where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newAttachInstancesToLoadBalancerResponse' smart constructor.
data AttachInstancesToLoadBalancerResponse = AttachInstancesToLoadBalancerResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttachInstancesToLoadBalancerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'attachInstancesToLoadBalancerResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'attachInstancesToLoadBalancerResponse_httpStatus' - The response's http status code.
newAttachInstancesToLoadBalancerResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  AttachInstancesToLoadBalancerResponse
newAttachInstancesToLoadBalancerResponse pHttpStatus_ =
  AttachInstancesToLoadBalancerResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
attachInstancesToLoadBalancerResponse_operations :: Lens.Lens' AttachInstancesToLoadBalancerResponse (Prelude.Maybe [Operation])
attachInstancesToLoadBalancerResponse_operations = Lens.lens (\AttachInstancesToLoadBalancerResponse' {operations} -> operations) (\s@AttachInstancesToLoadBalancerResponse' {} a -> s {operations = a} :: AttachInstancesToLoadBalancerResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
attachInstancesToLoadBalancerResponse_httpStatus :: Lens.Lens' AttachInstancesToLoadBalancerResponse Prelude.Int
attachInstancesToLoadBalancerResponse_httpStatus = Lens.lens (\AttachInstancesToLoadBalancerResponse' {httpStatus} -> httpStatus) (\s@AttachInstancesToLoadBalancerResponse' {} a -> s {httpStatus = a} :: AttachInstancesToLoadBalancerResponse)

instance
  Prelude.NFData
    AttachInstancesToLoadBalancerResponse
  where
  rnf AttachInstancesToLoadBalancerResponse' {..} =
    Prelude.rnf operations `Prelude.seq`
      Prelude.rnf httpStatus
