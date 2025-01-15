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
-- Module      : Amazonka.ELBV2.ModifyLoadBalancerAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified attributes of the specified Application Load
-- Balancer, Network Load Balancer, or Gateway Load Balancer.
--
-- If any of the specified attributes can\'t be modified as requested, the
-- call fails. Any existing attributes that you do not modify retain their
-- current values.
module Amazonka.ELBV2.ModifyLoadBalancerAttributes
  ( -- * Creating a Request
    ModifyLoadBalancerAttributes (..),
    newModifyLoadBalancerAttributes,

    -- * Request Lenses
    modifyLoadBalancerAttributes_loadBalancerArn,
    modifyLoadBalancerAttributes_attributes,

    -- * Destructuring the Response
    ModifyLoadBalancerAttributesResponse (..),
    newModifyLoadBalancerAttributesResponse,

    -- * Response Lenses
    modifyLoadBalancerAttributesResponse_attributes,
    modifyLoadBalancerAttributesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ELBV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyLoadBalancerAttributes' smart constructor.
data ModifyLoadBalancerAttributes = ModifyLoadBalancerAttributes'
  { -- | The Amazon Resource Name (ARN) of the load balancer.
    loadBalancerArn :: Prelude.Text,
    -- | The load balancer attributes.
    attributes :: [LoadBalancerAttribute]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyLoadBalancerAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerArn', 'modifyLoadBalancerAttributes_loadBalancerArn' - The Amazon Resource Name (ARN) of the load balancer.
--
-- 'attributes', 'modifyLoadBalancerAttributes_attributes' - The load balancer attributes.
newModifyLoadBalancerAttributes ::
  -- | 'loadBalancerArn'
  Prelude.Text ->
  ModifyLoadBalancerAttributes
newModifyLoadBalancerAttributes pLoadBalancerArn_ =
  ModifyLoadBalancerAttributes'
    { loadBalancerArn =
        pLoadBalancerArn_,
      attributes = Prelude.mempty
    }

-- | The Amazon Resource Name (ARN) of the load balancer.
modifyLoadBalancerAttributes_loadBalancerArn :: Lens.Lens' ModifyLoadBalancerAttributes Prelude.Text
modifyLoadBalancerAttributes_loadBalancerArn = Lens.lens (\ModifyLoadBalancerAttributes' {loadBalancerArn} -> loadBalancerArn) (\s@ModifyLoadBalancerAttributes' {} a -> s {loadBalancerArn = a} :: ModifyLoadBalancerAttributes)

-- | The load balancer attributes.
modifyLoadBalancerAttributes_attributes :: Lens.Lens' ModifyLoadBalancerAttributes [LoadBalancerAttribute]
modifyLoadBalancerAttributes_attributes = Lens.lens (\ModifyLoadBalancerAttributes' {attributes} -> attributes) (\s@ModifyLoadBalancerAttributes' {} a -> s {attributes = a} :: ModifyLoadBalancerAttributes) Prelude.. Lens.coerced

instance Core.AWSRequest ModifyLoadBalancerAttributes where
  type
    AWSResponse ModifyLoadBalancerAttributes =
      ModifyLoadBalancerAttributesResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyLoadBalancerAttributesResult"
      ( \s h x ->
          ModifyLoadBalancerAttributesResponse'
            Prelude.<$> ( x Data..@? "Attributes" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Data.parseXMLList "member")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ModifyLoadBalancerAttributes
  where
  hashWithSalt _salt ModifyLoadBalancerAttributes' {..} =
    _salt
      `Prelude.hashWithSalt` loadBalancerArn
      `Prelude.hashWithSalt` attributes

instance Prelude.NFData ModifyLoadBalancerAttributes where
  rnf ModifyLoadBalancerAttributes' {..} =
    Prelude.rnf loadBalancerArn `Prelude.seq`
      Prelude.rnf attributes

instance Data.ToHeaders ModifyLoadBalancerAttributes where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyLoadBalancerAttributes where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyLoadBalancerAttributes where
  toQuery ModifyLoadBalancerAttributes' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "ModifyLoadBalancerAttributes" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2015-12-01" :: Prelude.ByteString),
        "LoadBalancerArn" Data.=: loadBalancerArn,
        "Attributes"
          Data.=: Data.toQueryList "member" attributes
      ]

-- | /See:/ 'newModifyLoadBalancerAttributesResponse' smart constructor.
data ModifyLoadBalancerAttributesResponse = ModifyLoadBalancerAttributesResponse'
  { -- | Information about the load balancer attributes.
    attributes :: Prelude.Maybe [LoadBalancerAttribute],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyLoadBalancerAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributes', 'modifyLoadBalancerAttributesResponse_attributes' - Information about the load balancer attributes.
--
-- 'httpStatus', 'modifyLoadBalancerAttributesResponse_httpStatus' - The response's http status code.
newModifyLoadBalancerAttributesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyLoadBalancerAttributesResponse
newModifyLoadBalancerAttributesResponse pHttpStatus_ =
  ModifyLoadBalancerAttributesResponse'
    { attributes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the load balancer attributes.
modifyLoadBalancerAttributesResponse_attributes :: Lens.Lens' ModifyLoadBalancerAttributesResponse (Prelude.Maybe [LoadBalancerAttribute])
modifyLoadBalancerAttributesResponse_attributes = Lens.lens (\ModifyLoadBalancerAttributesResponse' {attributes} -> attributes) (\s@ModifyLoadBalancerAttributesResponse' {} a -> s {attributes = a} :: ModifyLoadBalancerAttributesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
modifyLoadBalancerAttributesResponse_httpStatus :: Lens.Lens' ModifyLoadBalancerAttributesResponse Prelude.Int
modifyLoadBalancerAttributesResponse_httpStatus = Lens.lens (\ModifyLoadBalancerAttributesResponse' {httpStatus} -> httpStatus) (\s@ModifyLoadBalancerAttributesResponse' {} a -> s {httpStatus = a} :: ModifyLoadBalancerAttributesResponse)

instance
  Prelude.NFData
    ModifyLoadBalancerAttributesResponse
  where
  rnf ModifyLoadBalancerAttributesResponse' {..} =
    Prelude.rnf attributes `Prelude.seq`
      Prelude.rnf httpStatus
