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
-- Module      : Amazonka.Lightsail.UpdateLoadBalancerAttribute
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attribute for a load balancer. You can only update
-- one attribute at a time.
--
-- The @update load balancer attribute@ operation supports tag-based access
-- control via resource tags applied to the resource identified by
-- @load balancer name@. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-controlling-access-using-tags Amazon Lightsail Developer Guide>.
module Amazonka.Lightsail.UpdateLoadBalancerAttribute
  ( -- * Creating a Request
    UpdateLoadBalancerAttribute (..),
    newUpdateLoadBalancerAttribute,

    -- * Request Lenses
    updateLoadBalancerAttribute_loadBalancerName,
    updateLoadBalancerAttribute_attributeName,
    updateLoadBalancerAttribute_attributeValue,

    -- * Destructuring the Response
    UpdateLoadBalancerAttributeResponse (..),
    newUpdateLoadBalancerAttributeResponse,

    -- * Response Lenses
    updateLoadBalancerAttributeResponse_operations,
    updateLoadBalancerAttributeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateLoadBalancerAttribute' smart constructor.
data UpdateLoadBalancerAttribute = UpdateLoadBalancerAttribute'
  { -- | The name of the load balancer that you want to modify (e.g.,
    -- @my-load-balancer@.
    loadBalancerName :: Prelude.Text,
    -- | The name of the attribute you want to update.
    attributeName :: LoadBalancerAttributeName,
    -- | The value that you want to specify for the attribute name.
    --
    -- The following values are supported depending on what you specify for the
    -- @attributeName@ request parameter:
    --
    -- -   If you specify @HealthCheckPath@ for the @attributeName@ request
    --     parameter, then the @attributeValue@ request parameter must be the
    --     path to ping on the target (for example,
    --     @\/weather\/us\/wa\/seattle@).
    --
    -- -   If you specify @SessionStickinessEnabled@ for the @attributeName@
    --     request parameter, then the @attributeValue@ request parameter must
    --     be @true@ to activate session stickiness or @false@ to deactivate
    --     session stickiness.
    --
    -- -   If you specify @SessionStickiness_LB_CookieDurationSeconds@ for the
    --     @attributeName@ request parameter, then the @attributeValue@ request
    --     parameter must be an interger that represents the cookie duration in
    --     seconds.
    --
    -- -   If you specify @HttpsRedirectionEnabled@ for the @attributeName@
    --     request parameter, then the @attributeValue@ request parameter must
    --     be @true@ to activate HTTP to HTTPS redirection or @false@ to
    --     deactivate HTTP to HTTPS redirection.
    --
    -- -   If you specify @TlsPolicyName@ for the @attributeName@ request
    --     parameter, then the @attributeValue@ request parameter must be the
    --     name of the TLS policy.
    --
    --     Use the
    --     <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetLoadBalancerTlsPolicies.html GetLoadBalancerTlsPolicies>
    --     action to get a list of TLS policy names that you can specify.
    attributeValue :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLoadBalancerAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loadBalancerName', 'updateLoadBalancerAttribute_loadBalancerName' - The name of the load balancer that you want to modify (e.g.,
-- @my-load-balancer@.
--
-- 'attributeName', 'updateLoadBalancerAttribute_attributeName' - The name of the attribute you want to update.
--
-- 'attributeValue', 'updateLoadBalancerAttribute_attributeValue' - The value that you want to specify for the attribute name.
--
-- The following values are supported depending on what you specify for the
-- @attributeName@ request parameter:
--
-- -   If you specify @HealthCheckPath@ for the @attributeName@ request
--     parameter, then the @attributeValue@ request parameter must be the
--     path to ping on the target (for example,
--     @\/weather\/us\/wa\/seattle@).
--
-- -   If you specify @SessionStickinessEnabled@ for the @attributeName@
--     request parameter, then the @attributeValue@ request parameter must
--     be @true@ to activate session stickiness or @false@ to deactivate
--     session stickiness.
--
-- -   If you specify @SessionStickiness_LB_CookieDurationSeconds@ for the
--     @attributeName@ request parameter, then the @attributeValue@ request
--     parameter must be an interger that represents the cookie duration in
--     seconds.
--
-- -   If you specify @HttpsRedirectionEnabled@ for the @attributeName@
--     request parameter, then the @attributeValue@ request parameter must
--     be @true@ to activate HTTP to HTTPS redirection or @false@ to
--     deactivate HTTP to HTTPS redirection.
--
-- -   If you specify @TlsPolicyName@ for the @attributeName@ request
--     parameter, then the @attributeValue@ request parameter must be the
--     name of the TLS policy.
--
--     Use the
--     <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetLoadBalancerTlsPolicies.html GetLoadBalancerTlsPolicies>
--     action to get a list of TLS policy names that you can specify.
newUpdateLoadBalancerAttribute ::
  -- | 'loadBalancerName'
  Prelude.Text ->
  -- | 'attributeName'
  LoadBalancerAttributeName ->
  -- | 'attributeValue'
  Prelude.Text ->
  UpdateLoadBalancerAttribute
newUpdateLoadBalancerAttribute
  pLoadBalancerName_
  pAttributeName_
  pAttributeValue_ =
    UpdateLoadBalancerAttribute'
      { loadBalancerName =
          pLoadBalancerName_,
        attributeName = pAttributeName_,
        attributeValue = pAttributeValue_
      }

-- | The name of the load balancer that you want to modify (e.g.,
-- @my-load-balancer@.
updateLoadBalancerAttribute_loadBalancerName :: Lens.Lens' UpdateLoadBalancerAttribute Prelude.Text
updateLoadBalancerAttribute_loadBalancerName = Lens.lens (\UpdateLoadBalancerAttribute' {loadBalancerName} -> loadBalancerName) (\s@UpdateLoadBalancerAttribute' {} a -> s {loadBalancerName = a} :: UpdateLoadBalancerAttribute)

-- | The name of the attribute you want to update.
updateLoadBalancerAttribute_attributeName :: Lens.Lens' UpdateLoadBalancerAttribute LoadBalancerAttributeName
updateLoadBalancerAttribute_attributeName = Lens.lens (\UpdateLoadBalancerAttribute' {attributeName} -> attributeName) (\s@UpdateLoadBalancerAttribute' {} a -> s {attributeName = a} :: UpdateLoadBalancerAttribute)

-- | The value that you want to specify for the attribute name.
--
-- The following values are supported depending on what you specify for the
-- @attributeName@ request parameter:
--
-- -   If you specify @HealthCheckPath@ for the @attributeName@ request
--     parameter, then the @attributeValue@ request parameter must be the
--     path to ping on the target (for example,
--     @\/weather\/us\/wa\/seattle@).
--
-- -   If you specify @SessionStickinessEnabled@ for the @attributeName@
--     request parameter, then the @attributeValue@ request parameter must
--     be @true@ to activate session stickiness or @false@ to deactivate
--     session stickiness.
--
-- -   If you specify @SessionStickiness_LB_CookieDurationSeconds@ for the
--     @attributeName@ request parameter, then the @attributeValue@ request
--     parameter must be an interger that represents the cookie duration in
--     seconds.
--
-- -   If you specify @HttpsRedirectionEnabled@ for the @attributeName@
--     request parameter, then the @attributeValue@ request parameter must
--     be @true@ to activate HTTP to HTTPS redirection or @false@ to
--     deactivate HTTP to HTTPS redirection.
--
-- -   If you specify @TlsPolicyName@ for the @attributeName@ request
--     parameter, then the @attributeValue@ request parameter must be the
--     name of the TLS policy.
--
--     Use the
--     <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetLoadBalancerTlsPolicies.html GetLoadBalancerTlsPolicies>
--     action to get a list of TLS policy names that you can specify.
updateLoadBalancerAttribute_attributeValue :: Lens.Lens' UpdateLoadBalancerAttribute Prelude.Text
updateLoadBalancerAttribute_attributeValue = Lens.lens (\UpdateLoadBalancerAttribute' {attributeValue} -> attributeValue) (\s@UpdateLoadBalancerAttribute' {} a -> s {attributeValue = a} :: UpdateLoadBalancerAttribute)

instance Core.AWSRequest UpdateLoadBalancerAttribute where
  type
    AWSResponse UpdateLoadBalancerAttribute =
      UpdateLoadBalancerAttributeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateLoadBalancerAttributeResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateLoadBalancerAttribute where
  hashWithSalt _salt UpdateLoadBalancerAttribute' {..} =
    _salt
      `Prelude.hashWithSalt` loadBalancerName
      `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` attributeValue

instance Prelude.NFData UpdateLoadBalancerAttribute where
  rnf UpdateLoadBalancerAttribute' {..} =
    Prelude.rnf loadBalancerName `Prelude.seq`
      Prelude.rnf attributeName `Prelude.seq`
        Prelude.rnf attributeValue

instance Data.ToHeaders UpdateLoadBalancerAttribute where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.UpdateLoadBalancerAttribute" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateLoadBalancerAttribute where
  toJSON UpdateLoadBalancerAttribute' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("loadBalancerName" Data..= loadBalancerName),
            Prelude.Just ("attributeName" Data..= attributeName),
            Prelude.Just
              ("attributeValue" Data..= attributeValue)
          ]
      )

instance Data.ToPath UpdateLoadBalancerAttribute where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateLoadBalancerAttribute where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateLoadBalancerAttributeResponse' smart constructor.
data UpdateLoadBalancerAttributeResponse = UpdateLoadBalancerAttributeResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateLoadBalancerAttributeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'updateLoadBalancerAttributeResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'updateLoadBalancerAttributeResponse_httpStatus' - The response's http status code.
newUpdateLoadBalancerAttributeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateLoadBalancerAttributeResponse
newUpdateLoadBalancerAttributeResponse pHttpStatus_ =
  UpdateLoadBalancerAttributeResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
updateLoadBalancerAttributeResponse_operations :: Lens.Lens' UpdateLoadBalancerAttributeResponse (Prelude.Maybe [Operation])
updateLoadBalancerAttributeResponse_operations = Lens.lens (\UpdateLoadBalancerAttributeResponse' {operations} -> operations) (\s@UpdateLoadBalancerAttributeResponse' {} a -> s {operations = a} :: UpdateLoadBalancerAttributeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateLoadBalancerAttributeResponse_httpStatus :: Lens.Lens' UpdateLoadBalancerAttributeResponse Prelude.Int
updateLoadBalancerAttributeResponse_httpStatus = Lens.lens (\UpdateLoadBalancerAttributeResponse' {httpStatus} -> httpStatus) (\s@UpdateLoadBalancerAttributeResponse' {} a -> s {httpStatus = a} :: UpdateLoadBalancerAttributeResponse)

instance
  Prelude.NFData
    UpdateLoadBalancerAttributeResponse
  where
  rnf UpdateLoadBalancerAttributeResponse' {..} =
    Prelude.rnf operations `Prelude.seq`
      Prelude.rnf httpStatus
