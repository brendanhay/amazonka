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
-- Module      : Network.AWS.Lightsail.UpdateLoadBalancerAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attribute for a load balancer. You can only update
-- one attribute at a time.
--
-- The @update load balancer attribute@ operation supports tag-based access
-- control via resource tags applied to the resource identified by
-- @load balancer name@. For more information, see the
-- <https://lightsail.aws.amazon.com/ls/docs/en/articles/amazon-lightsail-controlling-access-using-tags Lightsail Dev Guide>.
module Network.AWS.Lightsail.UpdateLoadBalancerAttribute
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateLoadBalancerAttribute' smart constructor.
data UpdateLoadBalancerAttribute = UpdateLoadBalancerAttribute'
  { -- | The name of the load balancer that you want to modify (e.g.,
    -- @my-load-balancer@.
    loadBalancerName :: Core.Text,
    -- | The name of the attribute you want to update. Valid values are below.
    attributeName :: LoadBalancerAttributeName,
    -- | The value that you want to specify for the attribute name.
    attributeValue :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'attributeName', 'updateLoadBalancerAttribute_attributeName' - The name of the attribute you want to update. Valid values are below.
--
-- 'attributeValue', 'updateLoadBalancerAttribute_attributeValue' - The value that you want to specify for the attribute name.
newUpdateLoadBalancerAttribute ::
  -- | 'loadBalancerName'
  Core.Text ->
  -- | 'attributeName'
  LoadBalancerAttributeName ->
  -- | 'attributeValue'
  Core.Text ->
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
updateLoadBalancerAttribute_loadBalancerName :: Lens.Lens' UpdateLoadBalancerAttribute Core.Text
updateLoadBalancerAttribute_loadBalancerName = Lens.lens (\UpdateLoadBalancerAttribute' {loadBalancerName} -> loadBalancerName) (\s@UpdateLoadBalancerAttribute' {} a -> s {loadBalancerName = a} :: UpdateLoadBalancerAttribute)

-- | The name of the attribute you want to update. Valid values are below.
updateLoadBalancerAttribute_attributeName :: Lens.Lens' UpdateLoadBalancerAttribute LoadBalancerAttributeName
updateLoadBalancerAttribute_attributeName = Lens.lens (\UpdateLoadBalancerAttribute' {attributeName} -> attributeName) (\s@UpdateLoadBalancerAttribute' {} a -> s {attributeName = a} :: UpdateLoadBalancerAttribute)

-- | The value that you want to specify for the attribute name.
updateLoadBalancerAttribute_attributeValue :: Lens.Lens' UpdateLoadBalancerAttribute Core.Text
updateLoadBalancerAttribute_attributeValue = Lens.lens (\UpdateLoadBalancerAttribute' {attributeValue} -> attributeValue) (\s@UpdateLoadBalancerAttribute' {} a -> s {attributeValue = a} :: UpdateLoadBalancerAttribute)

instance Core.AWSRequest UpdateLoadBalancerAttribute where
  type
    AWSResponse UpdateLoadBalancerAttribute =
      UpdateLoadBalancerAttributeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateLoadBalancerAttributeResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateLoadBalancerAttribute

instance Core.NFData UpdateLoadBalancerAttribute

instance Core.ToHeaders UpdateLoadBalancerAttribute where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.UpdateLoadBalancerAttribute" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateLoadBalancerAttribute where
  toJSON UpdateLoadBalancerAttribute' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("loadBalancerName" Core..= loadBalancerName),
            Core.Just ("attributeName" Core..= attributeName),
            Core.Just ("attributeValue" Core..= attributeValue)
          ]
      )

instance Core.ToPath UpdateLoadBalancerAttribute where
  toPath = Core.const "/"

instance Core.ToQuery UpdateLoadBalancerAttribute where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateLoadBalancerAttributeResponse' smart constructor.
data UpdateLoadBalancerAttributeResponse = UpdateLoadBalancerAttributeResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateLoadBalancerAttributeResponse
newUpdateLoadBalancerAttributeResponse pHttpStatus_ =
  UpdateLoadBalancerAttributeResponse'
    { operations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
updateLoadBalancerAttributeResponse_operations :: Lens.Lens' UpdateLoadBalancerAttributeResponse (Core.Maybe [Operation])
updateLoadBalancerAttributeResponse_operations = Lens.lens (\UpdateLoadBalancerAttributeResponse' {operations} -> operations) (\s@UpdateLoadBalancerAttributeResponse' {} a -> s {operations = a} :: UpdateLoadBalancerAttributeResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
updateLoadBalancerAttributeResponse_httpStatus :: Lens.Lens' UpdateLoadBalancerAttributeResponse Core.Int
updateLoadBalancerAttributeResponse_httpStatus = Lens.lens (\UpdateLoadBalancerAttributeResponse' {httpStatus} -> httpStatus) (\s@UpdateLoadBalancerAttributeResponse' {} a -> s {httpStatus = a} :: UpdateLoadBalancerAttributeResponse)

instance
  Core.NFData
    UpdateLoadBalancerAttributeResponse
