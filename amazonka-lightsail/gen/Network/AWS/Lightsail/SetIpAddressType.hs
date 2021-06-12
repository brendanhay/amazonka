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
-- Module      : Network.AWS.Lightsail.SetIpAddressType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the IP address type for an Amazon Lightsail resource.
--
-- Use this action to enable dual-stack for a resource, which enables IPv4
-- and IPv6 for the specified resource. Alternately, you can use this
-- action to disable dual-stack, and enable IPv4 only.
module Network.AWS.Lightsail.SetIpAddressType
  ( -- * Creating a Request
    SetIpAddressType (..),
    newSetIpAddressType,

    -- * Request Lenses
    setIpAddressType_resourceType,
    setIpAddressType_resourceName,
    setIpAddressType_ipAddressType,

    -- * Destructuring the Response
    SetIpAddressTypeResponse (..),
    newSetIpAddressTypeResponse,

    -- * Response Lenses
    setIpAddressTypeResponse_operations,
    setIpAddressTypeResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newSetIpAddressType' smart constructor.
data SetIpAddressType = SetIpAddressType'
  { -- | The resource type.
    --
    -- The possible values are @Distribution@, @Instance@, and @LoadBalancer@.
    --
    -- Distribution-related APIs are available only in the N. Virginia
    -- (@us-east-1@) AWS Region. Set your AWS Region configuration to
    -- @us-east-1@ to create, view, or edit distributions.
    resourceType :: ResourceType,
    -- | The name of the resource for which to set the IP address type.
    resourceName :: Core.Text,
    -- | The IP address type to set for the specified resource.
    --
    -- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
    -- and IPv6.
    ipAddressType :: IpAddressType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetIpAddressType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceType', 'setIpAddressType_resourceType' - The resource type.
--
-- The possible values are @Distribution@, @Instance@, and @LoadBalancer@.
--
-- Distribution-related APIs are available only in the N. Virginia
-- (@us-east-1@) AWS Region. Set your AWS Region configuration to
-- @us-east-1@ to create, view, or edit distributions.
--
-- 'resourceName', 'setIpAddressType_resourceName' - The name of the resource for which to set the IP address type.
--
-- 'ipAddressType', 'setIpAddressType_ipAddressType' - The IP address type to set for the specified resource.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
newSetIpAddressType ::
  -- | 'resourceType'
  ResourceType ->
  -- | 'resourceName'
  Core.Text ->
  -- | 'ipAddressType'
  IpAddressType ->
  SetIpAddressType
newSetIpAddressType
  pResourceType_
  pResourceName_
  pIpAddressType_ =
    SetIpAddressType'
      { resourceType = pResourceType_,
        resourceName = pResourceName_,
        ipAddressType = pIpAddressType_
      }

-- | The resource type.
--
-- The possible values are @Distribution@, @Instance@, and @LoadBalancer@.
--
-- Distribution-related APIs are available only in the N. Virginia
-- (@us-east-1@) AWS Region. Set your AWS Region configuration to
-- @us-east-1@ to create, view, or edit distributions.
setIpAddressType_resourceType :: Lens.Lens' SetIpAddressType ResourceType
setIpAddressType_resourceType = Lens.lens (\SetIpAddressType' {resourceType} -> resourceType) (\s@SetIpAddressType' {} a -> s {resourceType = a} :: SetIpAddressType)

-- | The name of the resource for which to set the IP address type.
setIpAddressType_resourceName :: Lens.Lens' SetIpAddressType Core.Text
setIpAddressType_resourceName = Lens.lens (\SetIpAddressType' {resourceName} -> resourceName) (\s@SetIpAddressType' {} a -> s {resourceName = a} :: SetIpAddressType)

-- | The IP address type to set for the specified resource.
--
-- The possible values are @ipv4@ for IPv4 only, and @dualstack@ for IPv4
-- and IPv6.
setIpAddressType_ipAddressType :: Lens.Lens' SetIpAddressType IpAddressType
setIpAddressType_ipAddressType = Lens.lens (\SetIpAddressType' {ipAddressType} -> ipAddressType) (\s@SetIpAddressType' {} a -> s {ipAddressType = a} :: SetIpAddressType)

instance Core.AWSRequest SetIpAddressType where
  type
    AWSResponse SetIpAddressType =
      SetIpAddressTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          SetIpAddressTypeResponse'
            Core.<$> (x Core..?> "operations" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable SetIpAddressType

instance Core.NFData SetIpAddressType

instance Core.ToHeaders SetIpAddressType where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.SetIpAddressType" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON SetIpAddressType where
  toJSON SetIpAddressType' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("resourceType" Core..= resourceType),
            Core.Just ("resourceName" Core..= resourceName),
            Core.Just ("ipAddressType" Core..= ipAddressType)
          ]
      )

instance Core.ToPath SetIpAddressType where
  toPath = Core.const "/"

instance Core.ToQuery SetIpAddressType where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newSetIpAddressTypeResponse' smart constructor.
data SetIpAddressTypeResponse = SetIpAddressTypeResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Core.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SetIpAddressTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'setIpAddressTypeResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'setIpAddressTypeResponse_httpStatus' - The response's http status code.
newSetIpAddressTypeResponse ::
  -- | 'httpStatus'
  Core.Int ->
  SetIpAddressTypeResponse
newSetIpAddressTypeResponse pHttpStatus_ =
  SetIpAddressTypeResponse'
    { operations =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
setIpAddressTypeResponse_operations :: Lens.Lens' SetIpAddressTypeResponse (Core.Maybe [Operation])
setIpAddressTypeResponse_operations = Lens.lens (\SetIpAddressTypeResponse' {operations} -> operations) (\s@SetIpAddressTypeResponse' {} a -> s {operations = a} :: SetIpAddressTypeResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
setIpAddressTypeResponse_httpStatus :: Lens.Lens' SetIpAddressTypeResponse Core.Int
setIpAddressTypeResponse_httpStatus = Lens.lens (\SetIpAddressTypeResponse' {httpStatus} -> httpStatus) (\s@SetIpAddressTypeResponse' {} a -> s {httpStatus = a} :: SetIpAddressTypeResponse)

instance Core.NFData SetIpAddressTypeResponse
