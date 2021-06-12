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
-- Module      : Network.AWS.EC2.RegisterInstanceEventNotificationAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Registers a set of tag keys to include in scheduled event notifications
-- for your resources.
--
-- To remove tags, use .
module Network.AWS.EC2.RegisterInstanceEventNotificationAttributes
  ( -- * Creating a Request
    RegisterInstanceEventNotificationAttributes (..),
    newRegisterInstanceEventNotificationAttributes,

    -- * Request Lenses
    registerInstanceEventNotificationAttributes_dryRun,
    registerInstanceEventNotificationAttributes_instanceTagAttribute,

    -- * Destructuring the Response
    RegisterInstanceEventNotificationAttributesResponse (..),
    newRegisterInstanceEventNotificationAttributesResponse,

    -- * Response Lenses
    registerInstanceEventNotificationAttributesResponse_instanceTagAttribute,
    registerInstanceEventNotificationAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRegisterInstanceEventNotificationAttributes' smart constructor.
data RegisterInstanceEventNotificationAttributes = RegisterInstanceEventNotificationAttributes'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | Information about the tag keys to register.
    instanceTagAttribute :: Core.Maybe RegisterInstanceTagAttributeRequest
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterInstanceEventNotificationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'registerInstanceEventNotificationAttributes_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'instanceTagAttribute', 'registerInstanceEventNotificationAttributes_instanceTagAttribute' - Information about the tag keys to register.
newRegisterInstanceEventNotificationAttributes ::
  RegisterInstanceEventNotificationAttributes
newRegisterInstanceEventNotificationAttributes =
  RegisterInstanceEventNotificationAttributes'
    { dryRun =
        Core.Nothing,
      instanceTagAttribute =
        Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
registerInstanceEventNotificationAttributes_dryRun :: Lens.Lens' RegisterInstanceEventNotificationAttributes (Core.Maybe Core.Bool)
registerInstanceEventNotificationAttributes_dryRun = Lens.lens (\RegisterInstanceEventNotificationAttributes' {dryRun} -> dryRun) (\s@RegisterInstanceEventNotificationAttributes' {} a -> s {dryRun = a} :: RegisterInstanceEventNotificationAttributes)

-- | Information about the tag keys to register.
registerInstanceEventNotificationAttributes_instanceTagAttribute :: Lens.Lens' RegisterInstanceEventNotificationAttributes (Core.Maybe RegisterInstanceTagAttributeRequest)
registerInstanceEventNotificationAttributes_instanceTagAttribute = Lens.lens (\RegisterInstanceEventNotificationAttributes' {instanceTagAttribute} -> instanceTagAttribute) (\s@RegisterInstanceEventNotificationAttributes' {} a -> s {instanceTagAttribute = a} :: RegisterInstanceEventNotificationAttributes)

instance
  Core.AWSRequest
    RegisterInstanceEventNotificationAttributes
  where
  type
    AWSResponse
      RegisterInstanceEventNotificationAttributes =
      RegisterInstanceEventNotificationAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          RegisterInstanceEventNotificationAttributesResponse'
            Core.<$> (x Core..@? "instanceTagAttribute")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    RegisterInstanceEventNotificationAttributes

instance
  Core.NFData
    RegisterInstanceEventNotificationAttributes

instance
  Core.ToHeaders
    RegisterInstanceEventNotificationAttributes
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    RegisterInstanceEventNotificationAttributes
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    RegisterInstanceEventNotificationAttributes
  where
  toQuery
    RegisterInstanceEventNotificationAttributes' {..} =
      Core.mconcat
        [ "Action"
            Core.=: ( "RegisterInstanceEventNotificationAttributes" ::
                        Core.ByteString
                    ),
          "Version" Core.=: ("2016-11-15" :: Core.ByteString),
          "DryRun" Core.=: dryRun,
          "InstanceTagAttribute" Core.=: instanceTagAttribute
        ]

-- | /See:/ 'newRegisterInstanceEventNotificationAttributesResponse' smart constructor.
data RegisterInstanceEventNotificationAttributesResponse = RegisterInstanceEventNotificationAttributesResponse'
  { -- | The resulting set of tag keys.
    instanceTagAttribute :: Core.Maybe InstanceTagNotificationAttribute,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RegisterInstanceEventNotificationAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceTagAttribute', 'registerInstanceEventNotificationAttributesResponse_instanceTagAttribute' - The resulting set of tag keys.
--
-- 'httpStatus', 'registerInstanceEventNotificationAttributesResponse_httpStatus' - The response's http status code.
newRegisterInstanceEventNotificationAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  RegisterInstanceEventNotificationAttributesResponse
newRegisterInstanceEventNotificationAttributesResponse
  pHttpStatus_ =
    RegisterInstanceEventNotificationAttributesResponse'
      { instanceTagAttribute =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The resulting set of tag keys.
registerInstanceEventNotificationAttributesResponse_instanceTagAttribute :: Lens.Lens' RegisterInstanceEventNotificationAttributesResponse (Core.Maybe InstanceTagNotificationAttribute)
registerInstanceEventNotificationAttributesResponse_instanceTagAttribute = Lens.lens (\RegisterInstanceEventNotificationAttributesResponse' {instanceTagAttribute} -> instanceTagAttribute) (\s@RegisterInstanceEventNotificationAttributesResponse' {} a -> s {instanceTagAttribute = a} :: RegisterInstanceEventNotificationAttributesResponse)

-- | The response's http status code.
registerInstanceEventNotificationAttributesResponse_httpStatus :: Lens.Lens' RegisterInstanceEventNotificationAttributesResponse Core.Int
registerInstanceEventNotificationAttributesResponse_httpStatus = Lens.lens (\RegisterInstanceEventNotificationAttributesResponse' {httpStatus} -> httpStatus) (\s@RegisterInstanceEventNotificationAttributesResponse' {} a -> s {httpStatus = a} :: RegisterInstanceEventNotificationAttributesResponse)

instance
  Core.NFData
    RegisterInstanceEventNotificationAttributesResponse
