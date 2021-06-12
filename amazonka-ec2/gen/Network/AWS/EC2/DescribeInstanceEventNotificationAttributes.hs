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
-- Module      : Network.AWS.EC2.DescribeInstanceEventNotificationAttributes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the tag keys that are registered to appear in scheduled event
-- notifications for resources in the current Region.
module Network.AWS.EC2.DescribeInstanceEventNotificationAttributes
  ( -- * Creating a Request
    DescribeInstanceEventNotificationAttributes (..),
    newDescribeInstanceEventNotificationAttributes,

    -- * Request Lenses
    describeInstanceEventNotificationAttributes_dryRun,

    -- * Destructuring the Response
    DescribeInstanceEventNotificationAttributesResponse (..),
    newDescribeInstanceEventNotificationAttributesResponse,

    -- * Response Lenses
    describeInstanceEventNotificationAttributesResponse_instanceTagAttribute,
    describeInstanceEventNotificationAttributesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeInstanceEventNotificationAttributes' smart constructor.
data DescribeInstanceEventNotificationAttributes = DescribeInstanceEventNotificationAttributes'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInstanceEventNotificationAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'describeInstanceEventNotificationAttributes_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
newDescribeInstanceEventNotificationAttributes ::
  DescribeInstanceEventNotificationAttributes
newDescribeInstanceEventNotificationAttributes =
  DescribeInstanceEventNotificationAttributes'
    { dryRun =
        Core.Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
describeInstanceEventNotificationAttributes_dryRun :: Lens.Lens' DescribeInstanceEventNotificationAttributes (Core.Maybe Core.Bool)
describeInstanceEventNotificationAttributes_dryRun = Lens.lens (\DescribeInstanceEventNotificationAttributes' {dryRun} -> dryRun) (\s@DescribeInstanceEventNotificationAttributes' {} a -> s {dryRun = a} :: DescribeInstanceEventNotificationAttributes)

instance
  Core.AWSRequest
    DescribeInstanceEventNotificationAttributes
  where
  type
    AWSResponse
      DescribeInstanceEventNotificationAttributes =
      DescribeInstanceEventNotificationAttributesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DescribeInstanceEventNotificationAttributesResponse'
            Core.<$> (x Core..@? "instanceTagAttribute")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeInstanceEventNotificationAttributes

instance
  Core.NFData
    DescribeInstanceEventNotificationAttributes

instance
  Core.ToHeaders
    DescribeInstanceEventNotificationAttributes
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeInstanceEventNotificationAttributes
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeInstanceEventNotificationAttributes
  where
  toQuery
    DescribeInstanceEventNotificationAttributes' {..} =
      Core.mconcat
        [ "Action"
            Core.=: ( "DescribeInstanceEventNotificationAttributes" ::
                        Core.ByteString
                    ),
          "Version" Core.=: ("2016-11-15" :: Core.ByteString),
          "DryRun" Core.=: dryRun
        ]

-- | /See:/ 'newDescribeInstanceEventNotificationAttributesResponse' smart constructor.
data DescribeInstanceEventNotificationAttributesResponse = DescribeInstanceEventNotificationAttributesResponse'
  { -- | Information about the registered tag keys.
    instanceTagAttribute :: Core.Maybe InstanceTagNotificationAttribute,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeInstanceEventNotificationAttributesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceTagAttribute', 'describeInstanceEventNotificationAttributesResponse_instanceTagAttribute' - Information about the registered tag keys.
--
-- 'httpStatus', 'describeInstanceEventNotificationAttributesResponse_httpStatus' - The response's http status code.
newDescribeInstanceEventNotificationAttributesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeInstanceEventNotificationAttributesResponse
newDescribeInstanceEventNotificationAttributesResponse
  pHttpStatus_ =
    DescribeInstanceEventNotificationAttributesResponse'
      { instanceTagAttribute =
          Core.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the registered tag keys.
describeInstanceEventNotificationAttributesResponse_instanceTagAttribute :: Lens.Lens' DescribeInstanceEventNotificationAttributesResponse (Core.Maybe InstanceTagNotificationAttribute)
describeInstanceEventNotificationAttributesResponse_instanceTagAttribute = Lens.lens (\DescribeInstanceEventNotificationAttributesResponse' {instanceTagAttribute} -> instanceTagAttribute) (\s@DescribeInstanceEventNotificationAttributesResponse' {} a -> s {instanceTagAttribute = a} :: DescribeInstanceEventNotificationAttributesResponse)

-- | The response's http status code.
describeInstanceEventNotificationAttributesResponse_httpStatus :: Lens.Lens' DescribeInstanceEventNotificationAttributesResponse Core.Int
describeInstanceEventNotificationAttributesResponse_httpStatus = Lens.lens (\DescribeInstanceEventNotificationAttributesResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceEventNotificationAttributesResponse' {} a -> s {httpStatus = a} :: DescribeInstanceEventNotificationAttributesResponse)

instance
  Core.NFData
    DescribeInstanceEventNotificationAttributesResponse
