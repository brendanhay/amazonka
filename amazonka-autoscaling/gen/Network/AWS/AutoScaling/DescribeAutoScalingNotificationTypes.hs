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
-- Module      : Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the notification types that are supported by Amazon EC2 Auto
-- Scaling.
module Network.AWS.AutoScaling.DescribeAutoScalingNotificationTypes
  ( -- * Creating a Request
    DescribeAutoScalingNotificationTypes (..),
    newDescribeAutoScalingNotificationTypes,

    -- * Destructuring the Response
    DescribeAutoScalingNotificationTypesResponse (..),
    newDescribeAutoScalingNotificationTypesResponse,

    -- * Response Lenses
    describeAutoScalingNotificationTypesResponse_autoScalingNotificationTypes,
    describeAutoScalingNotificationTypesResponse_httpStatus,
  )
where

import Network.AWS.AutoScaling.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeAutoScalingNotificationTypes' smart constructor.
data DescribeAutoScalingNotificationTypes = DescribeAutoScalingNotificationTypes'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAutoScalingNotificationTypes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDescribeAutoScalingNotificationTypes ::
  DescribeAutoScalingNotificationTypes
newDescribeAutoScalingNotificationTypes =
  DescribeAutoScalingNotificationTypes'

instance
  Core.AWSRequest
    DescribeAutoScalingNotificationTypes
  where
  type
    AWSResponse DescribeAutoScalingNotificationTypes =
      DescribeAutoScalingNotificationTypesResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DescribeAutoScalingNotificationTypesResult"
      ( \s h x ->
          DescribeAutoScalingNotificationTypesResponse'
            Core.<$> ( x Core..@? "AutoScalingNotificationTypes"
                         Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "member")
                     )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeAutoScalingNotificationTypes

instance
  Core.NFData
    DescribeAutoScalingNotificationTypes

instance
  Core.ToHeaders
    DescribeAutoScalingNotificationTypes
  where
  toHeaders = Core.const Core.mempty

instance
  Core.ToPath
    DescribeAutoScalingNotificationTypes
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeAutoScalingNotificationTypes
  where
  toQuery =
    Core.const
      ( Core.mconcat
          [ "Action"
              Core.=: ( "DescribeAutoScalingNotificationTypes" ::
                          Core.ByteString
                      ),
            "Version" Core.=: ("2011-01-01" :: Core.ByteString)
          ]
      )

-- | /See:/ 'newDescribeAutoScalingNotificationTypesResponse' smart constructor.
data DescribeAutoScalingNotificationTypesResponse = DescribeAutoScalingNotificationTypesResponse'
  { -- | The notification types.
    autoScalingNotificationTypes :: Core.Maybe [Core.Text],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeAutoScalingNotificationTypesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'autoScalingNotificationTypes', 'describeAutoScalingNotificationTypesResponse_autoScalingNotificationTypes' - The notification types.
--
-- 'httpStatus', 'describeAutoScalingNotificationTypesResponse_httpStatus' - The response's http status code.
newDescribeAutoScalingNotificationTypesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeAutoScalingNotificationTypesResponse
newDescribeAutoScalingNotificationTypesResponse
  pHttpStatus_ =
    DescribeAutoScalingNotificationTypesResponse'
      { autoScalingNotificationTypes =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The notification types.
describeAutoScalingNotificationTypesResponse_autoScalingNotificationTypes :: Lens.Lens' DescribeAutoScalingNotificationTypesResponse (Core.Maybe [Core.Text])
describeAutoScalingNotificationTypesResponse_autoScalingNotificationTypes = Lens.lens (\DescribeAutoScalingNotificationTypesResponse' {autoScalingNotificationTypes} -> autoScalingNotificationTypes) (\s@DescribeAutoScalingNotificationTypesResponse' {} a -> s {autoScalingNotificationTypes = a} :: DescribeAutoScalingNotificationTypesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeAutoScalingNotificationTypesResponse_httpStatus :: Lens.Lens' DescribeAutoScalingNotificationTypesResponse Core.Int
describeAutoScalingNotificationTypesResponse_httpStatus = Lens.lens (\DescribeAutoScalingNotificationTypesResponse' {httpStatus} -> httpStatus) (\s@DescribeAutoScalingNotificationTypesResponse' {} a -> s {httpStatus = a} :: DescribeAutoScalingNotificationTypesResponse)

instance
  Core.NFData
    DescribeAutoScalingNotificationTypesResponse
