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
-- Module      : Network.AWS.OpsWorks.DescribeServiceErrors
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes AWS OpsWorks Stacks service errors.
--
-- __Required Permissions__: To use this action, an IAM user must have a
-- Show, Deploy, or Manage permissions level for the stack, or an attached
-- policy that explicitly grants permissions. For more information about
-- user permissions, see
-- <https://docs.aws.amazon.com/opsworks/latest/userguide/opsworks-security-users.html Managing User Permissions>.
--
-- This call accepts only one resource-identifying parameter.
module Network.AWS.OpsWorks.DescribeServiceErrors
  ( -- * Creating a Request
    DescribeServiceErrors (..),
    newDescribeServiceErrors,

    -- * Request Lenses
    describeServiceErrors_instanceId,
    describeServiceErrors_stackId,
    describeServiceErrors_serviceErrorIds,

    -- * Destructuring the Response
    DescribeServiceErrorsResponse (..),
    newDescribeServiceErrorsResponse,

    -- * Response Lenses
    describeServiceErrorsResponse_serviceErrors,
    describeServiceErrorsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.OpsWorks.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeServiceErrors' smart constructor.
data DescribeServiceErrors = DescribeServiceErrors'
  { -- | The instance ID. If you use this parameter, @DescribeServiceErrors@
    -- returns descriptions of the errors associated with the specified
    -- instance.
    instanceId :: Core.Maybe Core.Text,
    -- | The stack ID. If you use this parameter, @DescribeServiceErrors@ returns
    -- descriptions of the errors associated with the specified stack.
    stackId :: Core.Maybe Core.Text,
    -- | An array of service error IDs. If you use this parameter,
    -- @DescribeServiceErrors@ returns descriptions of the specified errors.
    -- Otherwise, it returns a description of every error.
    serviceErrorIds :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeServiceErrors' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeServiceErrors_instanceId' - The instance ID. If you use this parameter, @DescribeServiceErrors@
-- returns descriptions of the errors associated with the specified
-- instance.
--
-- 'stackId', 'describeServiceErrors_stackId' - The stack ID. If you use this parameter, @DescribeServiceErrors@ returns
-- descriptions of the errors associated with the specified stack.
--
-- 'serviceErrorIds', 'describeServiceErrors_serviceErrorIds' - An array of service error IDs. If you use this parameter,
-- @DescribeServiceErrors@ returns descriptions of the specified errors.
-- Otherwise, it returns a description of every error.
newDescribeServiceErrors ::
  DescribeServiceErrors
newDescribeServiceErrors =
  DescribeServiceErrors'
    { instanceId = Core.Nothing,
      stackId = Core.Nothing,
      serviceErrorIds = Core.Nothing
    }

-- | The instance ID. If you use this parameter, @DescribeServiceErrors@
-- returns descriptions of the errors associated with the specified
-- instance.
describeServiceErrors_instanceId :: Lens.Lens' DescribeServiceErrors (Core.Maybe Core.Text)
describeServiceErrors_instanceId = Lens.lens (\DescribeServiceErrors' {instanceId} -> instanceId) (\s@DescribeServiceErrors' {} a -> s {instanceId = a} :: DescribeServiceErrors)

-- | The stack ID. If you use this parameter, @DescribeServiceErrors@ returns
-- descriptions of the errors associated with the specified stack.
describeServiceErrors_stackId :: Lens.Lens' DescribeServiceErrors (Core.Maybe Core.Text)
describeServiceErrors_stackId = Lens.lens (\DescribeServiceErrors' {stackId} -> stackId) (\s@DescribeServiceErrors' {} a -> s {stackId = a} :: DescribeServiceErrors)

-- | An array of service error IDs. If you use this parameter,
-- @DescribeServiceErrors@ returns descriptions of the specified errors.
-- Otherwise, it returns a description of every error.
describeServiceErrors_serviceErrorIds :: Lens.Lens' DescribeServiceErrors (Core.Maybe [Core.Text])
describeServiceErrors_serviceErrorIds = Lens.lens (\DescribeServiceErrors' {serviceErrorIds} -> serviceErrorIds) (\s@DescribeServiceErrors' {} a -> s {serviceErrorIds = a} :: DescribeServiceErrors) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeServiceErrors where
  type
    AWSResponse DescribeServiceErrors =
      DescribeServiceErrorsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServiceErrorsResponse'
            Core.<$> (x Core..?> "ServiceErrors" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeServiceErrors

instance Core.NFData DescribeServiceErrors

instance Core.ToHeaders DescribeServiceErrors where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OpsWorks_20130218.DescribeServiceErrors" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeServiceErrors where
  toJSON DescribeServiceErrors' {..} =
    Core.object
      ( Core.catMaybes
          [ ("InstanceId" Core..=) Core.<$> instanceId,
            ("StackId" Core..=) Core.<$> stackId,
            ("ServiceErrorIds" Core..=)
              Core.<$> serviceErrorIds
          ]
      )

instance Core.ToPath DescribeServiceErrors where
  toPath = Core.const "/"

instance Core.ToQuery DescribeServiceErrors where
  toQuery = Core.const Core.mempty

-- | Contains the response to a @DescribeServiceErrors@ request.
--
-- /See:/ 'newDescribeServiceErrorsResponse' smart constructor.
data DescribeServiceErrorsResponse = DescribeServiceErrorsResponse'
  { -- | An array of @ServiceError@ objects that describe the specified service
    -- errors.
    serviceErrors :: Core.Maybe [ServiceError'],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeServiceErrorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceErrors', 'describeServiceErrorsResponse_serviceErrors' - An array of @ServiceError@ objects that describe the specified service
-- errors.
--
-- 'httpStatus', 'describeServiceErrorsResponse_httpStatus' - The response's http status code.
newDescribeServiceErrorsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeServiceErrorsResponse
newDescribeServiceErrorsResponse pHttpStatus_ =
  DescribeServiceErrorsResponse'
    { serviceErrors =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @ServiceError@ objects that describe the specified service
-- errors.
describeServiceErrorsResponse_serviceErrors :: Lens.Lens' DescribeServiceErrorsResponse (Core.Maybe [ServiceError'])
describeServiceErrorsResponse_serviceErrors = Lens.lens (\DescribeServiceErrorsResponse' {serviceErrors} -> serviceErrors) (\s@DescribeServiceErrorsResponse' {} a -> s {serviceErrors = a} :: DescribeServiceErrorsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeServiceErrorsResponse_httpStatus :: Lens.Lens' DescribeServiceErrorsResponse Core.Int
describeServiceErrorsResponse_httpStatus = Lens.lens (\DescribeServiceErrorsResponse' {httpStatus} -> httpStatus) (\s@DescribeServiceErrorsResponse' {} a -> s {httpStatus = a} :: DescribeServiceErrorsResponse)

instance Core.NFData DescribeServiceErrorsResponse
