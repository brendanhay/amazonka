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
-- Module      : Amazonka.OpsWorks.DescribeServiceErrors
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
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
module Amazonka.OpsWorks.DescribeServiceErrors
  ( -- * Creating a Request
    DescribeServiceErrors (..),
    newDescribeServiceErrors,

    -- * Request Lenses
    describeServiceErrors_instanceId,
    describeServiceErrors_serviceErrorIds,
    describeServiceErrors_stackId,

    -- * Destructuring the Response
    DescribeServiceErrorsResponse (..),
    newDescribeServiceErrorsResponse,

    -- * Response Lenses
    describeServiceErrorsResponse_serviceErrors,
    describeServiceErrorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpsWorks.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeServiceErrors' smart constructor.
data DescribeServiceErrors = DescribeServiceErrors'
  { -- | The instance ID. If you use this parameter, @DescribeServiceErrors@
    -- returns descriptions of the errors associated with the specified
    -- instance.
    instanceId :: Prelude.Maybe Prelude.Text,
    -- | An array of service error IDs. If you use this parameter,
    -- @DescribeServiceErrors@ returns descriptions of the specified errors.
    -- Otherwise, it returns a description of every error.
    serviceErrorIds :: Prelude.Maybe [Prelude.Text],
    -- | The stack ID. If you use this parameter, @DescribeServiceErrors@ returns
    -- descriptions of the errors associated with the specified stack.
    stackId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'serviceErrorIds', 'describeServiceErrors_serviceErrorIds' - An array of service error IDs. If you use this parameter,
-- @DescribeServiceErrors@ returns descriptions of the specified errors.
-- Otherwise, it returns a description of every error.
--
-- 'stackId', 'describeServiceErrors_stackId' - The stack ID. If you use this parameter, @DescribeServiceErrors@ returns
-- descriptions of the errors associated with the specified stack.
newDescribeServiceErrors ::
  DescribeServiceErrors
newDescribeServiceErrors =
  DescribeServiceErrors'
    { instanceId =
        Prelude.Nothing,
      serviceErrorIds = Prelude.Nothing,
      stackId = Prelude.Nothing
    }

-- | The instance ID. If you use this parameter, @DescribeServiceErrors@
-- returns descriptions of the errors associated with the specified
-- instance.
describeServiceErrors_instanceId :: Lens.Lens' DescribeServiceErrors (Prelude.Maybe Prelude.Text)
describeServiceErrors_instanceId = Lens.lens (\DescribeServiceErrors' {instanceId} -> instanceId) (\s@DescribeServiceErrors' {} a -> s {instanceId = a} :: DescribeServiceErrors)

-- | An array of service error IDs. If you use this parameter,
-- @DescribeServiceErrors@ returns descriptions of the specified errors.
-- Otherwise, it returns a description of every error.
describeServiceErrors_serviceErrorIds :: Lens.Lens' DescribeServiceErrors (Prelude.Maybe [Prelude.Text])
describeServiceErrors_serviceErrorIds = Lens.lens (\DescribeServiceErrors' {serviceErrorIds} -> serviceErrorIds) (\s@DescribeServiceErrors' {} a -> s {serviceErrorIds = a} :: DescribeServiceErrors) Prelude.. Lens.mapping Lens.coerced

-- | The stack ID. If you use this parameter, @DescribeServiceErrors@ returns
-- descriptions of the errors associated with the specified stack.
describeServiceErrors_stackId :: Lens.Lens' DescribeServiceErrors (Prelude.Maybe Prelude.Text)
describeServiceErrors_stackId = Lens.lens (\DescribeServiceErrors' {stackId} -> stackId) (\s@DescribeServiceErrors' {} a -> s {stackId = a} :: DescribeServiceErrors)

instance Core.AWSRequest DescribeServiceErrors where
  type
    AWSResponse DescribeServiceErrors =
      DescribeServiceErrorsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeServiceErrorsResponse'
            Prelude.<$> (x Data..?> "ServiceErrors" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeServiceErrors where
  hashWithSalt _salt DescribeServiceErrors' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` serviceErrorIds
      `Prelude.hashWithSalt` stackId

instance Prelude.NFData DescribeServiceErrors where
  rnf DescribeServiceErrors' {..} =
    Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf serviceErrorIds
      `Prelude.seq` Prelude.rnf stackId

instance Data.ToHeaders DescribeServiceErrors where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "OpsWorks_20130218.DescribeServiceErrors" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeServiceErrors where
  toJSON DescribeServiceErrors' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("InstanceId" Data..=) Prelude.<$> instanceId,
            ("ServiceErrorIds" Data..=)
              Prelude.<$> serviceErrorIds,
            ("StackId" Data..=) Prelude.<$> stackId
          ]
      )

instance Data.ToPath DescribeServiceErrors where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeServiceErrors where
  toQuery = Prelude.const Prelude.mempty

-- | Contains the response to a @DescribeServiceErrors@ request.
--
-- /See:/ 'newDescribeServiceErrorsResponse' smart constructor.
data DescribeServiceErrorsResponse = DescribeServiceErrorsResponse'
  { -- | An array of @ServiceError@ objects that describe the specified service
    -- errors.
    serviceErrors :: Prelude.Maybe [ServiceError],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeServiceErrorsResponse
newDescribeServiceErrorsResponse pHttpStatus_ =
  DescribeServiceErrorsResponse'
    { serviceErrors =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of @ServiceError@ objects that describe the specified service
-- errors.
describeServiceErrorsResponse_serviceErrors :: Lens.Lens' DescribeServiceErrorsResponse (Prelude.Maybe [ServiceError])
describeServiceErrorsResponse_serviceErrors = Lens.lens (\DescribeServiceErrorsResponse' {serviceErrors} -> serviceErrors) (\s@DescribeServiceErrorsResponse' {} a -> s {serviceErrors = a} :: DescribeServiceErrorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeServiceErrorsResponse_httpStatus :: Lens.Lens' DescribeServiceErrorsResponse Prelude.Int
describeServiceErrorsResponse_httpStatus = Lens.lens (\DescribeServiceErrorsResponse' {httpStatus} -> httpStatus) (\s@DescribeServiceErrorsResponse' {} a -> s {httpStatus = a} :: DescribeServiceErrorsResponse)

instance Prelude.NFData DescribeServiceErrorsResponse where
  rnf DescribeServiceErrorsResponse' {..} =
    Prelude.rnf serviceErrors
      `Prelude.seq` Prelude.rnf httpStatus
