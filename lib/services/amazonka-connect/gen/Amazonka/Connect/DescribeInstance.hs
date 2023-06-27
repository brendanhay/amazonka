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
-- Module      : Amazonka.Connect.DescribeInstance
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Returns the current state of the specified instance identifier. It
-- tracks the instance while it is being created and returns an error
-- status, if applicable.
--
-- If an instance is not created successfully, the instance status reason
-- field returns details relevant to the reason. The instance in a failed
-- state is returned only for 24 hours after the CreateInstance API was
-- invoked.
module Amazonka.Connect.DescribeInstance
  ( -- * Creating a Request
    DescribeInstance (..),
    newDescribeInstance,

    -- * Request Lenses
    describeInstance_instanceId,

    -- * Destructuring the Response
    DescribeInstanceResponse (..),
    newDescribeInstanceResponse,

    -- * Response Lenses
    describeInstanceResponse_instance,
    describeInstanceResponse_httpStatus,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeInstance' smart constructor.
data DescribeInstance = DescribeInstance'
  { -- | The identifier of the Amazon Connect instance. You can
    -- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
    -- in the Amazon Resource Name (ARN) of the instance.
    instanceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstance' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'describeInstance_instanceId' - The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
newDescribeInstance ::
  -- | 'instanceId'
  Prelude.Text ->
  DescribeInstance
newDescribeInstance pInstanceId_ =
  DescribeInstance' {instanceId = pInstanceId_}

-- | The identifier of the Amazon Connect instance. You can
-- <https://docs.aws.amazon.com/connect/latest/adminguide/find-instance-arn.html find the instance ID>
-- in the Amazon Resource Name (ARN) of the instance.
describeInstance_instanceId :: Lens.Lens' DescribeInstance Prelude.Text
describeInstance_instanceId = Lens.lens (\DescribeInstance' {instanceId} -> instanceId) (\s@DescribeInstance' {} a -> s {instanceId = a} :: DescribeInstance)

instance Core.AWSRequest DescribeInstance where
  type
    AWSResponse DescribeInstance =
      DescribeInstanceResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstanceResponse'
            Prelude.<$> (x Data..?> "Instance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstance where
  hashWithSalt _salt DescribeInstance' {..} =
    _salt `Prelude.hashWithSalt` instanceId

instance Prelude.NFData DescribeInstance where
  rnf DescribeInstance' {..} = Prelude.rnf instanceId

instance Data.ToHeaders DescribeInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribeInstance where
  toPath DescribeInstance' {..} =
    Prelude.mconcat
      ["/instance/", Data.toBS instanceId]

instance Data.ToQuery DescribeInstance where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeInstanceResponse' smart constructor.
data DescribeInstanceResponse = DescribeInstanceResponse'
  { -- | The name of the instance.
    instance' :: Prelude.Maybe Instance,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInstanceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instance'', 'describeInstanceResponse_instance' - The name of the instance.
--
-- 'httpStatus', 'describeInstanceResponse_httpStatus' - The response's http status code.
newDescribeInstanceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInstanceResponse
newDescribeInstanceResponse pHttpStatus_ =
  DescribeInstanceResponse'
    { instance' =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the instance.
describeInstanceResponse_instance :: Lens.Lens' DescribeInstanceResponse (Prelude.Maybe Instance)
describeInstanceResponse_instance = Lens.lens (\DescribeInstanceResponse' {instance'} -> instance') (\s@DescribeInstanceResponse' {} a -> s {instance' = a} :: DescribeInstanceResponse)

-- | The response's http status code.
describeInstanceResponse_httpStatus :: Lens.Lens' DescribeInstanceResponse Prelude.Int
describeInstanceResponse_httpStatus = Lens.lens (\DescribeInstanceResponse' {httpStatus} -> httpStatus) (\s@DescribeInstanceResponse' {} a -> s {httpStatus = a} :: DescribeInstanceResponse)

instance Prelude.NFData DescribeInstanceResponse where
  rnf DescribeInstanceResponse' {..} =
    Prelude.rnf instance'
      `Prelude.seq` Prelude.rnf httpStatus
