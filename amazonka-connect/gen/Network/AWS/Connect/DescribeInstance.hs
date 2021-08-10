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
-- Module      : Network.AWS.Connect.DescribeInstance
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.Connect.DescribeInstance
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

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeInstance' smart constructor.
data DescribeInstance = DescribeInstance'
  { -- | The identifier of the Amazon Connect instance.
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
-- 'instanceId', 'describeInstance_instanceId' - The identifier of the Amazon Connect instance.
newDescribeInstance ::
  -- | 'instanceId'
  Prelude.Text ->
  DescribeInstance
newDescribeInstance pInstanceId_ =
  DescribeInstance' {instanceId = pInstanceId_}

-- | The identifier of the Amazon Connect instance.
describeInstance_instanceId :: Lens.Lens' DescribeInstance Prelude.Text
describeInstance_instanceId = Lens.lens (\DescribeInstance' {instanceId} -> instanceId) (\s@DescribeInstance' {} a -> s {instanceId = a} :: DescribeInstance)

instance Core.AWSRequest DescribeInstance where
  type
    AWSResponse DescribeInstance =
      DescribeInstanceResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInstanceResponse'
            Prelude.<$> (x Core..?> "Instance")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInstance

instance Prelude.NFData DescribeInstance

instance Core.ToHeaders DescribeInstance where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DescribeInstance where
  toPath DescribeInstance' {..} =
    Prelude.mconcat
      ["/instance/", Core.toBS instanceId]

instance Core.ToQuery DescribeInstance where
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

instance Prelude.NFData DescribeInstanceResponse
