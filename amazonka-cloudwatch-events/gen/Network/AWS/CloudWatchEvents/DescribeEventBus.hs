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
-- Module      : Network.AWS.CloudWatchEvents.DescribeEventBus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays details about an event bus in your account. This can include
-- the external AWS accounts that are permitted to write events to your
-- default event bus, and the associated policy. For custom event buses and
-- partner event buses, it displays the name, ARN, policy, state, and
-- creation time.
--
-- To enable your account to receive events from other accounts on its
-- default event bus, use PutPermission.
--
-- For more information about partner event buses, see CreateEventBus.
module Network.AWS.CloudWatchEvents.DescribeEventBus
  ( -- * Creating a Request
    DescribeEventBus (..),
    newDescribeEventBus,

    -- * Request Lenses
    describeEventBus_name,

    -- * Destructuring the Response
    DescribeEventBusResponse (..),
    newDescribeEventBusResponse,

    -- * Response Lenses
    describeEventBusResponse_arn,
    describeEventBusResponse_name,
    describeEventBusResponse_policy,
    describeEventBusResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEventBus' smart constructor.
data DescribeEventBus = DescribeEventBus'
  { -- | The name or ARN of the event bus to show details for. If you omit this,
    -- the default event bus is displayed.
    name :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEventBus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describeEventBus_name' - The name or ARN of the event bus to show details for. If you omit this,
-- the default event bus is displayed.
newDescribeEventBus ::
  DescribeEventBus
newDescribeEventBus =
  DescribeEventBus' {name = Core.Nothing}

-- | The name or ARN of the event bus to show details for. If you omit this,
-- the default event bus is displayed.
describeEventBus_name :: Lens.Lens' DescribeEventBus (Core.Maybe Core.Text)
describeEventBus_name = Lens.lens (\DescribeEventBus' {name} -> name) (\s@DescribeEventBus' {} a -> s {name = a} :: DescribeEventBus)

instance Core.AWSRequest DescribeEventBus where
  type
    AWSResponse DescribeEventBus =
      DescribeEventBusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventBusResponse'
            Core.<$> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Name")
            Core.<*> (x Core..?> "Policy")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeEventBus

instance Core.NFData DescribeEventBus

instance Core.ToHeaders DescribeEventBus where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSEvents.DescribeEventBus" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeEventBus where
  toJSON DescribeEventBus' {..} =
    Core.object
      (Core.catMaybes [("Name" Core..=) Core.<$> name])

instance Core.ToPath DescribeEventBus where
  toPath = Core.const "/"

instance Core.ToQuery DescribeEventBus where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeEventBusResponse' smart constructor.
data DescribeEventBusResponse = DescribeEventBusResponse'
  { -- | The Amazon Resource Name (ARN) of the account permitted to write events
    -- to the current account.
    arn :: Core.Maybe Core.Text,
    -- | The name of the event bus. Currently, this is always @default@.
    name :: Core.Maybe Core.Text,
    -- | The policy that enables the external account to send events to your
    -- account.
    policy :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeEventBusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describeEventBusResponse_arn' - The Amazon Resource Name (ARN) of the account permitted to write events
-- to the current account.
--
-- 'name', 'describeEventBusResponse_name' - The name of the event bus. Currently, this is always @default@.
--
-- 'policy', 'describeEventBusResponse_policy' - The policy that enables the external account to send events to your
-- account.
--
-- 'httpStatus', 'describeEventBusResponse_httpStatus' - The response's http status code.
newDescribeEventBusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeEventBusResponse
newDescribeEventBusResponse pHttpStatus_ =
  DescribeEventBusResponse'
    { arn = Core.Nothing,
      name = Core.Nothing,
      policy = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the account permitted to write events
-- to the current account.
describeEventBusResponse_arn :: Lens.Lens' DescribeEventBusResponse (Core.Maybe Core.Text)
describeEventBusResponse_arn = Lens.lens (\DescribeEventBusResponse' {arn} -> arn) (\s@DescribeEventBusResponse' {} a -> s {arn = a} :: DescribeEventBusResponse)

-- | The name of the event bus. Currently, this is always @default@.
describeEventBusResponse_name :: Lens.Lens' DescribeEventBusResponse (Core.Maybe Core.Text)
describeEventBusResponse_name = Lens.lens (\DescribeEventBusResponse' {name} -> name) (\s@DescribeEventBusResponse' {} a -> s {name = a} :: DescribeEventBusResponse)

-- | The policy that enables the external account to send events to your
-- account.
describeEventBusResponse_policy :: Lens.Lens' DescribeEventBusResponse (Core.Maybe Core.Text)
describeEventBusResponse_policy = Lens.lens (\DescribeEventBusResponse' {policy} -> policy) (\s@DescribeEventBusResponse' {} a -> s {policy = a} :: DescribeEventBusResponse)

-- | The response's http status code.
describeEventBusResponse_httpStatus :: Lens.Lens' DescribeEventBusResponse Core.Int
describeEventBusResponse_httpStatus = Lens.lens (\DescribeEventBusResponse' {httpStatus} -> httpStatus) (\s@DescribeEventBusResponse' {} a -> s {httpStatus = a} :: DescribeEventBusResponse)

instance Core.NFData DescribeEventBusResponse
