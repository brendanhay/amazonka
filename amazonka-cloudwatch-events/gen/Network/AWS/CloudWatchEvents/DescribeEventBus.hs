{-# LANGUAGE DeriveDataTypeable #-}
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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribeEventBus' smart constructor.
data DescribeEventBus = DescribeEventBus'
  { -- | The name or ARN of the event bus to show details for. If you omit this,
    -- the default event bus is displayed.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  DescribeEventBus' {name = Prelude.Nothing}

-- | The name or ARN of the event bus to show details for. If you omit this,
-- the default event bus is displayed.
describeEventBus_name :: Lens.Lens' DescribeEventBus (Prelude.Maybe Prelude.Text)
describeEventBus_name = Lens.lens (\DescribeEventBus' {name} -> name) (\s@DescribeEventBus' {} a -> s {name = a} :: DescribeEventBus)

instance Prelude.AWSRequest DescribeEventBus where
  type Rs DescribeEventBus = DescribeEventBusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventBusResponse'
            Prelude.<$> (x Prelude..?> "Arn")
            Prelude.<*> (x Prelude..?> "Name")
            Prelude.<*> (x Prelude..?> "Policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEventBus

instance Prelude.NFData DescribeEventBus

instance Prelude.ToHeaders DescribeEventBus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSEvents.DescribeEventBus" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeEventBus where
  toJSON DescribeEventBus' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("Name" Prelude..=) Prelude.<$> name]
      )

instance Prelude.ToPath DescribeEventBus where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeEventBus where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeEventBusResponse' smart constructor.
data DescribeEventBusResponse = DescribeEventBusResponse'
  { -- | The Amazon Resource Name (ARN) of the account permitted to write events
    -- to the current account.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The name of the event bus. Currently, this is always @default@.
    name :: Prelude.Maybe Prelude.Text,
    -- | The policy that enables the external account to send events to your
    -- account.
    policy :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DescribeEventBusResponse
newDescribeEventBusResponse pHttpStatus_ =
  DescribeEventBusResponse'
    { arn = Prelude.Nothing,
      name = Prelude.Nothing,
      policy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The Amazon Resource Name (ARN) of the account permitted to write events
-- to the current account.
describeEventBusResponse_arn :: Lens.Lens' DescribeEventBusResponse (Prelude.Maybe Prelude.Text)
describeEventBusResponse_arn = Lens.lens (\DescribeEventBusResponse' {arn} -> arn) (\s@DescribeEventBusResponse' {} a -> s {arn = a} :: DescribeEventBusResponse)

-- | The name of the event bus. Currently, this is always @default@.
describeEventBusResponse_name :: Lens.Lens' DescribeEventBusResponse (Prelude.Maybe Prelude.Text)
describeEventBusResponse_name = Lens.lens (\DescribeEventBusResponse' {name} -> name) (\s@DescribeEventBusResponse' {} a -> s {name = a} :: DescribeEventBusResponse)

-- | The policy that enables the external account to send events to your
-- account.
describeEventBusResponse_policy :: Lens.Lens' DescribeEventBusResponse (Prelude.Maybe Prelude.Text)
describeEventBusResponse_policy = Lens.lens (\DescribeEventBusResponse' {policy} -> policy) (\s@DescribeEventBusResponse' {} a -> s {policy = a} :: DescribeEventBusResponse)

-- | The response's http status code.
describeEventBusResponse_httpStatus :: Lens.Lens' DescribeEventBusResponse Prelude.Int
describeEventBusResponse_httpStatus = Lens.lens (\DescribeEventBusResponse' {httpStatus} -> httpStatus) (\s@DescribeEventBusResponse' {} a -> s {httpStatus = a} :: DescribeEventBusResponse)

instance Prelude.NFData DescribeEventBusResponse
