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
-- Module      : Amazonka.CloudWatchEvents.DescribeEventBus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Displays details about an event bus in your account. This can include
-- the external Amazon Web Services accounts that are permitted to write
-- events to your default event bus, and the associated policy. For custom
-- event buses and partner event buses, it displays the name, ARN, policy,
-- state, and creation time.
--
-- To enable your account to receive events from other accounts on its
-- default event bus, use
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_PutPermission.html PutPermission>.
--
-- For more information about partner event buses, see
-- <https://docs.aws.amazon.com/eventbridge/latest/APIReference/API_CreateEventBus.html CreateEventBus>.
module Amazonka.CloudWatchEvents.DescribeEventBus
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

import Amazonka.CloudWatchEvents.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribeEventBus' smart constructor.
data DescribeEventBus = DescribeEventBus'
  { -- | The name or ARN of the event bus to show details for. If you omit this,
    -- the default event bus is displayed.
    name :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DescribeEventBus where
  type
    AWSResponse DescribeEventBus =
      DescribeEventBusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeEventBusResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Policy")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeEventBus where
  hashWithSalt _salt DescribeEventBus' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DescribeEventBus where
  rnf DescribeEventBus' {..} = Prelude.rnf name

instance Data.ToHeaders DescribeEventBus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSEvents.DescribeEventBus" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeEventBus where
  toJSON DescribeEventBus' {..} =
    Data.object
      ( Prelude.catMaybes
          [("Name" Data..=) Prelude.<$> name]
      )

instance Data.ToPath DescribeEventBus where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeEventBus where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Prelude.NFData DescribeEventBusResponse where
  rnf DescribeEventBusResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf policy
      `Prelude.seq` Prelude.rnf httpStatus
