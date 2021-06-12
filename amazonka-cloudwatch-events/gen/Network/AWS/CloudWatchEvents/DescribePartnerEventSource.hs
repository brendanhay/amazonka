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
-- Module      : Network.AWS.CloudWatchEvents.DescribePartnerEventSource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- An SaaS partner can use this operation to list details about a partner
-- event source that they have created. AWS customers do not use this
-- operation. Instead, AWS customers can use DescribeEventSource to see
-- details about a partner event source that is shared with them.
module Network.AWS.CloudWatchEvents.DescribePartnerEventSource
  ( -- * Creating a Request
    DescribePartnerEventSource (..),
    newDescribePartnerEventSource,

    -- * Request Lenses
    describePartnerEventSource_name,

    -- * Destructuring the Response
    DescribePartnerEventSourceResponse (..),
    newDescribePartnerEventSourceResponse,

    -- * Response Lenses
    describePartnerEventSourceResponse_arn,
    describePartnerEventSourceResponse_name,
    describePartnerEventSourceResponse_httpStatus,
  )
where

import Network.AWS.CloudWatchEvents.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDescribePartnerEventSource' smart constructor.
data DescribePartnerEventSource = DescribePartnerEventSource'
  { -- | The name of the event source to display.
    name :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePartnerEventSource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describePartnerEventSource_name' - The name of the event source to display.
newDescribePartnerEventSource ::
  -- | 'name'
  Core.Text ->
  DescribePartnerEventSource
newDescribePartnerEventSource pName_ =
  DescribePartnerEventSource' {name = pName_}

-- | The name of the event source to display.
describePartnerEventSource_name :: Lens.Lens' DescribePartnerEventSource Core.Text
describePartnerEventSource_name = Lens.lens (\DescribePartnerEventSource' {name} -> name) (\s@DescribePartnerEventSource' {} a -> s {name = a} :: DescribePartnerEventSource)

instance Core.AWSRequest DescribePartnerEventSource where
  type
    AWSResponse DescribePartnerEventSource =
      DescribePartnerEventSourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePartnerEventSourceResponse'
            Core.<$> (x Core..?> "Arn")
            Core.<*> (x Core..?> "Name")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribePartnerEventSource

instance Core.NFData DescribePartnerEventSource

instance Core.ToHeaders DescribePartnerEventSource where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSEvents.DescribePartnerEventSource" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribePartnerEventSource where
  toJSON DescribePartnerEventSource' {..} =
    Core.object
      (Core.catMaybes [Core.Just ("Name" Core..= name)])

instance Core.ToPath DescribePartnerEventSource where
  toPath = Core.const "/"

instance Core.ToQuery DescribePartnerEventSource where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribePartnerEventSourceResponse' smart constructor.
data DescribePartnerEventSourceResponse = DescribePartnerEventSourceResponse'
  { -- | The ARN of the event source.
    arn :: Core.Maybe Core.Text,
    -- | The name of the event source.
    name :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribePartnerEventSourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describePartnerEventSourceResponse_arn' - The ARN of the event source.
--
-- 'name', 'describePartnerEventSourceResponse_name' - The name of the event source.
--
-- 'httpStatus', 'describePartnerEventSourceResponse_httpStatus' - The response's http status code.
newDescribePartnerEventSourceResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribePartnerEventSourceResponse
newDescribePartnerEventSourceResponse pHttpStatus_ =
  DescribePartnerEventSourceResponse'
    { arn =
        Core.Nothing,
      name = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the event source.
describePartnerEventSourceResponse_arn :: Lens.Lens' DescribePartnerEventSourceResponse (Core.Maybe Core.Text)
describePartnerEventSourceResponse_arn = Lens.lens (\DescribePartnerEventSourceResponse' {arn} -> arn) (\s@DescribePartnerEventSourceResponse' {} a -> s {arn = a} :: DescribePartnerEventSourceResponse)

-- | The name of the event source.
describePartnerEventSourceResponse_name :: Lens.Lens' DescribePartnerEventSourceResponse (Core.Maybe Core.Text)
describePartnerEventSourceResponse_name = Lens.lens (\DescribePartnerEventSourceResponse' {name} -> name) (\s@DescribePartnerEventSourceResponse' {} a -> s {name = a} :: DescribePartnerEventSourceResponse)

-- | The response's http status code.
describePartnerEventSourceResponse_httpStatus :: Lens.Lens' DescribePartnerEventSourceResponse Core.Int
describePartnerEventSourceResponse_httpStatus = Lens.lens (\DescribePartnerEventSourceResponse' {httpStatus} -> httpStatus) (\s@DescribePartnerEventSourceResponse' {} a -> s {httpStatus = a} :: DescribePartnerEventSourceResponse)

instance
  Core.NFData
    DescribePartnerEventSourceResponse
