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
-- Module      : Network.AWS.Config.DescribeDeliveryChannelStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the current status of the specified delivery channel. If a
-- delivery channel is not specified, this action returns the current
-- status of all delivery channels associated with the account.
--
-- Currently, you can specify only one delivery channel per region in your
-- account.
module Network.AWS.Config.DescribeDeliveryChannelStatus
  ( -- * Creating a Request
    DescribeDeliveryChannelStatus (..),
    newDescribeDeliveryChannelStatus,

    -- * Request Lenses
    describeDeliveryChannelStatus_deliveryChannelNames,

    -- * Destructuring the Response
    DescribeDeliveryChannelStatusResponse (..),
    newDescribeDeliveryChannelStatusResponse,

    -- * Response Lenses
    describeDeliveryChannelStatusResponse_deliveryChannelsStatus,
    describeDeliveryChannelStatusResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeliveryChannelStatus action.
--
-- /See:/ 'newDescribeDeliveryChannelStatus' smart constructor.
data DescribeDeliveryChannelStatus = DescribeDeliveryChannelStatus'
  { -- | A list of delivery channel names.
    deliveryChannelNames :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDeliveryChannelStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryChannelNames', 'describeDeliveryChannelStatus_deliveryChannelNames' - A list of delivery channel names.
newDescribeDeliveryChannelStatus ::
  DescribeDeliveryChannelStatus
newDescribeDeliveryChannelStatus =
  DescribeDeliveryChannelStatus'
    { deliveryChannelNames =
        Core.Nothing
    }

-- | A list of delivery channel names.
describeDeliveryChannelStatus_deliveryChannelNames :: Lens.Lens' DescribeDeliveryChannelStatus (Core.Maybe [Core.Text])
describeDeliveryChannelStatus_deliveryChannelNames = Lens.lens (\DescribeDeliveryChannelStatus' {deliveryChannelNames} -> deliveryChannelNames) (\s@DescribeDeliveryChannelStatus' {} a -> s {deliveryChannelNames = a} :: DescribeDeliveryChannelStatus) Core.. Lens.mapping Lens._Coerce

instance
  Core.AWSRequest
    DescribeDeliveryChannelStatus
  where
  type
    AWSResponse DescribeDeliveryChannelStatus =
      DescribeDeliveryChannelStatusResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeliveryChannelStatusResponse'
            Core.<$> ( x Core..?> "DeliveryChannelsStatus"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDeliveryChannelStatus

instance Core.NFData DescribeDeliveryChannelStatus

instance Core.ToHeaders DescribeDeliveryChannelStatus where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeDeliveryChannelStatus" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeDeliveryChannelStatus where
  toJSON DescribeDeliveryChannelStatus' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeliveryChannelNames" Core..=)
              Core.<$> deliveryChannelNames
          ]
      )

instance Core.ToPath DescribeDeliveryChannelStatus where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDeliveryChannelStatus where
  toQuery = Core.const Core.mempty

-- | The output for the DescribeDeliveryChannelStatus action.
--
-- /See:/ 'newDescribeDeliveryChannelStatusResponse' smart constructor.
data DescribeDeliveryChannelStatusResponse = DescribeDeliveryChannelStatusResponse'
  { -- | A list that contains the status of a specified delivery channel.
    deliveryChannelsStatus :: Core.Maybe [DeliveryChannelStatus],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDeliveryChannelStatusResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryChannelsStatus', 'describeDeliveryChannelStatusResponse_deliveryChannelsStatus' - A list that contains the status of a specified delivery channel.
--
-- 'httpStatus', 'describeDeliveryChannelStatusResponse_httpStatus' - The response's http status code.
newDescribeDeliveryChannelStatusResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDeliveryChannelStatusResponse
newDescribeDeliveryChannelStatusResponse pHttpStatus_ =
  DescribeDeliveryChannelStatusResponse'
    { deliveryChannelsStatus =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list that contains the status of a specified delivery channel.
describeDeliveryChannelStatusResponse_deliveryChannelsStatus :: Lens.Lens' DescribeDeliveryChannelStatusResponse (Core.Maybe [DeliveryChannelStatus])
describeDeliveryChannelStatusResponse_deliveryChannelsStatus = Lens.lens (\DescribeDeliveryChannelStatusResponse' {deliveryChannelsStatus} -> deliveryChannelsStatus) (\s@DescribeDeliveryChannelStatusResponse' {} a -> s {deliveryChannelsStatus = a} :: DescribeDeliveryChannelStatusResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeDeliveryChannelStatusResponse_httpStatus :: Lens.Lens' DescribeDeliveryChannelStatusResponse Core.Int
describeDeliveryChannelStatusResponse_httpStatus = Lens.lens (\DescribeDeliveryChannelStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeDeliveryChannelStatusResponse' {} a -> s {httpStatus = a} :: DescribeDeliveryChannelStatusResponse)

instance
  Core.NFData
    DescribeDeliveryChannelStatusResponse
