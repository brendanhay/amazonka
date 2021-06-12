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
-- Module      : Network.AWS.Config.DescribeDeliveryChannels
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about the specified delivery channel. If a delivery
-- channel is not specified, this action returns the details of all
-- delivery channels associated with the account.
--
-- Currently, you can specify only one delivery channel per region in your
-- account.
module Network.AWS.Config.DescribeDeliveryChannels
  ( -- * Creating a Request
    DescribeDeliveryChannels (..),
    newDescribeDeliveryChannels,

    -- * Request Lenses
    describeDeliveryChannels_deliveryChannelNames,

    -- * Destructuring the Response
    DescribeDeliveryChannelsResponse (..),
    newDescribeDeliveryChannelsResponse,

    -- * Response Lenses
    describeDeliveryChannelsResponse_deliveryChannels,
    describeDeliveryChannelsResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DescribeDeliveryChannels action.
--
-- /See:/ 'newDescribeDeliveryChannels' smart constructor.
data DescribeDeliveryChannels = DescribeDeliveryChannels'
  { -- | A list of delivery channel names.
    deliveryChannelNames :: Core.Maybe [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDeliveryChannels' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryChannelNames', 'describeDeliveryChannels_deliveryChannelNames' - A list of delivery channel names.
newDescribeDeliveryChannels ::
  DescribeDeliveryChannels
newDescribeDeliveryChannels =
  DescribeDeliveryChannels'
    { deliveryChannelNames =
        Core.Nothing
    }

-- | A list of delivery channel names.
describeDeliveryChannels_deliveryChannelNames :: Lens.Lens' DescribeDeliveryChannels (Core.Maybe [Core.Text])
describeDeliveryChannels_deliveryChannelNames = Lens.lens (\DescribeDeliveryChannels' {deliveryChannelNames} -> deliveryChannelNames) (\s@DescribeDeliveryChannels' {} a -> s {deliveryChannelNames = a} :: DescribeDeliveryChannels) Core.. Lens.mapping Lens._Coerce

instance Core.AWSRequest DescribeDeliveryChannels where
  type
    AWSResponse DescribeDeliveryChannels =
      DescribeDeliveryChannelsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeliveryChannelsResponse'
            Core.<$> (x Core..?> "DeliveryChannels" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeDeliveryChannels

instance Core.NFData DescribeDeliveryChannels

instance Core.ToHeaders DescribeDeliveryChannels where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeDeliveryChannels" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeDeliveryChannels where
  toJSON DescribeDeliveryChannels' {..} =
    Core.object
      ( Core.catMaybes
          [ ("DeliveryChannelNames" Core..=)
              Core.<$> deliveryChannelNames
          ]
      )

instance Core.ToPath DescribeDeliveryChannels where
  toPath = Core.const "/"

instance Core.ToQuery DescribeDeliveryChannels where
  toQuery = Core.const Core.mempty

-- | The output for the DescribeDeliveryChannels action.
--
-- /See:/ 'newDescribeDeliveryChannelsResponse' smart constructor.
data DescribeDeliveryChannelsResponse = DescribeDeliveryChannelsResponse'
  { -- | A list that contains the descriptions of the specified delivery channel.
    deliveryChannels :: Core.Maybe [DeliveryChannel],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeDeliveryChannelsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryChannels', 'describeDeliveryChannelsResponse_deliveryChannels' - A list that contains the descriptions of the specified delivery channel.
--
-- 'httpStatus', 'describeDeliveryChannelsResponse_httpStatus' - The response's http status code.
newDescribeDeliveryChannelsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeDeliveryChannelsResponse
newDescribeDeliveryChannelsResponse pHttpStatus_ =
  DescribeDeliveryChannelsResponse'
    { deliveryChannels =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list that contains the descriptions of the specified delivery channel.
describeDeliveryChannelsResponse_deliveryChannels :: Lens.Lens' DescribeDeliveryChannelsResponse (Core.Maybe [DeliveryChannel])
describeDeliveryChannelsResponse_deliveryChannels = Lens.lens (\DescribeDeliveryChannelsResponse' {deliveryChannels} -> deliveryChannels) (\s@DescribeDeliveryChannelsResponse' {} a -> s {deliveryChannels = a} :: DescribeDeliveryChannelsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeDeliveryChannelsResponse_httpStatus :: Lens.Lens' DescribeDeliveryChannelsResponse Core.Int
describeDeliveryChannelsResponse_httpStatus = Lens.lens (\DescribeDeliveryChannelsResponse' {httpStatus} -> httpStatus) (\s@DescribeDeliveryChannelsResponse' {} a -> s {httpStatus = a} :: DescribeDeliveryChannelsResponse)

instance Core.NFData DescribeDeliveryChannelsResponse
