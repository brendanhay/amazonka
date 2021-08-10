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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeliveryChannelStatus action.
--
-- /See:/ 'newDescribeDeliveryChannelStatus' smart constructor.
data DescribeDeliveryChannelStatus = DescribeDeliveryChannelStatus'
  { -- | A list of delivery channel names.
    deliveryChannelNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | A list of delivery channel names.
describeDeliveryChannelStatus_deliveryChannelNames :: Lens.Lens' DescribeDeliveryChannelStatus (Prelude.Maybe [Prelude.Text])
describeDeliveryChannelStatus_deliveryChannelNames = Lens.lens (\DescribeDeliveryChannelStatus' {deliveryChannelNames} -> deliveryChannelNames) (\s@DescribeDeliveryChannelStatus' {} a -> s {deliveryChannelNames = a} :: DescribeDeliveryChannelStatus) Prelude.. Lens.mapping Lens._Coerce

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
            Prelude.<$> ( x Core..?> "DeliveryChannelsStatus"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDeliveryChannelStatus

instance Prelude.NFData DescribeDeliveryChannelStatus

instance Core.ToHeaders DescribeDeliveryChannelStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DescribeDeliveryChannelStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeDeliveryChannelStatus where
  toJSON DescribeDeliveryChannelStatus' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("DeliveryChannelNames" Core..=)
              Prelude.<$> deliveryChannelNames
          ]
      )

instance Core.ToPath DescribeDeliveryChannelStatus where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeDeliveryChannelStatus where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the DescribeDeliveryChannelStatus action.
--
-- /See:/ 'newDescribeDeliveryChannelStatusResponse' smart constructor.
data DescribeDeliveryChannelStatusResponse = DescribeDeliveryChannelStatusResponse'
  { -- | A list that contains the status of a specified delivery channel.
    deliveryChannelsStatus :: Prelude.Maybe [DeliveryChannelStatus],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeDeliveryChannelStatusResponse
newDescribeDeliveryChannelStatusResponse pHttpStatus_ =
  DescribeDeliveryChannelStatusResponse'
    { deliveryChannelsStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list that contains the status of a specified delivery channel.
describeDeliveryChannelStatusResponse_deliveryChannelsStatus :: Lens.Lens' DescribeDeliveryChannelStatusResponse (Prelude.Maybe [DeliveryChannelStatus])
describeDeliveryChannelStatusResponse_deliveryChannelsStatus = Lens.lens (\DescribeDeliveryChannelStatusResponse' {deliveryChannelsStatus} -> deliveryChannelsStatus) (\s@DescribeDeliveryChannelStatusResponse' {} a -> s {deliveryChannelsStatus = a} :: DescribeDeliveryChannelStatusResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeDeliveryChannelStatusResponse_httpStatus :: Lens.Lens' DescribeDeliveryChannelStatusResponse Prelude.Int
describeDeliveryChannelStatusResponse_httpStatus = Lens.lens (\DescribeDeliveryChannelStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeDeliveryChannelStatusResponse' {} a -> s {httpStatus = a} :: DescribeDeliveryChannelStatusResponse)

instance
  Prelude.NFData
    DescribeDeliveryChannelStatusResponse
