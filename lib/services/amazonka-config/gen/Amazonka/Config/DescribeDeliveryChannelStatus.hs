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
-- Module      : Amazonka.Config.DescribeDeliveryChannelStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
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
module Amazonka.Config.DescribeDeliveryChannelStatus
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

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
describeDeliveryChannelStatus_deliveryChannelNames = Lens.lens (\DescribeDeliveryChannelStatus' {deliveryChannelNames} -> deliveryChannelNames) (\s@DescribeDeliveryChannelStatus' {} a -> s {deliveryChannelNames = a} :: DescribeDeliveryChannelStatus) Prelude.. Lens.mapping Lens.coerced

instance
  Core.AWSRequest
    DescribeDeliveryChannelStatus
  where
  type
    AWSResponse DescribeDeliveryChannelStatus =
      DescribeDeliveryChannelStatusResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeliveryChannelStatusResponse'
            Prelude.<$> ( x
                            Data..?> "DeliveryChannelsStatus"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeDeliveryChannelStatus
  where
  hashWithSalt _salt DescribeDeliveryChannelStatus' {..} =
    _salt `Prelude.hashWithSalt` deliveryChannelNames

instance Prelude.NFData DescribeDeliveryChannelStatus where
  rnf DescribeDeliveryChannelStatus' {..} =
    Prelude.rnf deliveryChannelNames

instance Data.ToHeaders DescribeDeliveryChannelStatus where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeDeliveryChannelStatus" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDeliveryChannelStatus where
  toJSON DescribeDeliveryChannelStatus' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeliveryChannelNames" Data..=)
              Prelude.<$> deliveryChannelNames
          ]
      )

instance Data.ToPath DescribeDeliveryChannelStatus where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDeliveryChannelStatus where
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
describeDeliveryChannelStatusResponse_deliveryChannelsStatus = Lens.lens (\DescribeDeliveryChannelStatusResponse' {deliveryChannelsStatus} -> deliveryChannelsStatus) (\s@DescribeDeliveryChannelStatusResponse' {} a -> s {deliveryChannelsStatus = a} :: DescribeDeliveryChannelStatusResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDeliveryChannelStatusResponse_httpStatus :: Lens.Lens' DescribeDeliveryChannelStatusResponse Prelude.Int
describeDeliveryChannelStatusResponse_httpStatus = Lens.lens (\DescribeDeliveryChannelStatusResponse' {httpStatus} -> httpStatus) (\s@DescribeDeliveryChannelStatusResponse' {} a -> s {httpStatus = a} :: DescribeDeliveryChannelStatusResponse)

instance
  Prelude.NFData
    DescribeDeliveryChannelStatusResponse
  where
  rnf DescribeDeliveryChannelStatusResponse' {..} =
    Prelude.rnf deliveryChannelsStatus
      `Prelude.seq` Prelude.rnf httpStatus
