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
-- Module      : Amazonka.Config.DescribeDeliveryChannels
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about the specified delivery channel. If a delivery
-- channel is not specified, this action returns the details of all
-- delivery channels associated with the account.
--
-- Currently, you can specify only one delivery channel per region in your
-- account.
module Amazonka.Config.DescribeDeliveryChannels
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the DescribeDeliveryChannels action.
--
-- /See:/ 'newDescribeDeliveryChannels' smart constructor.
data DescribeDeliveryChannels = DescribeDeliveryChannels'
  { -- | A list of delivery channel names.
    deliveryChannelNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | A list of delivery channel names.
describeDeliveryChannels_deliveryChannelNames :: Lens.Lens' DescribeDeliveryChannels (Prelude.Maybe [Prelude.Text])
describeDeliveryChannels_deliveryChannelNames = Lens.lens (\DescribeDeliveryChannels' {deliveryChannelNames} -> deliveryChannelNames) (\s@DescribeDeliveryChannels' {} a -> s {deliveryChannelNames = a} :: DescribeDeliveryChannels) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest DescribeDeliveryChannels where
  type
    AWSResponse DescribeDeliveryChannels =
      DescribeDeliveryChannelsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeliveryChannelsResponse'
            Prelude.<$> ( x
                            Data..?> "DeliveryChannels"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDeliveryChannels where
  hashWithSalt _salt DescribeDeliveryChannels' {..} =
    _salt `Prelude.hashWithSalt` deliveryChannelNames

instance Prelude.NFData DescribeDeliveryChannels where
  rnf DescribeDeliveryChannels' {..} =
    Prelude.rnf deliveryChannelNames

instance Data.ToHeaders DescribeDeliveryChannels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.DescribeDeliveryChannels" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeDeliveryChannels where
  toJSON DescribeDeliveryChannels' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeliveryChannelNames" Data..=)
              Prelude.<$> deliveryChannelNames
          ]
      )

instance Data.ToPath DescribeDeliveryChannels where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeDeliveryChannels where
  toQuery = Prelude.const Prelude.mempty

-- | The output for the DescribeDeliveryChannels action.
--
-- /See:/ 'newDescribeDeliveryChannelsResponse' smart constructor.
data DescribeDeliveryChannelsResponse = DescribeDeliveryChannelsResponse'
  { -- | A list that contains the descriptions of the specified delivery channel.
    deliveryChannels :: Prelude.Maybe [DeliveryChannel],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeDeliveryChannelsResponse
newDescribeDeliveryChannelsResponse pHttpStatus_ =
  DescribeDeliveryChannelsResponse'
    { deliveryChannels =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list that contains the descriptions of the specified delivery channel.
describeDeliveryChannelsResponse_deliveryChannels :: Lens.Lens' DescribeDeliveryChannelsResponse (Prelude.Maybe [DeliveryChannel])
describeDeliveryChannelsResponse_deliveryChannels = Lens.lens (\DescribeDeliveryChannelsResponse' {deliveryChannels} -> deliveryChannels) (\s@DescribeDeliveryChannelsResponse' {} a -> s {deliveryChannels = a} :: DescribeDeliveryChannelsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeDeliveryChannelsResponse_httpStatus :: Lens.Lens' DescribeDeliveryChannelsResponse Prelude.Int
describeDeliveryChannelsResponse_httpStatus = Lens.lens (\DescribeDeliveryChannelsResponse' {httpStatus} -> httpStatus) (\s@DescribeDeliveryChannelsResponse' {} a -> s {httpStatus = a} :: DescribeDeliveryChannelsResponse)

instance
  Prelude.NFData
    DescribeDeliveryChannelsResponse
  where
  rnf DescribeDeliveryChannelsResponse' {..} =
    Prelude.rnf deliveryChannels `Prelude.seq`
      Prelude.rnf httpStatus
