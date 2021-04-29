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
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DescribeDeliveryChannels action.
--
-- /See:/ 'newDescribeDeliveryChannels' smart constructor.
data DescribeDeliveryChannels = DescribeDeliveryChannels'
  { -- | A list of delivery channel names.
    deliveryChannelNames :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeDeliveryChannels_deliveryChannelNames = Lens.lens (\DescribeDeliveryChannels' {deliveryChannelNames} -> deliveryChannelNames) (\s@DescribeDeliveryChannels' {} a -> s {deliveryChannelNames = a} :: DescribeDeliveryChannels) Prelude.. Lens.mapping Prelude._Coerce

instance Prelude.AWSRequest DescribeDeliveryChannels where
  type
    Rs DescribeDeliveryChannels =
      DescribeDeliveryChannelsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeDeliveryChannelsResponse'
            Prelude.<$> ( x Prelude..?> "DeliveryChannels"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeDeliveryChannels

instance Prelude.NFData DescribeDeliveryChannels

instance Prelude.ToHeaders DescribeDeliveryChannels where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.DescribeDeliveryChannels" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DescribeDeliveryChannels where
  toJSON DescribeDeliveryChannels' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("DeliveryChannelNames" Prelude..=)
              Prelude.<$> deliveryChannelNames
          ]
      )

instance Prelude.ToPath DescribeDeliveryChannels where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DescribeDeliveryChannels where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
describeDeliveryChannelsResponse_deliveryChannels = Lens.lens (\DescribeDeliveryChannelsResponse' {deliveryChannels} -> deliveryChannels) (\s@DescribeDeliveryChannelsResponse' {} a -> s {deliveryChannels = a} :: DescribeDeliveryChannelsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
describeDeliveryChannelsResponse_httpStatus :: Lens.Lens' DescribeDeliveryChannelsResponse Prelude.Int
describeDeliveryChannelsResponse_httpStatus = Lens.lens (\DescribeDeliveryChannelsResponse' {httpStatus} -> httpStatus) (\s@DescribeDeliveryChannelsResponse' {} a -> s {httpStatus = a} :: DescribeDeliveryChannelsResponse)

instance
  Prelude.NFData
    DescribeDeliveryChannelsResponse
