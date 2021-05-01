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
-- Module      : Network.AWS.Config.PutDeliveryChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a delivery channel object to deliver configuration information
-- to an Amazon S3 bucket and Amazon SNS topic.
--
-- Before you can create a delivery channel, you must create a
-- configuration recorder.
--
-- You can use this action to change the Amazon S3 bucket or an Amazon SNS
-- topic of the existing delivery channel. To change the Amazon S3 bucket
-- or an Amazon SNS topic, call this action and specify the changed values
-- for the S3 bucket and the SNS topic. If you specify a different value
-- for either the S3 bucket or the SNS topic, this action will keep the
-- existing value for the parameter that is not changed.
--
-- You can have only one delivery channel per region in your account.
module Network.AWS.Config.PutDeliveryChannel
  ( -- * Creating a Request
    PutDeliveryChannel (..),
    newPutDeliveryChannel,

    -- * Request Lenses
    putDeliveryChannel_deliveryChannel,

    -- * Destructuring the Response
    PutDeliveryChannelResponse (..),
    newPutDeliveryChannelResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the PutDeliveryChannel action.
--
-- /See:/ 'newPutDeliveryChannel' smart constructor.
data PutDeliveryChannel = PutDeliveryChannel'
  { -- | The configuration delivery channel object that delivers the
    -- configuration information to an Amazon S3 bucket and to an Amazon SNS
    -- topic.
    deliveryChannel :: DeliveryChannel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutDeliveryChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryChannel', 'putDeliveryChannel_deliveryChannel' - The configuration delivery channel object that delivers the
-- configuration information to an Amazon S3 bucket and to an Amazon SNS
-- topic.
newPutDeliveryChannel ::
  -- | 'deliveryChannel'
  DeliveryChannel ->
  PutDeliveryChannel
newPutDeliveryChannel pDeliveryChannel_ =
  PutDeliveryChannel'
    { deliveryChannel =
        pDeliveryChannel_
    }

-- | The configuration delivery channel object that delivers the
-- configuration information to an Amazon S3 bucket and to an Amazon SNS
-- topic.
putDeliveryChannel_deliveryChannel :: Lens.Lens' PutDeliveryChannel DeliveryChannel
putDeliveryChannel_deliveryChannel = Lens.lens (\PutDeliveryChannel' {deliveryChannel} -> deliveryChannel) (\s@PutDeliveryChannel' {} a -> s {deliveryChannel = a} :: PutDeliveryChannel)

instance Prelude.AWSRequest PutDeliveryChannel where
  type
    Rs PutDeliveryChannel =
      PutDeliveryChannelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull PutDeliveryChannelResponse'

instance Prelude.Hashable PutDeliveryChannel

instance Prelude.NFData PutDeliveryChannel

instance Prelude.ToHeaders PutDeliveryChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StarlingDoveService.PutDeliveryChannel" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON PutDeliveryChannel where
  toJSON PutDeliveryChannel' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DeliveryChannel" Prelude..= deliveryChannel)
          ]
      )

instance Prelude.ToPath PutDeliveryChannel where
  toPath = Prelude.const "/"

instance Prelude.ToQuery PutDeliveryChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutDeliveryChannelResponse' smart constructor.
data PutDeliveryChannelResponse = PutDeliveryChannelResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutDeliveryChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutDeliveryChannelResponse ::
  PutDeliveryChannelResponse
newPutDeliveryChannelResponse =
  PutDeliveryChannelResponse'

instance Prelude.NFData PutDeliveryChannelResponse
