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
-- Module      : Amazonka.Config.PutDeliveryChannel
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Config.PutDeliveryChannel
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the PutDeliveryChannel action.
--
-- /See:/ 'newPutDeliveryChannel' smart constructor.
data PutDeliveryChannel = PutDeliveryChannel'
  { -- | The configuration delivery channel object that delivers the
    -- configuration information to an Amazon S3 bucket and to an Amazon SNS
    -- topic.
    deliveryChannel :: DeliveryChannel
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest PutDeliveryChannel where
  type
    AWSResponse PutDeliveryChannel =
      PutDeliveryChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull PutDeliveryChannelResponse'

instance Prelude.Hashable PutDeliveryChannel where
  hashWithSalt _salt PutDeliveryChannel' {..} =
    _salt `Prelude.hashWithSalt` deliveryChannel

instance Prelude.NFData PutDeliveryChannel where
  rnf PutDeliveryChannel' {..} =
    Prelude.rnf deliveryChannel

instance Data.ToHeaders PutDeliveryChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.PutDeliveryChannel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON PutDeliveryChannel where
  toJSON PutDeliveryChannel' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DeliveryChannel" Data..= deliveryChannel)
          ]
      )

instance Data.ToPath PutDeliveryChannel where
  toPath = Prelude.const "/"

instance Data.ToQuery PutDeliveryChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newPutDeliveryChannelResponse' smart constructor.
data PutDeliveryChannelResponse = PutDeliveryChannelResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutDeliveryChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutDeliveryChannelResponse ::
  PutDeliveryChannelResponse
newPutDeliveryChannelResponse =
  PutDeliveryChannelResponse'

instance Prelude.NFData PutDeliveryChannelResponse where
  rnf _ = ()
