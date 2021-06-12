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
-- Module      : Network.AWS.Config.DeleteDeliveryChannel
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the delivery channel.
--
-- Before you can delete the delivery channel, you must stop the
-- configuration recorder by using the StopConfigurationRecorder action.
module Network.AWS.Config.DeleteDeliveryChannel
  ( -- * Creating a Request
    DeleteDeliveryChannel (..),
    newDeleteDeliveryChannel,

    -- * Request Lenses
    deleteDeliveryChannel_deliveryChannelName,

    -- * Destructuring the Response
    DeleteDeliveryChannelResponse (..),
    newDeleteDeliveryChannelResponse,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | The input for the DeleteDeliveryChannel action. The action accepts the
-- following data, in JSON format.
--
-- /See:/ 'newDeleteDeliveryChannel' smart constructor.
data DeleteDeliveryChannel = DeleteDeliveryChannel'
  { -- | The name of the delivery channel to delete.
    deliveryChannelName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDeliveryChannel' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'deliveryChannelName', 'deleteDeliveryChannel_deliveryChannelName' - The name of the delivery channel to delete.
newDeleteDeliveryChannel ::
  -- | 'deliveryChannelName'
  Core.Text ->
  DeleteDeliveryChannel
newDeleteDeliveryChannel pDeliveryChannelName_ =
  DeleteDeliveryChannel'
    { deliveryChannelName =
        pDeliveryChannelName_
    }

-- | The name of the delivery channel to delete.
deleteDeliveryChannel_deliveryChannelName :: Lens.Lens' DeleteDeliveryChannel Core.Text
deleteDeliveryChannel_deliveryChannelName = Lens.lens (\DeleteDeliveryChannel' {deliveryChannelName} -> deliveryChannelName) (\s@DeleteDeliveryChannel' {} a -> s {deliveryChannelName = a} :: DeleteDeliveryChannel)

instance Core.AWSRequest DeleteDeliveryChannel where
  type
    AWSResponse DeleteDeliveryChannel =
      DeleteDeliveryChannelResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveNull DeleteDeliveryChannelResponse'

instance Core.Hashable DeleteDeliveryChannel

instance Core.NFData DeleteDeliveryChannel

instance Core.ToHeaders DeleteDeliveryChannel where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DeleteDeliveryChannel" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteDeliveryChannel where
  toJSON DeleteDeliveryChannel' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("DeliveryChannelName" Core..= deliveryChannelName)
          ]
      )

instance Core.ToPath DeleteDeliveryChannel where
  toPath = Core.const "/"

instance Core.ToQuery DeleteDeliveryChannel where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteDeliveryChannelResponse' smart constructor.
data DeleteDeliveryChannelResponse = DeleteDeliveryChannelResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDeliveryChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDeliveryChannelResponse ::
  DeleteDeliveryChannelResponse
newDeleteDeliveryChannelResponse =
  DeleteDeliveryChannelResponse'

instance Core.NFData DeleteDeliveryChannelResponse
