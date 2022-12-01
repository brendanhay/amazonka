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
-- Module      : Amazonka.Config.DeleteDeliveryChannel
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the delivery channel.
--
-- Before you can delete the delivery channel, you must stop the
-- configuration recorder by using the StopConfigurationRecorder action.
module Amazonka.Config.DeleteDeliveryChannel
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

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | The input for the DeleteDeliveryChannel action. The action accepts the
-- following data, in JSON format.
--
-- /See:/ 'newDeleteDeliveryChannel' smart constructor.
data DeleteDeliveryChannel = DeleteDeliveryChannel'
  { -- | The name of the delivery channel to delete.
    deliveryChannelName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteDeliveryChannel
newDeleteDeliveryChannel pDeliveryChannelName_ =
  DeleteDeliveryChannel'
    { deliveryChannelName =
        pDeliveryChannelName_
    }

-- | The name of the delivery channel to delete.
deleteDeliveryChannel_deliveryChannelName :: Lens.Lens' DeleteDeliveryChannel Prelude.Text
deleteDeliveryChannel_deliveryChannelName = Lens.lens (\DeleteDeliveryChannel' {deliveryChannelName} -> deliveryChannelName) (\s@DeleteDeliveryChannel' {} a -> s {deliveryChannelName = a} :: DeleteDeliveryChannel)

instance Core.AWSRequest DeleteDeliveryChannel where
  type
    AWSResponse DeleteDeliveryChannel =
      DeleteDeliveryChannelResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull DeleteDeliveryChannelResponse'

instance Prelude.Hashable DeleteDeliveryChannel where
  hashWithSalt _salt DeleteDeliveryChannel' {..} =
    _salt `Prelude.hashWithSalt` deliveryChannelName

instance Prelude.NFData DeleteDeliveryChannel where
  rnf DeleteDeliveryChannel' {..} =
    Prelude.rnf deliveryChannelName

instance Core.ToHeaders DeleteDeliveryChannel where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.DeleteDeliveryChannel" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DeleteDeliveryChannel where
  toJSON DeleteDeliveryChannel' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("DeliveryChannelName" Core..= deliveryChannelName)
          ]
      )

instance Core.ToPath DeleteDeliveryChannel where
  toPath = Prelude.const "/"

instance Core.ToQuery DeleteDeliveryChannel where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteDeliveryChannelResponse' smart constructor.
data DeleteDeliveryChannelResponse = DeleteDeliveryChannelResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteDeliveryChannelResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteDeliveryChannelResponse ::
  DeleteDeliveryChannelResponse
newDeleteDeliveryChannelResponse =
  DeleteDeliveryChannelResponse'

instance Prelude.NFData DeleteDeliveryChannelResponse where
  rnf _ = ()
