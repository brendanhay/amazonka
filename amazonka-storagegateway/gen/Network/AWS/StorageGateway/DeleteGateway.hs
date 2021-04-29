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
-- Module      : Network.AWS.StorageGateway.DeleteGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a gateway. To specify which gateway to delete, use the Amazon
-- Resource Name (ARN) of the gateway in your request. The operation
-- deletes the gateway; however, it does not delete the gateway virtual
-- machine (VM) from your host computer.
--
-- After you delete a gateway, you cannot reactivate it. Completed
-- snapshots of the gateway volumes are not deleted upon deleting the
-- gateway, however, pending snapshots will not complete. After you delete
-- a gateway, your next step is to remove it from your environment.
--
-- You no longer pay software charges after the gateway is deleted;
-- however, your existing Amazon EBS snapshots persist and you will
-- continue to be billed for these snapshots. You can choose to remove all
-- remaining Amazon EBS snapshots by canceling your Amazon EC2
-- subscription.  If you prefer not to cancel your Amazon EC2 subscription,
-- you can delete your snapshots using the Amazon EC2 console. For more
-- information, see the
-- <http://aws.amazon.com/storagegateway AWS Storage Gateway detail page>.
module Network.AWS.StorageGateway.DeleteGateway
  ( -- * Creating a Request
    DeleteGateway (..),
    newDeleteGateway,

    -- * Request Lenses
    deleteGateway_gatewayARN,

    -- * Destructuring the Response
    DeleteGatewayResponse (..),
    newDeleteGatewayResponse,

    -- * Response Lenses
    deleteGatewayResponse_gatewayARN,
    deleteGatewayResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the ID of the gateway to delete.
--
-- /See:/ 'newDeleteGateway' smart constructor.
data DeleteGateway = DeleteGateway'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'deleteGateway_gatewayARN' - Undocumented member.
newDeleteGateway ::
  -- | 'gatewayARN'
  Prelude.Text ->
  DeleteGateway
newDeleteGateway pGatewayARN_ =
  DeleteGateway' {gatewayARN = pGatewayARN_}

-- | Undocumented member.
deleteGateway_gatewayARN :: Lens.Lens' DeleteGateway Prelude.Text
deleteGateway_gatewayARN = Lens.lens (\DeleteGateway' {gatewayARN} -> gatewayARN) (\s@DeleteGateway' {} a -> s {gatewayARN = a} :: DeleteGateway)

instance Prelude.AWSRequest DeleteGateway where
  type Rs DeleteGateway = DeleteGatewayResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteGatewayResponse'
            Prelude.<$> (x Prelude..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteGateway

instance Prelude.NFData DeleteGateway

instance Prelude.ToHeaders DeleteGateway where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.DeleteGateway" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteGateway where
  toJSON DeleteGateway' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Prelude..= gatewayARN)]
      )

instance Prelude.ToPath DeleteGateway where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteGateway where
  toQuery = Prelude.const Prelude.mempty

-- | A JSON object containing the ID of the deleted gateway.
--
-- /See:/ 'newDeleteGatewayResponse' smart constructor.
data DeleteGatewayResponse = DeleteGatewayResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'DeleteGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'deleteGatewayResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'deleteGatewayResponse_httpStatus' - The response's http status code.
newDeleteGatewayResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteGatewayResponse
newDeleteGatewayResponse pHttpStatus_ =
  DeleteGatewayResponse'
    { gatewayARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteGatewayResponse_gatewayARN :: Lens.Lens' DeleteGatewayResponse (Prelude.Maybe Prelude.Text)
deleteGatewayResponse_gatewayARN = Lens.lens (\DeleteGatewayResponse' {gatewayARN} -> gatewayARN) (\s@DeleteGatewayResponse' {} a -> s {gatewayARN = a} :: DeleteGatewayResponse)

-- | The response's http status code.
deleteGatewayResponse_httpStatus :: Lens.Lens' DeleteGatewayResponse Prelude.Int
deleteGatewayResponse_httpStatus = Lens.lens (\DeleteGatewayResponse' {httpStatus} -> httpStatus) (\s@DeleteGatewayResponse' {} a -> s {httpStatus = a} :: DeleteGatewayResponse)

instance Prelude.NFData DeleteGatewayResponse
