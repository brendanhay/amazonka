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
-- Module      : Network.AWS.MQ.DeleteBroker
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a broker. Note: This API is asynchronous.
module Network.AWS.MQ.DeleteBroker
  ( -- * Creating a Request
    DeleteBroker (..),
    newDeleteBroker,

    -- * Request Lenses
    deleteBroker_brokerId,

    -- * Destructuring the Response
    DeleteBrokerResponse (..),
    newDeleteBrokerResponse,

    -- * Response Lenses
    deleteBrokerResponse_brokerId,
    deleteBrokerResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MQ.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteBroker' smart constructor.
data DeleteBroker = DeleteBroker'
  { -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteBroker' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'brokerId', 'deleteBroker_brokerId' - The unique ID that Amazon MQ generates for the broker.
newDeleteBroker ::
  -- | 'brokerId'
  Core.Text ->
  DeleteBroker
newDeleteBroker pBrokerId_ =
  DeleteBroker' {brokerId = pBrokerId_}

-- | The unique ID that Amazon MQ generates for the broker.
deleteBroker_brokerId :: Lens.Lens' DeleteBroker Core.Text
deleteBroker_brokerId = Lens.lens (\DeleteBroker' {brokerId} -> brokerId) (\s@DeleteBroker' {} a -> s {brokerId = a} :: DeleteBroker)

instance Core.AWSRequest DeleteBroker where
  type AWSResponse DeleteBroker = DeleteBrokerResponse
  request = Request.delete defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBrokerResponse'
            Core.<$> (x Core..?> "brokerId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteBroker

instance Core.NFData DeleteBroker

instance Core.ToHeaders DeleteBroker where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath DeleteBroker where
  toPath DeleteBroker' {..} =
    Core.mconcat ["/v1/brokers/", Core.toBS brokerId]

instance Core.ToQuery DeleteBroker where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteBrokerResponse' smart constructor.
data DeleteBrokerResponse = DeleteBrokerResponse'
  { -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteBrokerResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'brokerId', 'deleteBrokerResponse_brokerId' - The unique ID that Amazon MQ generates for the broker.
--
-- 'httpStatus', 'deleteBrokerResponse_httpStatus' - The response's http status code.
newDeleteBrokerResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteBrokerResponse
newDeleteBrokerResponse pHttpStatus_ =
  DeleteBrokerResponse'
    { brokerId = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID that Amazon MQ generates for the broker.
deleteBrokerResponse_brokerId :: Lens.Lens' DeleteBrokerResponse (Core.Maybe Core.Text)
deleteBrokerResponse_brokerId = Lens.lens (\DeleteBrokerResponse' {brokerId} -> brokerId) (\s@DeleteBrokerResponse' {} a -> s {brokerId = a} :: DeleteBrokerResponse)

-- | The response's http status code.
deleteBrokerResponse_httpStatus :: Lens.Lens' DeleteBrokerResponse Core.Int
deleteBrokerResponse_httpStatus = Lens.lens (\DeleteBrokerResponse' {httpStatus} -> httpStatus) (\s@DeleteBrokerResponse' {} a -> s {httpStatus = a} :: DeleteBrokerResponse)

instance Core.NFData DeleteBrokerResponse
