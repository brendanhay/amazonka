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
-- Module      : Amazonka.MQ.DeleteBroker
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a broker. Note: This API is asynchronous.
module Amazonka.MQ.DeleteBroker
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MQ.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteBroker' smart constructor.
data DeleteBroker = DeleteBroker'
  { -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteBroker
newDeleteBroker pBrokerId_ =
  DeleteBroker' {brokerId = pBrokerId_}

-- | The unique ID that Amazon MQ generates for the broker.
deleteBroker_brokerId :: Lens.Lens' DeleteBroker Prelude.Text
deleteBroker_brokerId = Lens.lens (\DeleteBroker' {brokerId} -> brokerId) (\s@DeleteBroker' {} a -> s {brokerId = a} :: DeleteBroker)

instance Core.AWSRequest DeleteBroker where
  type AWSResponse DeleteBroker = DeleteBrokerResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBrokerResponse'
            Prelude.<$> (x Data..?> "brokerId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBroker where
  hashWithSalt _salt DeleteBroker' {..} =
    _salt `Prelude.hashWithSalt` brokerId

instance Prelude.NFData DeleteBroker where
  rnf DeleteBroker' {..} = Prelude.rnf brokerId

instance Data.ToHeaders DeleteBroker where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteBroker where
  toPath DeleteBroker' {..} =
    Prelude.mconcat
      ["/v1/brokers/", Data.toBS brokerId]

instance Data.ToQuery DeleteBroker where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteBrokerResponse' smart constructor.
data DeleteBrokerResponse = DeleteBrokerResponse'
  { -- | The unique ID that Amazon MQ generates for the broker.
    brokerId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DeleteBrokerResponse
newDeleteBrokerResponse pHttpStatus_ =
  DeleteBrokerResponse'
    { brokerId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The unique ID that Amazon MQ generates for the broker.
deleteBrokerResponse_brokerId :: Lens.Lens' DeleteBrokerResponse (Prelude.Maybe Prelude.Text)
deleteBrokerResponse_brokerId = Lens.lens (\DeleteBrokerResponse' {brokerId} -> brokerId) (\s@DeleteBrokerResponse' {} a -> s {brokerId = a} :: DeleteBrokerResponse)

-- | The response's http status code.
deleteBrokerResponse_httpStatus :: Lens.Lens' DeleteBrokerResponse Prelude.Int
deleteBrokerResponse_httpStatus = Lens.lens (\DeleteBrokerResponse' {httpStatus} -> httpStatus) (\s@DeleteBrokerResponse' {} a -> s {httpStatus = a} :: DeleteBrokerResponse)

instance Prelude.NFData DeleteBrokerResponse where
  rnf DeleteBrokerResponse' {..} =
    Prelude.rnf brokerId
      `Prelude.seq` Prelude.rnf httpStatus
