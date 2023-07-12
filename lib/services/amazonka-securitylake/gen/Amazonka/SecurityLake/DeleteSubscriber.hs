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
-- Module      : Amazonka.SecurityLake.DeleteSubscriber
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the subscription permission for accounts that are already
-- enabled in Amazon Security Lake. You can delete a subscriber and remove
-- access to data in the current Amazon Web Services Region.
module Amazonka.SecurityLake.DeleteSubscriber
  ( -- * Creating a Request
    DeleteSubscriber (..),
    newDeleteSubscriber,

    -- * Request Lenses
    deleteSubscriber_id,

    -- * Destructuring the Response
    DeleteSubscriberResponse (..),
    newDeleteSubscriberResponse,

    -- * Response Lenses
    deleteSubscriberResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityLake.Types

-- | /See:/ 'newDeleteSubscriber' smart constructor.
data DeleteSubscriber = DeleteSubscriber'
  { -- | A value created by Security Lake that uniquely identifies your
    -- @DeleteSubscriber@ API request.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSubscriber' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'deleteSubscriber_id' - A value created by Security Lake that uniquely identifies your
-- @DeleteSubscriber@ API request.
newDeleteSubscriber ::
  -- | 'id'
  Prelude.Text ->
  DeleteSubscriber
newDeleteSubscriber pId_ =
  DeleteSubscriber' {id = pId_}

-- | A value created by Security Lake that uniquely identifies your
-- @DeleteSubscriber@ API request.
deleteSubscriber_id :: Lens.Lens' DeleteSubscriber Prelude.Text
deleteSubscriber_id = Lens.lens (\DeleteSubscriber' {id} -> id) (\s@DeleteSubscriber' {} a -> s {id = a} :: DeleteSubscriber)

instance Core.AWSRequest DeleteSubscriber where
  type
    AWSResponse DeleteSubscriber =
      DeleteSubscriberResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteSubscriberResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteSubscriber where
  hashWithSalt _salt DeleteSubscriber' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData DeleteSubscriber where
  rnf DeleteSubscriber' {..} = Prelude.rnf id

instance Data.ToHeaders DeleteSubscriber where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteSubscriber where
  toPath = Prelude.const "/v1/subscribers"

instance Data.ToQuery DeleteSubscriber where
  toQuery DeleteSubscriber' {..} =
    Prelude.mconcat ["id" Data.=: id]

-- | /See:/ 'newDeleteSubscriberResponse' smart constructor.
data DeleteSubscriberResponse = DeleteSubscriberResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteSubscriberResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteSubscriberResponse_httpStatus' - The response's http status code.
newDeleteSubscriberResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteSubscriberResponse
newDeleteSubscriberResponse pHttpStatus_ =
  DeleteSubscriberResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteSubscriberResponse_httpStatus :: Lens.Lens' DeleteSubscriberResponse Prelude.Int
deleteSubscriberResponse_httpStatus = Lens.lens (\DeleteSubscriberResponse' {httpStatus} -> httpStatus) (\s@DeleteSubscriberResponse' {} a -> s {httpStatus = a} :: DeleteSubscriberResponse)

instance Prelude.NFData DeleteSubscriberResponse where
  rnf DeleteSubscriberResponse' {..} =
    Prelude.rnf httpStatus
