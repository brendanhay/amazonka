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
-- Module      : Amazonka.FraudDetector.DeleteEventType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes an event type.
--
-- You cannot delete an event type that is used in a detector or a model.
--
-- When you delete an event type, Amazon Fraud Detector permanently deletes
-- that event type and the data is no longer stored in Amazon Fraud
-- Detector.
module Amazonka.FraudDetector.DeleteEventType
  ( -- * Creating a Request
    DeleteEventType (..),
    newDeleteEventType,

    -- * Request Lenses
    deleteEventType_name,

    -- * Destructuring the Response
    DeleteEventTypeResponse (..),
    newDeleteEventTypeResponse,

    -- * Response Lenses
    deleteEventTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteEventType' smart constructor.
data DeleteEventType = DeleteEventType'
  { -- | The name of the event type to delete.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'deleteEventType_name' - The name of the event type to delete.
newDeleteEventType ::
  -- | 'name'
  Prelude.Text ->
  DeleteEventType
newDeleteEventType pName_ =
  DeleteEventType' {name = pName_}

-- | The name of the event type to delete.
deleteEventType_name :: Lens.Lens' DeleteEventType Prelude.Text
deleteEventType_name = Lens.lens (\DeleteEventType' {name} -> name) (\s@DeleteEventType' {} a -> s {name = a} :: DeleteEventType)

instance Core.AWSRequest DeleteEventType where
  type
    AWSResponse DeleteEventType =
      DeleteEventTypeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteEventTypeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteEventType where
  hashWithSalt _salt DeleteEventType' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DeleteEventType where
  rnf DeleteEventType' {..} = Prelude.rnf name

instance Data.ToHeaders DeleteEventType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.DeleteEventType" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteEventType where
  toJSON DeleteEventType' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("name" Data..= name)]
      )

instance Data.ToPath DeleteEventType where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteEventType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteEventTypeResponse' smart constructor.
data DeleteEventTypeResponse = DeleteEventTypeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteEventTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteEventTypeResponse_httpStatus' - The response's http status code.
newDeleteEventTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteEventTypeResponse
newDeleteEventTypeResponse pHttpStatus_ =
  DeleteEventTypeResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
deleteEventTypeResponse_httpStatus :: Lens.Lens' DeleteEventTypeResponse Prelude.Int
deleteEventTypeResponse_httpStatus = Lens.lens (\DeleteEventTypeResponse' {httpStatus} -> httpStatus) (\s@DeleteEventTypeResponse' {} a -> s {httpStatus = a} :: DeleteEventTypeResponse)

instance Prelude.NFData DeleteEventTypeResponse where
  rnf DeleteEventTypeResponse' {..} =
    Prelude.rnf httpStatus
