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
-- Module      : Amazonka.Connect.DeleteHoursOfOperation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Deletes an hours of operation.
module Amazonka.Connect.DeleteHoursOfOperation
  ( -- * Creating a Request
    DeleteHoursOfOperation (..),
    newDeleteHoursOfOperation,

    -- * Request Lenses
    deleteHoursOfOperation_instanceId,
    deleteHoursOfOperation_hoursOfOperationId,

    -- * Destructuring the Response
    DeleteHoursOfOperationResponse (..),
    newDeleteHoursOfOperationResponse,
  )
where

import Amazonka.Connect.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteHoursOfOperation' smart constructor.
data DeleteHoursOfOperation = DeleteHoursOfOperation'
  { -- | The identifier of the Amazon Connect instance. You can find the
    -- instanceId in the ARN of the instance.
    instanceId :: Prelude.Text,
    -- | The identifier for the hours of operation.
    hoursOfOperationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHoursOfOperation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceId', 'deleteHoursOfOperation_instanceId' - The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
--
-- 'hoursOfOperationId', 'deleteHoursOfOperation_hoursOfOperationId' - The identifier for the hours of operation.
newDeleteHoursOfOperation ::
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'hoursOfOperationId'
  Prelude.Text ->
  DeleteHoursOfOperation
newDeleteHoursOfOperation
  pInstanceId_
  pHoursOfOperationId_ =
    DeleteHoursOfOperation'
      { instanceId = pInstanceId_,
        hoursOfOperationId = pHoursOfOperationId_
      }

-- | The identifier of the Amazon Connect instance. You can find the
-- instanceId in the ARN of the instance.
deleteHoursOfOperation_instanceId :: Lens.Lens' DeleteHoursOfOperation Prelude.Text
deleteHoursOfOperation_instanceId = Lens.lens (\DeleteHoursOfOperation' {instanceId} -> instanceId) (\s@DeleteHoursOfOperation' {} a -> s {instanceId = a} :: DeleteHoursOfOperation)

-- | The identifier for the hours of operation.
deleteHoursOfOperation_hoursOfOperationId :: Lens.Lens' DeleteHoursOfOperation Prelude.Text
deleteHoursOfOperation_hoursOfOperationId = Lens.lens (\DeleteHoursOfOperation' {hoursOfOperationId} -> hoursOfOperationId) (\s@DeleteHoursOfOperation' {} a -> s {hoursOfOperationId = a} :: DeleteHoursOfOperation)

instance Core.AWSRequest DeleteHoursOfOperation where
  type
    AWSResponse DeleteHoursOfOperation =
      DeleteHoursOfOperationResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull
      DeleteHoursOfOperationResponse'

instance Prelude.Hashable DeleteHoursOfOperation where
  hashWithSalt _salt DeleteHoursOfOperation' {..} =
    _salt
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` hoursOfOperationId

instance Prelude.NFData DeleteHoursOfOperation where
  rnf DeleteHoursOfOperation' {..} =
    Prelude.rnf instanceId `Prelude.seq`
      Prelude.rnf hoursOfOperationId

instance Data.ToHeaders DeleteHoursOfOperation where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DeleteHoursOfOperation where
  toPath DeleteHoursOfOperation' {..} =
    Prelude.mconcat
      [ "/hours-of-operations/",
        Data.toBS instanceId,
        "/",
        Data.toBS hoursOfOperationId
      ]

instance Data.ToQuery DeleteHoursOfOperation where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteHoursOfOperationResponse' smart constructor.
data DeleteHoursOfOperationResponse = DeleteHoursOfOperationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteHoursOfOperationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteHoursOfOperationResponse ::
  DeleteHoursOfOperationResponse
newDeleteHoursOfOperationResponse =
  DeleteHoursOfOperationResponse'

instance
  Prelude.NFData
    DeleteHoursOfOperationResponse
  where
  rnf _ = ()
