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
-- Module      : Amazonka.SSMIncidents.DeleteIncidentRecord
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Delete an incident record from Incident Manager.
module Amazonka.SSMIncidents.DeleteIncidentRecord
  ( -- * Creating a Request
    DeleteIncidentRecord (..),
    newDeleteIncidentRecord,

    -- * Request Lenses
    deleteIncidentRecord_arn,

    -- * Destructuring the Response
    DeleteIncidentRecordResponse (..),
    newDeleteIncidentRecordResponse,

    -- * Response Lenses
    deleteIncidentRecordResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newDeleteIncidentRecord' smart constructor.
data DeleteIncidentRecord = DeleteIncidentRecord'
  { -- | The Amazon Resource Name (ARN) of the incident record you are deleting.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIncidentRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'deleteIncidentRecord_arn' - The Amazon Resource Name (ARN) of the incident record you are deleting.
newDeleteIncidentRecord ::
  -- | 'arn'
  Prelude.Text ->
  DeleteIncidentRecord
newDeleteIncidentRecord pArn_ =
  DeleteIncidentRecord' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the incident record you are deleting.
deleteIncidentRecord_arn :: Lens.Lens' DeleteIncidentRecord Prelude.Text
deleteIncidentRecord_arn = Lens.lens (\DeleteIncidentRecord' {arn} -> arn) (\s@DeleteIncidentRecord' {} a -> s {arn = a} :: DeleteIncidentRecord)

instance Core.AWSRequest DeleteIncidentRecord where
  type
    AWSResponse DeleteIncidentRecord =
      DeleteIncidentRecordResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteIncidentRecordResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteIncidentRecord where
  hashWithSalt _salt DeleteIncidentRecord' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData DeleteIncidentRecord where
  rnf DeleteIncidentRecord' {..} = Prelude.rnf arn

instance Data.ToHeaders DeleteIncidentRecord where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteIncidentRecord where
  toJSON DeleteIncidentRecord' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath DeleteIncidentRecord where
  toPath = Prelude.const "/deleteIncidentRecord"

instance Data.ToQuery DeleteIncidentRecord where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteIncidentRecordResponse' smart constructor.
data DeleteIncidentRecordResponse = DeleteIncidentRecordResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteIncidentRecordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteIncidentRecordResponse_httpStatus' - The response's http status code.
newDeleteIncidentRecordResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteIncidentRecordResponse
newDeleteIncidentRecordResponse pHttpStatus_ =
  DeleteIncidentRecordResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteIncidentRecordResponse_httpStatus :: Lens.Lens' DeleteIncidentRecordResponse Prelude.Int
deleteIncidentRecordResponse_httpStatus = Lens.lens (\DeleteIncidentRecordResponse' {httpStatus} -> httpStatus) (\s@DeleteIncidentRecordResponse' {} a -> s {httpStatus = a} :: DeleteIncidentRecordResponse)

instance Prelude.NFData DeleteIncidentRecordResponse where
  rnf DeleteIncidentRecordResponse' {..} =
    Prelude.rnf httpStatus
