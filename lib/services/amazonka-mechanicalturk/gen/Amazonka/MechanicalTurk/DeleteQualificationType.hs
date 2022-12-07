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
-- Module      : Amazonka.MechanicalTurk.DeleteQualificationType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @DeleteQualificationType@ deletes a Qualification type and deletes
-- any HIT types that are associated with the Qualification type.
--
-- This operation does not revoke Qualifications already assigned to
-- Workers because the Qualifications might be needed for active HITs. If
-- there are any pending requests for the Qualification type, Amazon
-- Mechanical Turk rejects those requests. After you delete a Qualification
-- type, you can no longer use it to create HITs or HIT types.
--
-- DeleteQualificationType must wait for all the HITs that use the deleted
-- Qualification type to be deleted before completing. It may take up to 48
-- hours before DeleteQualificationType completes and the unique name of
-- the Qualification type is available for reuse with
-- CreateQualificationType.
module Amazonka.MechanicalTurk.DeleteQualificationType
  ( -- * Creating a Request
    DeleteQualificationType (..),
    newDeleteQualificationType,

    -- * Request Lenses
    deleteQualificationType_qualificationTypeId,

    -- * Destructuring the Response
    DeleteQualificationTypeResponse (..),
    newDeleteQualificationTypeResponse,

    -- * Response Lenses
    deleteQualificationTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteQualificationType' smart constructor.
data DeleteQualificationType = DeleteQualificationType'
  { -- | The ID of the QualificationType to dispose.
    qualificationTypeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteQualificationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualificationTypeId', 'deleteQualificationType_qualificationTypeId' - The ID of the QualificationType to dispose.
newDeleteQualificationType ::
  -- | 'qualificationTypeId'
  Prelude.Text ->
  DeleteQualificationType
newDeleteQualificationType pQualificationTypeId_ =
  DeleteQualificationType'
    { qualificationTypeId =
        pQualificationTypeId_
    }

-- | The ID of the QualificationType to dispose.
deleteQualificationType_qualificationTypeId :: Lens.Lens' DeleteQualificationType Prelude.Text
deleteQualificationType_qualificationTypeId = Lens.lens (\DeleteQualificationType' {qualificationTypeId} -> qualificationTypeId) (\s@DeleteQualificationType' {} a -> s {qualificationTypeId = a} :: DeleteQualificationType)

instance Core.AWSRequest DeleteQualificationType where
  type
    AWSResponse DeleteQualificationType =
      DeleteQualificationTypeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteQualificationTypeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteQualificationType where
  hashWithSalt _salt DeleteQualificationType' {..} =
    _salt `Prelude.hashWithSalt` qualificationTypeId

instance Prelude.NFData DeleteQualificationType where
  rnf DeleteQualificationType' {..} =
    Prelude.rnf qualificationTypeId

instance Data.ToHeaders DeleteQualificationType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.DeleteQualificationType" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteQualificationType where
  toJSON DeleteQualificationType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("QualificationTypeId" Data..= qualificationTypeId)
          ]
      )

instance Data.ToPath DeleteQualificationType where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteQualificationType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteQualificationTypeResponse' smart constructor.
data DeleteQualificationTypeResponse = DeleteQualificationTypeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteQualificationTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteQualificationTypeResponse_httpStatus' - The response's http status code.
newDeleteQualificationTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteQualificationTypeResponse
newDeleteQualificationTypeResponse pHttpStatus_ =
  DeleteQualificationTypeResponse'
    { httpStatus =
        pHttpStatus_
    }

-- | The response's http status code.
deleteQualificationTypeResponse_httpStatus :: Lens.Lens' DeleteQualificationTypeResponse Prelude.Int
deleteQualificationTypeResponse_httpStatus = Lens.lens (\DeleteQualificationTypeResponse' {httpStatus} -> httpStatus) (\s@DeleteQualificationTypeResponse' {} a -> s {httpStatus = a} :: DeleteQualificationTypeResponse)

instance
  Prelude.NFData
    DeleteQualificationTypeResponse
  where
  rnf DeleteQualificationTypeResponse' {..} =
    Prelude.rnf httpStatus
