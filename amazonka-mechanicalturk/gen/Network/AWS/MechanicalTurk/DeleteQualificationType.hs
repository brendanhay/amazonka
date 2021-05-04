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
-- Module      : Network.AWS.MechanicalTurk.DeleteQualificationType
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.MechanicalTurk.DeleteQualificationType
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

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteQualificationType' smart constructor.
data DeleteQualificationType = DeleteQualificationType'
  { -- | The ID of the QualificationType to dispose.
    qualificationTypeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest DeleteQualificationType where
  type
    Rs DeleteQualificationType =
      DeleteQualificationTypeResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteQualificationTypeResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteQualificationType

instance Prelude.NFData DeleteQualificationType

instance Prelude.ToHeaders DeleteQualificationType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MTurkRequesterServiceV20170117.DeleteQualificationType" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteQualificationType where
  toJSON DeleteQualificationType' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "QualificationTypeId"
                  Prelude..= qualificationTypeId
              )
          ]
      )

instance Prelude.ToPath DeleteQualificationType where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteQualificationType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDeleteQualificationTypeResponse' smart constructor.
data DeleteQualificationTypeResponse = DeleteQualificationTypeResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
