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
-- Module      : Amazonka.MechanicalTurk.GetQualificationType
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetQualificationType@operation retrieves information about a
-- Qualification type using its ID.
module Amazonka.MechanicalTurk.GetQualificationType
  ( -- * Creating a Request
    GetQualificationType (..),
    newGetQualificationType,

    -- * Request Lenses
    getQualificationType_qualificationTypeId,

    -- * Destructuring the Response
    GetQualificationTypeResponse (..),
    newGetQualificationTypeResponse,

    -- * Response Lenses
    getQualificationTypeResponse_qualificationType,
    getQualificationTypeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MechanicalTurk.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetQualificationType' smart constructor.
data GetQualificationType = GetQualificationType'
  { -- | The ID of the QualificationType.
    qualificationTypeId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQualificationType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualificationTypeId', 'getQualificationType_qualificationTypeId' - The ID of the QualificationType.
newGetQualificationType ::
  -- | 'qualificationTypeId'
  Prelude.Text ->
  GetQualificationType
newGetQualificationType pQualificationTypeId_ =
  GetQualificationType'
    { qualificationTypeId =
        pQualificationTypeId_
    }

-- | The ID of the QualificationType.
getQualificationType_qualificationTypeId :: Lens.Lens' GetQualificationType Prelude.Text
getQualificationType_qualificationTypeId = Lens.lens (\GetQualificationType' {qualificationTypeId} -> qualificationTypeId) (\s@GetQualificationType' {} a -> s {qualificationTypeId = a} :: GetQualificationType)

instance Core.AWSRequest GetQualificationType where
  type
    AWSResponse GetQualificationType =
      GetQualificationTypeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetQualificationTypeResponse'
            Prelude.<$> (x Data..?> "QualificationType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetQualificationType where
  hashWithSalt _salt GetQualificationType' {..} =
    _salt `Prelude.hashWithSalt` qualificationTypeId

instance Prelude.NFData GetQualificationType where
  rnf GetQualificationType' {..} =
    Prelude.rnf qualificationTypeId

instance Data.ToHeaders GetQualificationType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "MTurkRequesterServiceV20170117.GetQualificationType" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetQualificationType where
  toJSON GetQualificationType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("QualificationTypeId" Data..= qualificationTypeId)
          ]
      )

instance Data.ToPath GetQualificationType where
  toPath = Prelude.const "/"

instance Data.ToQuery GetQualificationType where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetQualificationTypeResponse' smart constructor.
data GetQualificationTypeResponse = GetQualificationTypeResponse'
  { -- | The returned Qualification Type
    qualificationType :: Prelude.Maybe QualificationType,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetQualificationTypeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'qualificationType', 'getQualificationTypeResponse_qualificationType' - The returned Qualification Type
--
-- 'httpStatus', 'getQualificationTypeResponse_httpStatus' - The response's http status code.
newGetQualificationTypeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetQualificationTypeResponse
newGetQualificationTypeResponse pHttpStatus_ =
  GetQualificationTypeResponse'
    { qualificationType =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The returned Qualification Type
getQualificationTypeResponse_qualificationType :: Lens.Lens' GetQualificationTypeResponse (Prelude.Maybe QualificationType)
getQualificationTypeResponse_qualificationType = Lens.lens (\GetQualificationTypeResponse' {qualificationType} -> qualificationType) (\s@GetQualificationTypeResponse' {} a -> s {qualificationType = a} :: GetQualificationTypeResponse)

-- | The response's http status code.
getQualificationTypeResponse_httpStatus :: Lens.Lens' GetQualificationTypeResponse Prelude.Int
getQualificationTypeResponse_httpStatus = Lens.lens (\GetQualificationTypeResponse' {httpStatus} -> httpStatus) (\s@GetQualificationTypeResponse' {} a -> s {httpStatus = a} :: GetQualificationTypeResponse)

instance Prelude.NFData GetQualificationTypeResponse where
  rnf GetQualificationTypeResponse' {..} =
    Prelude.rnf qualificationType `Prelude.seq`
      Prelude.rnf httpStatus
