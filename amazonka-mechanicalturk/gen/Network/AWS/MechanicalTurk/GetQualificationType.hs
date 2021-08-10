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
-- Module      : Network.AWS.MechanicalTurk.GetQualificationType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetQualificationType@operation retrieves information about a
-- Qualification type using its ID.
module Network.AWS.MechanicalTurk.GetQualificationType
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetQualificationTypeResponse'
            Prelude.<$> (x Core..?> "QualificationType")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetQualificationType

instance Prelude.NFData GetQualificationType

instance Core.ToHeaders GetQualificationType where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.GetQualificationType" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetQualificationType where
  toJSON GetQualificationType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("QualificationTypeId" Core..= qualificationTypeId)
          ]
      )

instance Core.ToPath GetQualificationType where
  toPath = Prelude.const "/"

instance Core.ToQuery GetQualificationType where
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

instance Prelude.NFData GetQualificationTypeResponse
