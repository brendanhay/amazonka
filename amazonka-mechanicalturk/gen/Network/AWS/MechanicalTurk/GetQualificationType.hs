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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetQualificationType' smart constructor.
data GetQualificationType = GetQualificationType'
  { -- | The ID of the QualificationType.
    qualificationTypeId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  GetQualificationType
newGetQualificationType pQualificationTypeId_ =
  GetQualificationType'
    { qualificationTypeId =
        pQualificationTypeId_
    }

-- | The ID of the QualificationType.
getQualificationType_qualificationTypeId :: Lens.Lens' GetQualificationType Core.Text
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
            Core.<$> (x Core..?> "QualificationType")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetQualificationType

instance Core.NFData GetQualificationType

instance Core.ToHeaders GetQualificationType where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "MTurkRequesterServiceV20170117.GetQualificationType" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetQualificationType where
  toJSON GetQualificationType' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just
              ("QualificationTypeId" Core..= qualificationTypeId)
          ]
      )

instance Core.ToPath GetQualificationType where
  toPath = Core.const "/"

instance Core.ToQuery GetQualificationType where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetQualificationTypeResponse' smart constructor.
data GetQualificationTypeResponse = GetQualificationTypeResponse'
  { -- | The returned Qualification Type
    qualificationType :: Core.Maybe QualificationType,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  GetQualificationTypeResponse
newGetQualificationTypeResponse pHttpStatus_ =
  GetQualificationTypeResponse'
    { qualificationType =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The returned Qualification Type
getQualificationTypeResponse_qualificationType :: Lens.Lens' GetQualificationTypeResponse (Core.Maybe QualificationType)
getQualificationTypeResponse_qualificationType = Lens.lens (\GetQualificationTypeResponse' {qualificationType} -> qualificationType) (\s@GetQualificationTypeResponse' {} a -> s {qualificationType = a} :: GetQualificationTypeResponse)

-- | The response's http status code.
getQualificationTypeResponse_httpStatus :: Lens.Lens' GetQualificationTypeResponse Core.Int
getQualificationTypeResponse_httpStatus = Lens.lens (\GetQualificationTypeResponse' {httpStatus} -> httpStatus) (\s@GetQualificationTypeResponse' {} a -> s {httpStatus = a} :: GetQualificationTypeResponse)

instance Core.NFData GetQualificationTypeResponse
