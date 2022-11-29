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
-- Module      : Amazonka.RolesAnywhere.GetSubject
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a Subject. A Subject associates a certificate identity with
-- authentication attempts by CreateSession. The Subject resources stores
-- audit information such as status of the last authentication attempt, the
-- certificate data used in the attempt, and the last time the associated
-- identity attempted authentication.
--
-- __Required permissions:__ @rolesanywhere:GetSubject@.
module Amazonka.RolesAnywhere.GetSubject
  ( -- * Creating a Request
    GetSubject (..),
    newGetSubject,

    -- * Request Lenses
    getSubject_subjectId,

    -- * Destructuring the Response
    GetSubjectResponse (..),
    newGetSubjectResponse,

    -- * Response Lenses
    getSubjectResponse_subject,
    getSubjectResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.RolesAnywhere.Types

-- | /See:/ 'newGetSubject' smart constructor.
data GetSubject = GetSubject'
  { -- | The unique identifier of the subject.
    subjectId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSubject' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subjectId', 'getSubject_subjectId' - The unique identifier of the subject.
newGetSubject ::
  -- | 'subjectId'
  Prelude.Text ->
  GetSubject
newGetSubject pSubjectId_ =
  GetSubject' {subjectId = pSubjectId_}

-- | The unique identifier of the subject.
getSubject_subjectId :: Lens.Lens' GetSubject Prelude.Text
getSubject_subjectId = Lens.lens (\GetSubject' {subjectId} -> subjectId) (\s@GetSubject' {} a -> s {subjectId = a} :: GetSubject)

instance Core.AWSRequest GetSubject where
  type AWSResponse GetSubject = GetSubjectResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSubjectResponse'
            Prelude.<$> (x Core..?> "subject")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetSubject where
  hashWithSalt _salt GetSubject' {..} =
    _salt `Prelude.hashWithSalt` subjectId

instance Prelude.NFData GetSubject where
  rnf GetSubject' {..} = Prelude.rnf subjectId

instance Core.ToHeaders GetSubject where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetSubject where
  toPath GetSubject' {..} =
    Prelude.mconcat ["/subject/", Core.toBS subjectId]

instance Core.ToQuery GetSubject where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSubjectResponse' smart constructor.
data GetSubjectResponse = GetSubjectResponse'
  { -- | The state of the subject after a read or write operation.
    subject :: Prelude.Maybe SubjectDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSubjectResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'subject', 'getSubjectResponse_subject' - The state of the subject after a read or write operation.
--
-- 'httpStatus', 'getSubjectResponse_httpStatus' - The response's http status code.
newGetSubjectResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetSubjectResponse
newGetSubjectResponse pHttpStatus_ =
  GetSubjectResponse'
    { subject = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The state of the subject after a read or write operation.
getSubjectResponse_subject :: Lens.Lens' GetSubjectResponse (Prelude.Maybe SubjectDetail)
getSubjectResponse_subject = Lens.lens (\GetSubjectResponse' {subject} -> subject) (\s@GetSubjectResponse' {} a -> s {subject = a} :: GetSubjectResponse)

-- | The response's http status code.
getSubjectResponse_httpStatus :: Lens.Lens' GetSubjectResponse Prelude.Int
getSubjectResponse_httpStatus = Lens.lens (\GetSubjectResponse' {httpStatus} -> httpStatus) (\s@GetSubjectResponse' {} a -> s {httpStatus = a} :: GetSubjectResponse)

instance Prelude.NFData GetSubjectResponse where
  rnf GetSubjectResponse' {..} =
    Prelude.rnf subject
      `Prelude.seq` Prelude.rnf httpStatus
