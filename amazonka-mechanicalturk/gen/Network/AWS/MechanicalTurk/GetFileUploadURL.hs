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
-- Module      : Network.AWS.MechanicalTurk.GetFileUploadURL
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetFileUploadURL@ operation generates and returns a temporary URL.
-- You use the temporary URL to retrieve a file uploaded by a Worker as an
-- answer to a FileUploadAnswer question for a HIT. The temporary URL is
-- generated the instant the GetFileUploadURL operation is called, and is
-- valid for 60 seconds. You can get a temporary file upload URL any time
-- until the HIT is disposed. After the HIT is disposed, any uploaded files
-- are deleted, and cannot be retrieved. Pending Deprecation on December
-- 12, 2017. The Answer Specification structure will no longer support the
-- @FileUploadAnswer@ element to be used for the QuestionForm data
-- structure. Instead, we recommend that Requesters who want to create HITs
-- asking Workers to upload files to use Amazon S3.
module Network.AWS.MechanicalTurk.GetFileUploadURL
  ( -- * Creating a Request
    GetFileUploadURL (..),
    newGetFileUploadURL,

    -- * Request Lenses
    getFileUploadURL_assignmentId,
    getFileUploadURL_questionIdentifier,

    -- * Destructuring the Response
    GetFileUploadURLResponse (..),
    newGetFileUploadURLResponse,

    -- * Response Lenses
    getFileUploadURLResponse_fileUploadURL,
    getFileUploadURLResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetFileUploadURL' smart constructor.
data GetFileUploadURL = GetFileUploadURL'
  { -- | The ID of the assignment that contains the question with a
    -- FileUploadAnswer.
    assignmentId :: Prelude.Text,
    -- | The identifier of the question with a FileUploadAnswer, as specified in
    -- the QuestionForm of the HIT.
    questionIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetFileUploadURL' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignmentId', 'getFileUploadURL_assignmentId' - The ID of the assignment that contains the question with a
-- FileUploadAnswer.
--
-- 'questionIdentifier', 'getFileUploadURL_questionIdentifier' - The identifier of the question with a FileUploadAnswer, as specified in
-- the QuestionForm of the HIT.
newGetFileUploadURL ::
  -- | 'assignmentId'
  Prelude.Text ->
  -- | 'questionIdentifier'
  Prelude.Text ->
  GetFileUploadURL
newGetFileUploadURL
  pAssignmentId_
  pQuestionIdentifier_ =
    GetFileUploadURL'
      { assignmentId = pAssignmentId_,
        questionIdentifier = pQuestionIdentifier_
      }

-- | The ID of the assignment that contains the question with a
-- FileUploadAnswer.
getFileUploadURL_assignmentId :: Lens.Lens' GetFileUploadURL Prelude.Text
getFileUploadURL_assignmentId = Lens.lens (\GetFileUploadURL' {assignmentId} -> assignmentId) (\s@GetFileUploadURL' {} a -> s {assignmentId = a} :: GetFileUploadURL)

-- | The identifier of the question with a FileUploadAnswer, as specified in
-- the QuestionForm of the HIT.
getFileUploadURL_questionIdentifier :: Lens.Lens' GetFileUploadURL Prelude.Text
getFileUploadURL_questionIdentifier = Lens.lens (\GetFileUploadURL' {questionIdentifier} -> questionIdentifier) (\s@GetFileUploadURL' {} a -> s {questionIdentifier = a} :: GetFileUploadURL)

instance Prelude.AWSRequest GetFileUploadURL where
  type Rs GetFileUploadURL = GetFileUploadURLResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFileUploadURLResponse'
            Prelude.<$> (x Prelude..?> "FileUploadURL")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetFileUploadURL

instance Prelude.NFData GetFileUploadURL

instance Prelude.ToHeaders GetFileUploadURL where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "MTurkRequesterServiceV20170117.GetFileUploadURL" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetFileUploadURL where
  toJSON GetFileUploadURL' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("AssignmentId" Prelude..= assignmentId),
            Prelude.Just
              ( "QuestionIdentifier"
                  Prelude..= questionIdentifier
              )
          ]
      )

instance Prelude.ToPath GetFileUploadURL where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetFileUploadURL where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetFileUploadURLResponse' smart constructor.
data GetFileUploadURLResponse = GetFileUploadURLResponse'
  { -- | A temporary URL for the file that the Worker uploaded for the answer.
    fileUploadURL :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetFileUploadURLResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fileUploadURL', 'getFileUploadURLResponse_fileUploadURL' - A temporary URL for the file that the Worker uploaded for the answer.
--
-- 'httpStatus', 'getFileUploadURLResponse_httpStatus' - The response's http status code.
newGetFileUploadURLResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetFileUploadURLResponse
newGetFileUploadURLResponse pHttpStatus_ =
  GetFileUploadURLResponse'
    { fileUploadURL =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A temporary URL for the file that the Worker uploaded for the answer.
getFileUploadURLResponse_fileUploadURL :: Lens.Lens' GetFileUploadURLResponse (Prelude.Maybe Prelude.Text)
getFileUploadURLResponse_fileUploadURL = Lens.lens (\GetFileUploadURLResponse' {fileUploadURL} -> fileUploadURL) (\s@GetFileUploadURLResponse' {} a -> s {fileUploadURL = a} :: GetFileUploadURLResponse)

-- | The response's http status code.
getFileUploadURLResponse_httpStatus :: Lens.Lens' GetFileUploadURLResponse Prelude.Int
getFileUploadURLResponse_httpStatus = Lens.lens (\GetFileUploadURLResponse' {httpStatus} -> httpStatus) (\s@GetFileUploadURLResponse' {} a -> s {httpStatus = a} :: GetFileUploadURLResponse)

instance Prelude.NFData GetFileUploadURLResponse
