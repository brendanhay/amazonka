{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MechanicalTurk.GetFileUploadURL
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- The @GetFileUploadURL@ operation generates and returns a temporary URL. You use the temporary URL to retrieve a file uploaded by a Worker as an answer to a FileUploadAnswer question for a HIT. The temporary URL is generated the instant the GetFileUploadURL operation is called, and is valid for 60 seconds. You can get a temporary file upload URL any time until the HIT is disposed. After the HIT is disposed, any uploaded files are deleted, and cannot be retrieved. Pending Deprecation on December 12, 2017. The Answer Specification structure will no longer support the @FileUploadAnswer@ element to be used for the QuestionForm data structure. Instead, we recommend that Requesters who want to create HITs asking Workers to upload files to use Amazon S3.
module Network.AWS.MechanicalTurk.GetFileUploadURL
  ( -- * Creating a request
    GetFileUploadURL (..),
    mkGetFileUploadURL,

    -- ** Request lenses
    gfuuQuestionIdentifier,
    gfuuAssignmentId,

    -- * Destructuring the response
    GetFileUploadURLResponse (..),
    mkGetFileUploadURLResponse,

    -- ** Response lenses
    gfuursFileUploadURL,
    gfuursResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MechanicalTurk.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetFileUploadURL' smart constructor.
data GetFileUploadURL = GetFileUploadURL'
  { -- | The identifier of the question with a FileUploadAnswer, as specified in the QuestionForm of the HIT.
    questionIdentifier :: Lude.Text,
    -- | The ID of the assignment that contains the question with a FileUploadAnswer.
    assignmentId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFileUploadURL' with the minimum fields required to make a request.
--
-- * 'questionIdentifier' - The identifier of the question with a FileUploadAnswer, as specified in the QuestionForm of the HIT.
-- * 'assignmentId' - The ID of the assignment that contains the question with a FileUploadAnswer.
mkGetFileUploadURL ::
  -- | 'questionIdentifier'
  Lude.Text ->
  -- | 'assignmentId'
  Lude.Text ->
  GetFileUploadURL
mkGetFileUploadURL pQuestionIdentifier_ pAssignmentId_ =
  GetFileUploadURL'
    { questionIdentifier = pQuestionIdentifier_,
      assignmentId = pAssignmentId_
    }

-- | The identifier of the question with a FileUploadAnswer, as specified in the QuestionForm of the HIT.
--
-- /Note:/ Consider using 'questionIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfuuQuestionIdentifier :: Lens.Lens' GetFileUploadURL Lude.Text
gfuuQuestionIdentifier = Lens.lens (questionIdentifier :: GetFileUploadURL -> Lude.Text) (\s a -> s {questionIdentifier = a} :: GetFileUploadURL)
{-# DEPRECATED gfuuQuestionIdentifier "Use generic-lens or generic-optics with 'questionIdentifier' instead." #-}

-- | The ID of the assignment that contains the question with a FileUploadAnswer.
--
-- /Note:/ Consider using 'assignmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfuuAssignmentId :: Lens.Lens' GetFileUploadURL Lude.Text
gfuuAssignmentId = Lens.lens (assignmentId :: GetFileUploadURL -> Lude.Text) (\s a -> s {assignmentId = a} :: GetFileUploadURL)
{-# DEPRECATED gfuuAssignmentId "Use generic-lens or generic-optics with 'assignmentId' instead." #-}

instance Lude.AWSRequest GetFileUploadURL where
  type Rs GetFileUploadURL = GetFileUploadURLResponse
  request = Req.postJSON mechanicalTurkService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetFileUploadURLResponse'
            Lude.<$> (x Lude..?> "FileUploadURL")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetFileUploadURL where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "MTurkRequesterServiceV20170117.GetFileUploadURL" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetFileUploadURL where
  toJSON GetFileUploadURL' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("QuestionIdentifier" Lude..= questionIdentifier),
            Lude.Just ("AssignmentId" Lude..= assignmentId)
          ]
      )

instance Lude.ToPath GetFileUploadURL where
  toPath = Lude.const "/"

instance Lude.ToQuery GetFileUploadURL where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetFileUploadURLResponse' smart constructor.
data GetFileUploadURLResponse = GetFileUploadURLResponse'
  { -- | A temporary URL for the file that the Worker uploaded for the answer.
    fileUploadURL :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetFileUploadURLResponse' with the minimum fields required to make a request.
--
-- * 'fileUploadURL' - A temporary URL for the file that the Worker uploaded for the answer.
-- * 'responseStatus' - The response status code.
mkGetFileUploadURLResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetFileUploadURLResponse
mkGetFileUploadURLResponse pResponseStatus_ =
  GetFileUploadURLResponse'
    { fileUploadURL = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A temporary URL for the file that the Worker uploaded for the answer.
--
-- /Note:/ Consider using 'fileUploadURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfuursFileUploadURL :: Lens.Lens' GetFileUploadURLResponse (Lude.Maybe Lude.Text)
gfuursFileUploadURL = Lens.lens (fileUploadURL :: GetFileUploadURLResponse -> Lude.Maybe Lude.Text) (\s a -> s {fileUploadURL = a} :: GetFileUploadURLResponse)
{-# DEPRECATED gfuursFileUploadURL "Use generic-lens or generic-optics with 'fileUploadURL' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfuursResponseStatus :: Lens.Lens' GetFileUploadURLResponse Lude.Int
gfuursResponseStatus = Lens.lens (responseStatus :: GetFileUploadURLResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetFileUploadURLResponse)
{-# DEPRECATED gfuursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
