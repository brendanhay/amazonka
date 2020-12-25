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
    gfuurlAssignmentId,
    gfuurlQuestionIdentifier,

    -- * Destructuring the response
    GetFileUploadURLResponse (..),
    mkGetFileUploadURLResponse,

    -- ** Response lenses
    gfuurlrrsFileUploadURL,
    gfuurlrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFileUploadURL' smart constructor.
data GetFileUploadURL = GetFileUploadURL'
  { -- | The ID of the assignment that contains the question with a FileUploadAnswer.
    assignmentId :: Types.AssignmentId,
    -- | The identifier of the question with a FileUploadAnswer, as specified in the QuestionForm of the HIT.
    questionIdentifier :: Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFileUploadURL' value with any optional fields omitted.
mkGetFileUploadURL ::
  -- | 'assignmentId'
  Types.AssignmentId ->
  -- | 'questionIdentifier'
  Types.String ->
  GetFileUploadURL
mkGetFileUploadURL assignmentId questionIdentifier =
  GetFileUploadURL' {assignmentId, questionIdentifier}

-- | The ID of the assignment that contains the question with a FileUploadAnswer.
--
-- /Note:/ Consider using 'assignmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfuurlAssignmentId :: Lens.Lens' GetFileUploadURL Types.AssignmentId
gfuurlAssignmentId = Lens.field @"assignmentId"
{-# DEPRECATED gfuurlAssignmentId "Use generic-lens or generic-optics with 'assignmentId' instead." #-}

-- | The identifier of the question with a FileUploadAnswer, as specified in the QuestionForm of the HIT.
--
-- /Note:/ Consider using 'questionIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfuurlQuestionIdentifier :: Lens.Lens' GetFileUploadURL Types.String
gfuurlQuestionIdentifier = Lens.field @"questionIdentifier"
{-# DEPRECATED gfuurlQuestionIdentifier "Use generic-lens or generic-optics with 'questionIdentifier' instead." #-}

instance Core.FromJSON GetFileUploadURL where
  toJSON GetFileUploadURL {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("AssignmentId" Core..= assignmentId),
            Core.Just ("QuestionIdentifier" Core..= questionIdentifier)
          ]
      )

instance Core.AWSRequest GetFileUploadURL where
  type Rs GetFileUploadURL = GetFileUploadURLResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "MTurkRequesterServiceV20170117.GetFileUploadURL")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetFileUploadURLResponse'
            Core.<$> (x Core..:? "FileUploadURL")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetFileUploadURLResponse' smart constructor.
data GetFileUploadURLResponse = GetFileUploadURLResponse'
  { -- | A temporary URL for the file that the Worker uploaded for the answer.
    fileUploadURL :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFileUploadURLResponse' value with any optional fields omitted.
mkGetFileUploadURLResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetFileUploadURLResponse
mkGetFileUploadURLResponse responseStatus =
  GetFileUploadURLResponse'
    { fileUploadURL = Core.Nothing,
      responseStatus
    }

-- | A temporary URL for the file that the Worker uploaded for the answer.
--
-- /Note:/ Consider using 'fileUploadURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfuurlrrsFileUploadURL :: Lens.Lens' GetFileUploadURLResponse (Core.Maybe Types.String)
gfuurlrrsFileUploadURL = Lens.field @"fileUploadURL"
{-# DEPRECATED gfuurlrrsFileUploadURL "Use generic-lens or generic-optics with 'fileUploadURL' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfuurlrrsResponseStatus :: Lens.Lens' GetFileUploadURLResponse Core.Int
gfuurlrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gfuurlrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
