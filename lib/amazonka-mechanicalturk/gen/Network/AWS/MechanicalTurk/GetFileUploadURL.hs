{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetFileUploadURL (..)
    , mkGetFileUploadURL
    -- ** Request lenses
    , gfuurlAssignmentId
    , gfuurlQuestionIdentifier

    -- * Destructuring the response
    , GetFileUploadURLResponse (..)
    , mkGetFileUploadURLResponse
    -- ** Response lenses
    , gfuurlrrsFileUploadURL
    , gfuurlrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MechanicalTurk.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetFileUploadURL' smart constructor.
data GetFileUploadURL = GetFileUploadURL'
  { assignmentId :: Types.AssignmentId
    -- ^ The ID of the assignment that contains the question with a FileUploadAnswer.
  , questionIdentifier :: Core.Text
    -- ^ The identifier of the question with a FileUploadAnswer, as specified in the QuestionForm of the HIT.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFileUploadURL' value with any optional fields omitted.
mkGetFileUploadURL
    :: Types.AssignmentId -- ^ 'assignmentId'
    -> Core.Text -- ^ 'questionIdentifier'
    -> GetFileUploadURL
mkGetFileUploadURL assignmentId questionIdentifier
  = GetFileUploadURL'{assignmentId, questionIdentifier}

-- | The ID of the assignment that contains the question with a FileUploadAnswer.
--
-- /Note:/ Consider using 'assignmentId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfuurlAssignmentId :: Lens.Lens' GetFileUploadURL Types.AssignmentId
gfuurlAssignmentId = Lens.field @"assignmentId"
{-# INLINEABLE gfuurlAssignmentId #-}
{-# DEPRECATED assignmentId "Use generic-lens or generic-optics with 'assignmentId' instead"  #-}

-- | The identifier of the question with a FileUploadAnswer, as specified in the QuestionForm of the HIT.
--
-- /Note:/ Consider using 'questionIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfuurlQuestionIdentifier :: Lens.Lens' GetFileUploadURL Core.Text
gfuurlQuestionIdentifier = Lens.field @"questionIdentifier"
{-# INLINEABLE gfuurlQuestionIdentifier #-}
{-# DEPRECATED questionIdentifier "Use generic-lens or generic-optics with 'questionIdentifier' instead"  #-}

instance Core.ToQuery GetFileUploadURL where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetFileUploadURL where
        toHeaders GetFileUploadURL{..}
          = Core.pure
              ("X-Amz-Target", "MTurkRequesterServiceV20170117.GetFileUploadURL")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetFileUploadURL where
        toJSON GetFileUploadURL{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("AssignmentId" Core..= assignmentId),
                  Core.Just ("QuestionIdentifier" Core..= questionIdentifier)])

instance Core.AWSRequest GetFileUploadURL where
        type Rs GetFileUploadURL = GetFileUploadURLResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetFileUploadURLResponse' Core.<$>
                   (x Core..:? "FileUploadURL") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetFileUploadURLResponse' smart constructor.
data GetFileUploadURLResponse = GetFileUploadURLResponse'
  { fileUploadURL :: Core.Maybe Core.Text
    -- ^ A temporary URL for the file that the Worker uploaded for the answer. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetFileUploadURLResponse' value with any optional fields omitted.
mkGetFileUploadURLResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetFileUploadURLResponse
mkGetFileUploadURLResponse responseStatus
  = GetFileUploadURLResponse'{fileUploadURL = Core.Nothing,
                              responseStatus}

-- | A temporary URL for the file that the Worker uploaded for the answer. 
--
-- /Note:/ Consider using 'fileUploadURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfuurlrrsFileUploadURL :: Lens.Lens' GetFileUploadURLResponse (Core.Maybe Core.Text)
gfuurlrrsFileUploadURL = Lens.field @"fileUploadURL"
{-# INLINEABLE gfuurlrrsFileUploadURL #-}
{-# DEPRECATED fileUploadURL "Use generic-lens or generic-optics with 'fileUploadURL' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gfuurlrrsResponseStatus :: Lens.Lens' GetFileUploadURLResponse Core.Int
gfuurlrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gfuurlrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
