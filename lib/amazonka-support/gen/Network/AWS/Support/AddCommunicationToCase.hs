{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.AddCommunicationToCase
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds additional customer communication to an AWS Support case. Use the @caseId@ parameter to identify the case to which to add communication. You can list a set of email addresses to copy on the communication by using the @ccEmailAddresses@ parameter. The @communicationBody@ value contains the text of the communication.
module Network.AWS.Support.AddCommunicationToCase
  ( -- * Creating a request
    AddCommunicationToCase (..),
    mkAddCommunicationToCase,

    -- ** Request lenses
    actcCommunicationBody,
    actcAttachmentSetId,
    actcCaseId,
    actcCcEmailAddresses,

    -- * Destructuring the response
    AddCommunicationToCaseResponse (..),
    mkAddCommunicationToCaseResponse,

    -- ** Response lenses
    actcrrsResult,
    actcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Support.Types as Types

-- | /See:/ 'mkAddCommunicationToCase' smart constructor.
data AddCommunicationToCase = AddCommunicationToCase'
  { -- | The body of an email communication to add to the support case.
    communicationBody :: Types.CommunicationBody,
    -- | The ID of a set of one or more attachments for the communication to add to the case. Create the set by calling 'AddAttachmentsToSet'
    attachmentSetId :: Core.Maybe Types.AttachmentSetId,
    -- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
    caseId :: Core.Maybe Types.CaseId,
    -- | The email addresses in the CC line of an email to be added to the support case.
    ccEmailAddresses :: Core.Maybe [Types.CcEmailAddress]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddCommunicationToCase' value with any optional fields omitted.
mkAddCommunicationToCase ::
  -- | 'communicationBody'
  Types.CommunicationBody ->
  AddCommunicationToCase
mkAddCommunicationToCase communicationBody =
  AddCommunicationToCase'
    { communicationBody,
      attachmentSetId = Core.Nothing,
      caseId = Core.Nothing,
      ccEmailAddresses = Core.Nothing
    }

-- | The body of an email communication to add to the support case.
--
-- /Note:/ Consider using 'communicationBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actcCommunicationBody :: Lens.Lens' AddCommunicationToCase Types.CommunicationBody
actcCommunicationBody = Lens.field @"communicationBody"
{-# DEPRECATED actcCommunicationBody "Use generic-lens or generic-optics with 'communicationBody' instead." #-}

-- | The ID of a set of one or more attachments for the communication to add to the case. Create the set by calling 'AddAttachmentsToSet'
--
-- /Note:/ Consider using 'attachmentSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actcAttachmentSetId :: Lens.Lens' AddCommunicationToCase (Core.Maybe Types.AttachmentSetId)
actcAttachmentSetId = Lens.field @"attachmentSetId"
{-# DEPRECATED actcAttachmentSetId "Use generic-lens or generic-optics with 'attachmentSetId' instead." #-}

-- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
--
-- /Note:/ Consider using 'caseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actcCaseId :: Lens.Lens' AddCommunicationToCase (Core.Maybe Types.CaseId)
actcCaseId = Lens.field @"caseId"
{-# DEPRECATED actcCaseId "Use generic-lens or generic-optics with 'caseId' instead." #-}

-- | The email addresses in the CC line of an email to be added to the support case.
--
-- /Note:/ Consider using 'ccEmailAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actcCcEmailAddresses :: Lens.Lens' AddCommunicationToCase (Core.Maybe [Types.CcEmailAddress])
actcCcEmailAddresses = Lens.field @"ccEmailAddresses"
{-# DEPRECATED actcCcEmailAddresses "Use generic-lens or generic-optics with 'ccEmailAddresses' instead." #-}

instance Core.FromJSON AddCommunicationToCase where
  toJSON AddCommunicationToCase {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("communicationBody" Core..= communicationBody),
            ("attachmentSetId" Core..=) Core.<$> attachmentSetId,
            ("caseId" Core..=) Core.<$> caseId,
            ("ccEmailAddresses" Core..=) Core.<$> ccEmailAddresses
          ]
      )

instance Core.AWSRequest AddCommunicationToCase where
  type Rs AddCommunicationToCase = AddCommunicationToCaseResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSSupport_20130415.AddCommunicationToCase")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          AddCommunicationToCaseResponse'
            Core.<$> (x Core..:? "result") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | The result of the 'AddCommunicationToCase' operation.
--
-- /See:/ 'mkAddCommunicationToCaseResponse' smart constructor.
data AddCommunicationToCaseResponse = AddCommunicationToCaseResponse'
  { -- | True if 'AddCommunicationToCase' succeeds. Otherwise, returns an error.
    result :: Core.Maybe Core.Bool,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AddCommunicationToCaseResponse' value with any optional fields omitted.
mkAddCommunicationToCaseResponse ::
  -- | 'responseStatus'
  Core.Int ->
  AddCommunicationToCaseResponse
mkAddCommunicationToCaseResponse responseStatus =
  AddCommunicationToCaseResponse'
    { result = Core.Nothing,
      responseStatus
    }

-- | True if 'AddCommunicationToCase' succeeds. Otherwise, returns an error.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actcrrsResult :: Lens.Lens' AddCommunicationToCaseResponse (Core.Maybe Core.Bool)
actcrrsResult = Lens.field @"result"
{-# DEPRECATED actcrrsResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actcrrsResponseStatus :: Lens.Lens' AddCommunicationToCaseResponse Core.Int
actcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED actcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
