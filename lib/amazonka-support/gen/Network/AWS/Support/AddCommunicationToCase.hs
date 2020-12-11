{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    actcCaseId,
    actcCcEmailAddresses,
    actcAttachmentSetId,
    actcCommunicationBody,

    -- * Destructuring the response
    AddCommunicationToCaseResponse (..),
    mkAddCommunicationToCaseResponse,

    -- ** Response lenses
    actcrsResult,
    actcrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.Support.Types

-- | /See:/ 'mkAddCommunicationToCase' smart constructor.
data AddCommunicationToCase = AddCommunicationToCase'
  { caseId ::
      Lude.Maybe Lude.Text,
    ccEmailAddresses :: Lude.Maybe [Lude.Text],
    attachmentSetId :: Lude.Maybe Lude.Text,
    communicationBody :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddCommunicationToCase' with the minimum fields required to make a request.
--
-- * 'attachmentSetId' - The ID of a set of one or more attachments for the communication to add to the case. Create the set by calling 'AddAttachmentsToSet'
-- * 'caseId' - The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
-- * 'ccEmailAddresses' - The email addresses in the CC line of an email to be added to the support case.
-- * 'communicationBody' - The body of an email communication to add to the support case.
mkAddCommunicationToCase ::
  -- | 'communicationBody'
  Lude.Text ->
  AddCommunicationToCase
mkAddCommunicationToCase pCommunicationBody_ =
  AddCommunicationToCase'
    { caseId = Lude.Nothing,
      ccEmailAddresses = Lude.Nothing,
      attachmentSetId = Lude.Nothing,
      communicationBody = pCommunicationBody_
    }

-- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
--
-- /Note:/ Consider using 'caseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actcCaseId :: Lens.Lens' AddCommunicationToCase (Lude.Maybe Lude.Text)
actcCaseId = Lens.lens (caseId :: AddCommunicationToCase -> Lude.Maybe Lude.Text) (\s a -> s {caseId = a} :: AddCommunicationToCase)
{-# DEPRECATED actcCaseId "Use generic-lens or generic-optics with 'caseId' instead." #-}

-- | The email addresses in the CC line of an email to be added to the support case.
--
-- /Note:/ Consider using 'ccEmailAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actcCcEmailAddresses :: Lens.Lens' AddCommunicationToCase (Lude.Maybe [Lude.Text])
actcCcEmailAddresses = Lens.lens (ccEmailAddresses :: AddCommunicationToCase -> Lude.Maybe [Lude.Text]) (\s a -> s {ccEmailAddresses = a} :: AddCommunicationToCase)
{-# DEPRECATED actcCcEmailAddresses "Use generic-lens or generic-optics with 'ccEmailAddresses' instead." #-}

-- | The ID of a set of one or more attachments for the communication to add to the case. Create the set by calling 'AddAttachmentsToSet'
--
-- /Note:/ Consider using 'attachmentSetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actcAttachmentSetId :: Lens.Lens' AddCommunicationToCase (Lude.Maybe Lude.Text)
actcAttachmentSetId = Lens.lens (attachmentSetId :: AddCommunicationToCase -> Lude.Maybe Lude.Text) (\s a -> s {attachmentSetId = a} :: AddCommunicationToCase)
{-# DEPRECATED actcAttachmentSetId "Use generic-lens or generic-optics with 'attachmentSetId' instead." #-}

-- | The body of an email communication to add to the support case.
--
-- /Note:/ Consider using 'communicationBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actcCommunicationBody :: Lens.Lens' AddCommunicationToCase Lude.Text
actcCommunicationBody = Lens.lens (communicationBody :: AddCommunicationToCase -> Lude.Text) (\s a -> s {communicationBody = a} :: AddCommunicationToCase)
{-# DEPRECATED actcCommunicationBody "Use generic-lens or generic-optics with 'communicationBody' instead." #-}

instance Lude.AWSRequest AddCommunicationToCase where
  type Rs AddCommunicationToCase = AddCommunicationToCaseResponse
  request = Req.postJSON supportService
  response =
    Res.receiveJSON
      ( \s h x ->
          AddCommunicationToCaseResponse'
            Lude.<$> (x Lude..?> "result") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddCommunicationToCase where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSSupport_20130415.AddCommunicationToCase" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddCommunicationToCase where
  toJSON AddCommunicationToCase' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("caseId" Lude..=) Lude.<$> caseId,
            ("ccEmailAddresses" Lude..=) Lude.<$> ccEmailAddresses,
            ("attachmentSetId" Lude..=) Lude.<$> attachmentSetId,
            Lude.Just ("communicationBody" Lude..= communicationBody)
          ]
      )

instance Lude.ToPath AddCommunicationToCase where
  toPath = Lude.const "/"

instance Lude.ToQuery AddCommunicationToCase where
  toQuery = Lude.const Lude.mempty

-- | The result of the 'AddCommunicationToCase' operation.
--
-- /See:/ 'mkAddCommunicationToCaseResponse' smart constructor.
data AddCommunicationToCaseResponse = AddCommunicationToCaseResponse'
  { result ::
      Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddCommunicationToCaseResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'result' - True if 'AddCommunicationToCase' succeeds. Otherwise, returns an error.
mkAddCommunicationToCaseResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddCommunicationToCaseResponse
mkAddCommunicationToCaseResponse pResponseStatus_ =
  AddCommunicationToCaseResponse'
    { result = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | True if 'AddCommunicationToCase' succeeds. Otherwise, returns an error.
--
-- /Note:/ Consider using 'result' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actcrsResult :: Lens.Lens' AddCommunicationToCaseResponse (Lude.Maybe Lude.Bool)
actcrsResult = Lens.lens (result :: AddCommunicationToCaseResponse -> Lude.Maybe Lude.Bool) (\s a -> s {result = a} :: AddCommunicationToCaseResponse)
{-# DEPRECATED actcrsResult "Use generic-lens or generic-optics with 'result' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
actcrsResponseStatus :: Lens.Lens' AddCommunicationToCaseResponse Lude.Int
actcrsResponseStatus = Lens.lens (responseStatus :: AddCommunicationToCaseResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddCommunicationToCaseResponse)
{-# DEPRECATED actcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
