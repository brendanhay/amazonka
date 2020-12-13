{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.Communication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Support.Types.Communication
  ( Communication (..),

    -- * Smart constructor
    mkCommunication,

    -- * Lenses
    cBody,
    cCaseId,
    cSubmittedBy,
    cTimeCreated,
    cAttachmentSet,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.Support.Types.AttachmentDetails

-- | A communication associated with an AWS Support case. The communication consists of the case ID, the message body, attachment information, the submitter of the communication, and the date and time of the communication.
--
-- /See:/ 'mkCommunication' smart constructor.
data Communication = Communication'
  { -- | The text of the communication between the customer and AWS Support.
    body :: Lude.Maybe Lude.Text,
    -- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
    caseId :: Lude.Maybe Lude.Text,
    -- | The identity of the account that submitted, or responded to, the support case. Customer entries include the role or IAM user as well as the email address. For example, "AdminRole (Role) <someone@example.com>. Entries from the AWS Support team display "Amazon Web Services," and do not show an email address.
    submittedBy :: Lude.Maybe Lude.Text,
    -- | The time the communication was created.
    timeCreated :: Lude.Maybe Lude.Text,
    -- | Information about the attachments to the case communication.
    attachmentSet :: Lude.Maybe [AttachmentDetails]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Communication' with the minimum fields required to make a request.
--
-- * 'body' - The text of the communication between the customer and AWS Support.
-- * 'caseId' - The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
-- * 'submittedBy' - The identity of the account that submitted, or responded to, the support case. Customer entries include the role or IAM user as well as the email address. For example, "AdminRole (Role) <someone@example.com>. Entries from the AWS Support team display "Amazon Web Services," and do not show an email address.
-- * 'timeCreated' - The time the communication was created.
-- * 'attachmentSet' - Information about the attachments to the case communication.
mkCommunication ::
  Communication
mkCommunication =
  Communication'
    { body = Lude.Nothing,
      caseId = Lude.Nothing,
      submittedBy = Lude.Nothing,
      timeCreated = Lude.Nothing,
      attachmentSet = Lude.Nothing
    }

-- | The text of the communication between the customer and AWS Support.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cBody :: Lens.Lens' Communication (Lude.Maybe Lude.Text)
cBody = Lens.lens (body :: Communication -> Lude.Maybe Lude.Text) (\s a -> s {body = a} :: Communication)
{-# DEPRECATED cBody "Use generic-lens or generic-optics with 'body' instead." #-}

-- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/
--
-- /Note:/ Consider using 'caseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCaseId :: Lens.Lens' Communication (Lude.Maybe Lude.Text)
cCaseId = Lens.lens (caseId :: Communication -> Lude.Maybe Lude.Text) (\s a -> s {caseId = a} :: Communication)
{-# DEPRECATED cCaseId "Use generic-lens or generic-optics with 'caseId' instead." #-}

-- | The identity of the account that submitted, or responded to, the support case. Customer entries include the role or IAM user as well as the email address. For example, "AdminRole (Role) <someone@example.com>. Entries from the AWS Support team display "Amazon Web Services," and do not show an email address.
--
-- /Note:/ Consider using 'submittedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSubmittedBy :: Lens.Lens' Communication (Lude.Maybe Lude.Text)
cSubmittedBy = Lens.lens (submittedBy :: Communication -> Lude.Maybe Lude.Text) (\s a -> s {submittedBy = a} :: Communication)
{-# DEPRECATED cSubmittedBy "Use generic-lens or generic-optics with 'submittedBy' instead." #-}

-- | The time the communication was created.
--
-- /Note:/ Consider using 'timeCreated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTimeCreated :: Lens.Lens' Communication (Lude.Maybe Lude.Text)
cTimeCreated = Lens.lens (timeCreated :: Communication -> Lude.Maybe Lude.Text) (\s a -> s {timeCreated = a} :: Communication)
{-# DEPRECATED cTimeCreated "Use generic-lens or generic-optics with 'timeCreated' instead." #-}

-- | Information about the attachments to the case communication.
--
-- /Note:/ Consider using 'attachmentSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAttachmentSet :: Lens.Lens' Communication (Lude.Maybe [AttachmentDetails])
cAttachmentSet = Lens.lens (attachmentSet :: Communication -> Lude.Maybe [AttachmentDetails]) (\s a -> s {attachmentSet = a} :: Communication)
{-# DEPRECATED cAttachmentSet "Use generic-lens or generic-optics with 'attachmentSet' instead." #-}

instance Lude.FromJSON Communication where
  parseJSON =
    Lude.withObject
      "Communication"
      ( \x ->
          Communication'
            Lude.<$> (x Lude..:? "body")
            Lude.<*> (x Lude..:? "caseId")
            Lude.<*> (x Lude..:? "submittedBy")
            Lude.<*> (x Lude..:? "timeCreated")
            Lude.<*> (x Lude..:? "attachmentSet" Lude..!= Lude.mempty)
      )
