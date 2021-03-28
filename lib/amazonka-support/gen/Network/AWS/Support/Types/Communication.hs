{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Support.Types.Communication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Support.Types.Communication
  ( Communication (..)
  -- * Smart constructor
  , mkCommunication
  -- * Lenses
  , cAttachmentSet
  , cBody
  , cCaseId
  , cSubmittedBy
  , cTimeCreated
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Support.Types.AttachmentDetails as Types
import qualified Network.AWS.Support.Types.Body as Types
import qualified Network.AWS.Support.Types.CaseId as Types
import qualified Network.AWS.Support.Types.SubmittedBy as Types
import qualified Network.AWS.Support.Types.TimeCreated as Types

-- | A communication associated with an AWS Support case. The communication consists of the case ID, the message body, attachment information, the submitter of the communication, and the date and time of the communication.
--
-- /See:/ 'mkCommunication' smart constructor.
data Communication = Communication'
  { attachmentSet :: Core.Maybe [Types.AttachmentDetails]
    -- ^ Information about the attachments to the case communication.
  , body :: Core.Maybe Types.Body
    -- ^ The text of the communication between the customer and AWS Support.
  , caseId :: Core.Maybe Types.CaseId
    -- ^ The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/ 
  , submittedBy :: Core.Maybe Types.SubmittedBy
    -- ^ The identity of the account that submitted, or responded to, the support case. Customer entries include the role or IAM user as well as the email address. For example, "AdminRole (Role) <someone@example.com>. Entries from the AWS Support team display "Amazon Web Services," and do not show an email address. 
  , timeCreated :: Core.Maybe Types.TimeCreated
    -- ^ The time the communication was created.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Communication' value with any optional fields omitted.
mkCommunication
    :: Communication
mkCommunication
  = Communication'{attachmentSet = Core.Nothing, body = Core.Nothing,
                   caseId = Core.Nothing, submittedBy = Core.Nothing,
                   timeCreated = Core.Nothing}

-- | Information about the attachments to the case communication.
--
-- /Note:/ Consider using 'attachmentSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cAttachmentSet :: Lens.Lens' Communication (Core.Maybe [Types.AttachmentDetails])
cAttachmentSet = Lens.field @"attachmentSet"
{-# INLINEABLE cAttachmentSet #-}
{-# DEPRECATED attachmentSet "Use generic-lens or generic-optics with 'attachmentSet' instead"  #-}

-- | The text of the communication between the customer and AWS Support.
--
-- /Note:/ Consider using 'body' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cBody :: Lens.Lens' Communication (Core.Maybe Types.Body)
cBody = Lens.field @"body"
{-# INLINEABLE cBody #-}
{-# DEPRECATED body "Use generic-lens or generic-optics with 'body' instead"  #-}

-- | The AWS Support case ID requested or returned in the call. The case ID is an alphanumeric string formatted as shown in this example: case-/12345678910-2013-c4c1d2bf33c5cf47/ 
--
-- /Note:/ Consider using 'caseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCaseId :: Lens.Lens' Communication (Core.Maybe Types.CaseId)
cCaseId = Lens.field @"caseId"
{-# INLINEABLE cCaseId #-}
{-# DEPRECATED caseId "Use generic-lens or generic-optics with 'caseId' instead"  #-}

-- | The identity of the account that submitted, or responded to, the support case. Customer entries include the role or IAM user as well as the email address. For example, "AdminRole (Role) <someone@example.com>. Entries from the AWS Support team display "Amazon Web Services," and do not show an email address. 
--
-- /Note:/ Consider using 'submittedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSubmittedBy :: Lens.Lens' Communication (Core.Maybe Types.SubmittedBy)
cSubmittedBy = Lens.field @"submittedBy"
{-# INLINEABLE cSubmittedBy #-}
{-# DEPRECATED submittedBy "Use generic-lens or generic-optics with 'submittedBy' instead"  #-}

-- | The time the communication was created.
--
-- /Note:/ Consider using 'timeCreated' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cTimeCreated :: Lens.Lens' Communication (Core.Maybe Types.TimeCreated)
cTimeCreated = Lens.field @"timeCreated"
{-# INLINEABLE cTimeCreated #-}
{-# DEPRECATED timeCreated "Use generic-lens or generic-optics with 'timeCreated' instead"  #-}

instance Core.FromJSON Communication where
        parseJSON
          = Core.withObject "Communication" Core.$
              \ x ->
                Communication' Core.<$>
                  (x Core..:? "attachmentSet") Core.<*> x Core..:? "body" Core.<*>
                    x Core..:? "caseId"
                    Core.<*> x Core..:? "submittedBy"
                    Core.<*> x Core..:? "timeCreated"
