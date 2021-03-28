{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SES.Types.Template
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.SES.Types.Template
  ( Template (..)
  -- * Smart constructor
  , mkTemplate
  -- * Lenses
  , tTemplateName
  , tHtmlPart
  , tSubjectPart
  , tTextPart
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.SES.Types.HtmlPart as Types
import qualified Network.AWS.SES.Types.SubjectPart as Types
import qualified Network.AWS.SES.Types.TemplateName as Types
import qualified Network.AWS.SES.Types.TextPart as Types

-- | The content of the email, composed of a subject line, an HTML part, and a text-only part.
--
-- /See:/ 'mkTemplate' smart constructor.
data Template = Template'
  { templateName :: Types.TemplateName
    -- ^ The name of the template. You will refer to this name when you send email using the @SendTemplatedEmail@ or @SendBulkTemplatedEmail@ operations.
  , htmlPart :: Core.Maybe Types.HtmlPart
    -- ^ The HTML body of the email.
  , subjectPart :: Core.Maybe Types.SubjectPart
    -- ^ The subject line of the email.
  , textPart :: Core.Maybe Types.TextPart
    -- ^ The email body that will be visible to recipients whose email clients do not display HTML.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Template' value with any optional fields omitted.
mkTemplate
    :: Types.TemplateName -- ^ 'templateName'
    -> Template
mkTemplate templateName
  = Template'{templateName, htmlPart = Core.Nothing,
              subjectPart = Core.Nothing, textPart = Core.Nothing}

-- | The name of the template. You will refer to this name when you send email using the @SendTemplatedEmail@ or @SendBulkTemplatedEmail@ operations.
--
-- /Note:/ Consider using 'templateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTemplateName :: Lens.Lens' Template Types.TemplateName
tTemplateName = Lens.field @"templateName"
{-# INLINEABLE tTemplateName #-}
{-# DEPRECATED templateName "Use generic-lens or generic-optics with 'templateName' instead"  #-}

-- | The HTML body of the email.
--
-- /Note:/ Consider using 'htmlPart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tHtmlPart :: Lens.Lens' Template (Core.Maybe Types.HtmlPart)
tHtmlPart = Lens.field @"htmlPart"
{-# INLINEABLE tHtmlPart #-}
{-# DEPRECATED htmlPart "Use generic-lens or generic-optics with 'htmlPart' instead"  #-}

-- | The subject line of the email.
--
-- /Note:/ Consider using 'subjectPart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tSubjectPart :: Lens.Lens' Template (Core.Maybe Types.SubjectPart)
tSubjectPart = Lens.field @"subjectPart"
{-# INLINEABLE tSubjectPart #-}
{-# DEPRECATED subjectPart "Use generic-lens or generic-optics with 'subjectPart' instead"  #-}

-- | The email body that will be visible to recipients whose email clients do not display HTML.
--
-- /Note:/ Consider using 'textPart' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTextPart :: Lens.Lens' Template (Core.Maybe Types.TextPart)
tTextPart = Lens.field @"textPart"
{-# INLINEABLE tTextPart #-}
{-# DEPRECATED textPart "Use generic-lens or generic-optics with 'textPart' instead"  #-}

instance Core.ToQuery Template where
        toQuery Template{..}
          = Core.toQueryPair "TemplateName" templateName Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "HtmlPart") htmlPart
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SubjectPart") subjectPart
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "TextPart") textPart

instance Core.FromXML Template where
        parseXML x
          = Template' Core.<$>
              (x Core..@ "TemplateName") Core.<*> x Core..@? "HtmlPart" Core.<*>
                x Core..@? "SubjectPart"
                Core.<*> x Core..@? "TextPart"
