{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.NotifyEmailType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.NotifyEmailType
  ( NotifyEmailType (..)
  -- * Smart constructor
  , mkNotifyEmailType
  -- * Lenses
  , netSubject
  , netHtmlBody
  , netTextBody
  ) where

import qualified Network.AWS.CognitoIdentityProvider.Types.EmailNotificationSubjectType as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.HtmlBody as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.TextBody as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The notify email type.
--
-- /See:/ 'mkNotifyEmailType' smart constructor.
data NotifyEmailType = NotifyEmailType'
  { subject :: Types.EmailNotificationSubjectType
    -- ^ The subject.
  , htmlBody :: Core.Maybe Types.HtmlBody
    -- ^ The HTML body.
  , textBody :: Core.Maybe Types.TextBody
    -- ^ The text body.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NotifyEmailType' value with any optional fields omitted.
mkNotifyEmailType
    :: Types.EmailNotificationSubjectType -- ^ 'subject'
    -> NotifyEmailType
mkNotifyEmailType subject
  = NotifyEmailType'{subject, htmlBody = Core.Nothing,
                     textBody = Core.Nothing}

-- | The subject.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
netSubject :: Lens.Lens' NotifyEmailType Types.EmailNotificationSubjectType
netSubject = Lens.field @"subject"
{-# INLINEABLE netSubject #-}
{-# DEPRECATED subject "Use generic-lens or generic-optics with 'subject' instead"  #-}

-- | The HTML body.
--
-- /Note:/ Consider using 'htmlBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
netHtmlBody :: Lens.Lens' NotifyEmailType (Core.Maybe Types.HtmlBody)
netHtmlBody = Lens.field @"htmlBody"
{-# INLINEABLE netHtmlBody #-}
{-# DEPRECATED htmlBody "Use generic-lens or generic-optics with 'htmlBody' instead"  #-}

-- | The text body.
--
-- /Note:/ Consider using 'textBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
netTextBody :: Lens.Lens' NotifyEmailType (Core.Maybe Types.TextBody)
netTextBody = Lens.field @"textBody"
{-# INLINEABLE netTextBody #-}
{-# DEPRECATED textBody "Use generic-lens or generic-optics with 'textBody' instead"  #-}

instance Core.FromJSON NotifyEmailType where
        toJSON NotifyEmailType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Subject" Core..= subject),
                  ("HtmlBody" Core..=) Core.<$> htmlBody,
                  ("TextBody" Core..=) Core.<$> textBody])

instance Core.FromJSON NotifyEmailType where
        parseJSON
          = Core.withObject "NotifyEmailType" Core.$
              \ x ->
                NotifyEmailType' Core.<$>
                  (x Core..: "Subject") Core.<*> x Core..:? "HtmlBody" Core.<*>
                    x Core..:? "TextBody"
