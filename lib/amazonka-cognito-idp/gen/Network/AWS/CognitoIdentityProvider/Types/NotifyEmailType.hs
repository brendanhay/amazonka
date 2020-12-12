{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.NotifyEmailType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.NotifyEmailType
  ( NotifyEmailType (..),

    -- * Smart constructor
    mkNotifyEmailType,

    -- * Lenses
    netTextBody,
    netHTMLBody,
    netSubject,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The notify email type.
--
-- /See:/ 'mkNotifyEmailType' smart constructor.
data NotifyEmailType = NotifyEmailType'
  { textBody ::
      Lude.Maybe Lude.Text,
    htmlBody :: Lude.Maybe Lude.Text,
    subject :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotifyEmailType' with the minimum fields required to make a request.
--
-- * 'htmlBody' - The HTML body.
-- * 'subject' - The subject.
-- * 'textBody' - The text body.
mkNotifyEmailType ::
  -- | 'subject'
  Lude.Text ->
  NotifyEmailType
mkNotifyEmailType pSubject_ =
  NotifyEmailType'
    { textBody = Lude.Nothing,
      htmlBody = Lude.Nothing,
      subject = pSubject_
    }

-- | The text body.
--
-- /Note:/ Consider using 'textBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
netTextBody :: Lens.Lens' NotifyEmailType (Lude.Maybe Lude.Text)
netTextBody = Lens.lens (textBody :: NotifyEmailType -> Lude.Maybe Lude.Text) (\s a -> s {textBody = a} :: NotifyEmailType)
{-# DEPRECATED netTextBody "Use generic-lens or generic-optics with 'textBody' instead." #-}

-- | The HTML body.
--
-- /Note:/ Consider using 'htmlBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
netHTMLBody :: Lens.Lens' NotifyEmailType (Lude.Maybe Lude.Text)
netHTMLBody = Lens.lens (htmlBody :: NotifyEmailType -> Lude.Maybe Lude.Text) (\s a -> s {htmlBody = a} :: NotifyEmailType)
{-# DEPRECATED netHTMLBody "Use generic-lens or generic-optics with 'htmlBody' instead." #-}

-- | The subject.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
netSubject :: Lens.Lens' NotifyEmailType Lude.Text
netSubject = Lens.lens (subject :: NotifyEmailType -> Lude.Text) (\s a -> s {subject = a} :: NotifyEmailType)
{-# DEPRECATED netSubject "Use generic-lens or generic-optics with 'subject' instead." #-}

instance Lude.FromJSON NotifyEmailType where
  parseJSON =
    Lude.withObject
      "NotifyEmailType"
      ( \x ->
          NotifyEmailType'
            Lude.<$> (x Lude..:? "TextBody")
            Lude.<*> (x Lude..:? "HtmlBody")
            Lude.<*> (x Lude..: "Subject")
      )

instance Lude.ToJSON NotifyEmailType where
  toJSON NotifyEmailType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("TextBody" Lude..=) Lude.<$> textBody,
            ("HtmlBody" Lude..=) Lude.<$> htmlBody,
            Lude.Just ("Subject" Lude..= subject)
          ]
      )
