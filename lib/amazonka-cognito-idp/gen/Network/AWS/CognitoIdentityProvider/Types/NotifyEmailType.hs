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
    netSubject,
    netTextBody,
    netHTMLBody,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The notify email type.
--
-- /See:/ 'mkNotifyEmailType' smart constructor.
data NotifyEmailType = NotifyEmailType'
  { -- | The subject.
    subject :: Lude.Text,
    -- | The text body.
    textBody :: Lude.Maybe Lude.Text,
    -- | The HTML body.
    htmlBody :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotifyEmailType' with the minimum fields required to make a request.
--
-- * 'subject' - The subject.
-- * 'textBody' - The text body.
-- * 'htmlBody' - The HTML body.
mkNotifyEmailType ::
  -- | 'subject'
  Lude.Text ->
  NotifyEmailType
mkNotifyEmailType pSubject_ =
  NotifyEmailType'
    { subject = pSubject_,
      textBody = Lude.Nothing,
      htmlBody = Lude.Nothing
    }

-- | The subject.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
netSubject :: Lens.Lens' NotifyEmailType Lude.Text
netSubject = Lens.lens (subject :: NotifyEmailType -> Lude.Text) (\s a -> s {subject = a} :: NotifyEmailType)
{-# DEPRECATED netSubject "Use generic-lens or generic-optics with 'subject' instead." #-}

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

instance Lude.FromJSON NotifyEmailType where
  parseJSON =
    Lude.withObject
      "NotifyEmailType"
      ( \x ->
          NotifyEmailType'
            Lude.<$> (x Lude..: "Subject")
            Lude.<*> (x Lude..:? "TextBody")
            Lude.<*> (x Lude..:? "HtmlBody")
      )

instance Lude.ToJSON NotifyEmailType where
  toJSON NotifyEmailType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("Subject" Lude..= subject),
            ("TextBody" Lude..=) Lude.<$> textBody,
            ("HtmlBody" Lude..=) Lude.<$> htmlBody
          ]
      )
