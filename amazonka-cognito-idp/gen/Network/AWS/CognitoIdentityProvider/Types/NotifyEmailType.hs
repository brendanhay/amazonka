{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.NotifyEmailType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.NotifyEmailType where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | The notify email type.
--
-- /See:/ 'newNotifyEmailType' smart constructor.
data NotifyEmailType = NotifyEmailType'
  { -- | The HTML body.
    htmlBody :: Core.Maybe Core.Text,
    -- | The text body.
    textBody :: Core.Maybe Core.Text,
    -- | The subject.
    subject :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'NotifyEmailType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'htmlBody', 'notifyEmailType_htmlBody' - The HTML body.
--
-- 'textBody', 'notifyEmailType_textBody' - The text body.
--
-- 'subject', 'notifyEmailType_subject' - The subject.
newNotifyEmailType ::
  -- | 'subject'
  Core.Text ->
  NotifyEmailType
newNotifyEmailType pSubject_ =
  NotifyEmailType'
    { htmlBody = Core.Nothing,
      textBody = Core.Nothing,
      subject = pSubject_
    }

-- | The HTML body.
notifyEmailType_htmlBody :: Lens.Lens' NotifyEmailType (Core.Maybe Core.Text)
notifyEmailType_htmlBody = Lens.lens (\NotifyEmailType' {htmlBody} -> htmlBody) (\s@NotifyEmailType' {} a -> s {htmlBody = a} :: NotifyEmailType)

-- | The text body.
notifyEmailType_textBody :: Lens.Lens' NotifyEmailType (Core.Maybe Core.Text)
notifyEmailType_textBody = Lens.lens (\NotifyEmailType' {textBody} -> textBody) (\s@NotifyEmailType' {} a -> s {textBody = a} :: NotifyEmailType)

-- | The subject.
notifyEmailType_subject :: Lens.Lens' NotifyEmailType Core.Text
notifyEmailType_subject = Lens.lens (\NotifyEmailType' {subject} -> subject) (\s@NotifyEmailType' {} a -> s {subject = a} :: NotifyEmailType)

instance Core.FromJSON NotifyEmailType where
  parseJSON =
    Core.withObject
      "NotifyEmailType"
      ( \x ->
          NotifyEmailType'
            Core.<$> (x Core..:? "HtmlBody")
            Core.<*> (x Core..:? "TextBody")
            Core.<*> (x Core..: "Subject")
      )

instance Core.Hashable NotifyEmailType

instance Core.NFData NotifyEmailType

instance Core.ToJSON NotifyEmailType where
  toJSON NotifyEmailType' {..} =
    Core.object
      ( Core.catMaybes
          [ ("HtmlBody" Core..=) Core.<$> htmlBody,
            ("TextBody" Core..=) Core.<$> textBody,
            Core.Just ("Subject" Core..= subject)
          ]
      )
