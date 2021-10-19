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
import qualified Network.AWS.Prelude as Prelude

-- | The notify email type.
--
-- /See:/ 'newNotifyEmailType' smart constructor.
data NotifyEmailType = NotifyEmailType'
  { -- | The text body.
    textBody :: Prelude.Maybe Prelude.Text,
    -- | The HTML body.
    htmlBody :: Prelude.Maybe Prelude.Text,
    -- | The subject.
    subject :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NotifyEmailType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'textBody', 'notifyEmailType_textBody' - The text body.
--
-- 'htmlBody', 'notifyEmailType_htmlBody' - The HTML body.
--
-- 'subject', 'notifyEmailType_subject' - The subject.
newNotifyEmailType ::
  -- | 'subject'
  Prelude.Text ->
  NotifyEmailType
newNotifyEmailType pSubject_ =
  NotifyEmailType'
    { textBody = Prelude.Nothing,
      htmlBody = Prelude.Nothing,
      subject = pSubject_
    }

-- | The text body.
notifyEmailType_textBody :: Lens.Lens' NotifyEmailType (Prelude.Maybe Prelude.Text)
notifyEmailType_textBody = Lens.lens (\NotifyEmailType' {textBody} -> textBody) (\s@NotifyEmailType' {} a -> s {textBody = a} :: NotifyEmailType)

-- | The HTML body.
notifyEmailType_htmlBody :: Lens.Lens' NotifyEmailType (Prelude.Maybe Prelude.Text)
notifyEmailType_htmlBody = Lens.lens (\NotifyEmailType' {htmlBody} -> htmlBody) (\s@NotifyEmailType' {} a -> s {htmlBody = a} :: NotifyEmailType)

-- | The subject.
notifyEmailType_subject :: Lens.Lens' NotifyEmailType Prelude.Text
notifyEmailType_subject = Lens.lens (\NotifyEmailType' {subject} -> subject) (\s@NotifyEmailType' {} a -> s {subject = a} :: NotifyEmailType)

instance Core.FromJSON NotifyEmailType where
  parseJSON =
    Core.withObject
      "NotifyEmailType"
      ( \x ->
          NotifyEmailType'
            Prelude.<$> (x Core..:? "TextBody")
            Prelude.<*> (x Core..:? "HtmlBody")
            Prelude.<*> (x Core..: "Subject")
      )

instance Prelude.Hashable NotifyEmailType

instance Prelude.NFData NotifyEmailType

instance Core.ToJSON NotifyEmailType where
  toJSON NotifyEmailType' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TextBody" Core..=) Prelude.<$> textBody,
            ("HtmlBody" Core..=) Prelude.<$> htmlBody,
            Prelude.Just ("Subject" Core..= subject)
          ]
      )
