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
-- Module      : Amazonka.CognitoIdentityProvider.Types.NotifyEmailType
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CognitoIdentityProvider.Types.NotifyEmailType where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The notify email type.
--
-- /See:/ 'newNotifyEmailType' smart constructor.
data NotifyEmailType = NotifyEmailType'
  { -- | The email text body.
    textBody :: Prelude.Maybe Prelude.Text,
    -- | The email HTML body.
    htmlBody :: Prelude.Maybe Prelude.Text,
    -- | The email subject.
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
-- 'textBody', 'notifyEmailType_textBody' - The email text body.
--
-- 'htmlBody', 'notifyEmailType_htmlBody' - The email HTML body.
--
-- 'subject', 'notifyEmailType_subject' - The email subject.
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

-- | The email text body.
notifyEmailType_textBody :: Lens.Lens' NotifyEmailType (Prelude.Maybe Prelude.Text)
notifyEmailType_textBody = Lens.lens (\NotifyEmailType' {textBody} -> textBody) (\s@NotifyEmailType' {} a -> s {textBody = a} :: NotifyEmailType)

-- | The email HTML body.
notifyEmailType_htmlBody :: Lens.Lens' NotifyEmailType (Prelude.Maybe Prelude.Text)
notifyEmailType_htmlBody = Lens.lens (\NotifyEmailType' {htmlBody} -> htmlBody) (\s@NotifyEmailType' {} a -> s {htmlBody = a} :: NotifyEmailType)

-- | The email subject.
notifyEmailType_subject :: Lens.Lens' NotifyEmailType Prelude.Text
notifyEmailType_subject = Lens.lens (\NotifyEmailType' {subject} -> subject) (\s@NotifyEmailType' {} a -> s {subject = a} :: NotifyEmailType)

instance Data.FromJSON NotifyEmailType where
  parseJSON =
    Data.withObject
      "NotifyEmailType"
      ( \x ->
          NotifyEmailType'
            Prelude.<$> (x Data..:? "TextBody")
            Prelude.<*> (x Data..:? "HtmlBody")
            Prelude.<*> (x Data..: "Subject")
      )

instance Prelude.Hashable NotifyEmailType where
  hashWithSalt _salt NotifyEmailType' {..} =
    _salt `Prelude.hashWithSalt` textBody
      `Prelude.hashWithSalt` htmlBody
      `Prelude.hashWithSalt` subject

instance Prelude.NFData NotifyEmailType where
  rnf NotifyEmailType' {..} =
    Prelude.rnf textBody
      `Prelude.seq` Prelude.rnf htmlBody
      `Prelude.seq` Prelude.rnf subject

instance Data.ToJSON NotifyEmailType where
  toJSON NotifyEmailType' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("TextBody" Data..=) Prelude.<$> textBody,
            ("HtmlBody" Data..=) Prelude.<$> htmlBody,
            Prelude.Just ("Subject" Data..= subject)
          ]
      )
