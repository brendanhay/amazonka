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
-- Module      : Amazonka.AmplifyBackend.Types.EmailSettings
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.EmailSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The configuration for the email sent when an app user forgets their
-- password.
--
-- /See:/ 'newEmailSettings' smart constructor.
data EmailSettings = EmailSettings'
  { -- | The contents of the email message.
    emailMessage :: Prelude.Maybe Prelude.Text,
    -- | The contents of the subject line of the email message.
    emailSubject :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EmailSettings' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailMessage', 'emailSettings_emailMessage' - The contents of the email message.
--
-- 'emailSubject', 'emailSettings_emailSubject' - The contents of the subject line of the email message.
newEmailSettings ::
  EmailSettings
newEmailSettings =
  EmailSettings'
    { emailMessage = Prelude.Nothing,
      emailSubject = Prelude.Nothing
    }

-- | The contents of the email message.
emailSettings_emailMessage :: Lens.Lens' EmailSettings (Prelude.Maybe Prelude.Text)
emailSettings_emailMessage = Lens.lens (\EmailSettings' {emailMessage} -> emailMessage) (\s@EmailSettings' {} a -> s {emailMessage = a} :: EmailSettings)

-- | The contents of the subject line of the email message.
emailSettings_emailSubject :: Lens.Lens' EmailSettings (Prelude.Maybe Prelude.Text)
emailSettings_emailSubject = Lens.lens (\EmailSettings' {emailSubject} -> emailSubject) (\s@EmailSettings' {} a -> s {emailSubject = a} :: EmailSettings)

instance Data.FromJSON EmailSettings where
  parseJSON =
    Data.withObject
      "EmailSettings"
      ( \x ->
          EmailSettings'
            Prelude.<$> (x Data..:? "emailMessage")
            Prelude.<*> (x Data..:? "emailSubject")
      )

instance Prelude.Hashable EmailSettings where
  hashWithSalt _salt EmailSettings' {..} =
    _salt
      `Prelude.hashWithSalt` emailMessage
      `Prelude.hashWithSalt` emailSubject

instance Prelude.NFData EmailSettings where
  rnf EmailSettings' {..} =
    Prelude.rnf emailMessage
      `Prelude.seq` Prelude.rnf emailSubject

instance Data.ToJSON EmailSettings where
  toJSON EmailSettings' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("emailMessage" Data..=) Prelude.<$> emailMessage,
            ("emailSubject" Data..=) Prelude.<$> emailSubject
          ]
      )
