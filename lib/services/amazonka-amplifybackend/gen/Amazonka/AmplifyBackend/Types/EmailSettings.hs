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
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AmplifyBackend.Types.EmailSettings where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | The configuration for the email sent when an app user forgets their
-- password.
--
-- /See:/ 'newEmailSettings' smart constructor.
data EmailSettings = EmailSettings'
  { -- | The subject of the email.
    emailSubject :: Prelude.Maybe Prelude.Text,
    -- | The body of the email.
    emailMessage :: Prelude.Maybe Prelude.Text
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
-- 'emailSubject', 'emailSettings_emailSubject' - The subject of the email.
--
-- 'emailMessage', 'emailSettings_emailMessage' - The body of the email.
newEmailSettings ::
  EmailSettings
newEmailSettings =
  EmailSettings'
    { emailSubject = Prelude.Nothing,
      emailMessage = Prelude.Nothing
    }

-- | The subject of the email.
emailSettings_emailSubject :: Lens.Lens' EmailSettings (Prelude.Maybe Prelude.Text)
emailSettings_emailSubject = Lens.lens (\EmailSettings' {emailSubject} -> emailSubject) (\s@EmailSettings' {} a -> s {emailSubject = a} :: EmailSettings)

-- | The body of the email.
emailSettings_emailMessage :: Lens.Lens' EmailSettings (Prelude.Maybe Prelude.Text)
emailSettings_emailMessage = Lens.lens (\EmailSettings' {emailMessage} -> emailMessage) (\s@EmailSettings' {} a -> s {emailMessage = a} :: EmailSettings)

instance Core.FromJSON EmailSettings where
  parseJSON =
    Core.withObject
      "EmailSettings"
      ( \x ->
          EmailSettings'
            Prelude.<$> (x Core..:? "emailSubject")
            Prelude.<*> (x Core..:? "emailMessage")
      )

instance Prelude.Hashable EmailSettings where
  hashWithSalt salt' EmailSettings' {..} =
    salt' `Prelude.hashWithSalt` emailMessage
      `Prelude.hashWithSalt` emailSubject

instance Prelude.NFData EmailSettings where
  rnf EmailSettings' {..} =
    Prelude.rnf emailSubject
      `Prelude.seq` Prelude.rnf emailMessage

instance Core.ToJSON EmailSettings where
  toJSON EmailSettings' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("emailSubject" Core..=) Prelude.<$> emailSubject,
            ("emailMessage" Core..=) Prelude.<$> emailMessage
          ]
      )
