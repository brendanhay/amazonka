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
-- Module      : Amazonka.IoTEvents.Types.EmailContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IoTEvents.Types.EmailContent where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains the subject and message of an email.
--
-- /See:/ 'newEmailContent' smart constructor.
data EmailContent = EmailContent'
  { -- | The message that you want to send. The message can be up to 200
    -- characters.
    additionalMessage :: Prelude.Maybe Prelude.Text,
    -- | The subject of the email.
    subject :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EmailContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'additionalMessage', 'emailContent_additionalMessage' - The message that you want to send. The message can be up to 200
-- characters.
--
-- 'subject', 'emailContent_subject' - The subject of the email.
newEmailContent ::
  EmailContent
newEmailContent =
  EmailContent'
    { additionalMessage = Prelude.Nothing,
      subject = Prelude.Nothing
    }

-- | The message that you want to send. The message can be up to 200
-- characters.
emailContent_additionalMessage :: Lens.Lens' EmailContent (Prelude.Maybe Prelude.Text)
emailContent_additionalMessage = Lens.lens (\EmailContent' {additionalMessage} -> additionalMessage) (\s@EmailContent' {} a -> s {additionalMessage = a} :: EmailContent)

-- | The subject of the email.
emailContent_subject :: Lens.Lens' EmailContent (Prelude.Maybe Prelude.Text)
emailContent_subject = Lens.lens (\EmailContent' {subject} -> subject) (\s@EmailContent' {} a -> s {subject = a} :: EmailContent)

instance Data.FromJSON EmailContent where
  parseJSON =
    Data.withObject
      "EmailContent"
      ( \x ->
          EmailContent'
            Prelude.<$> (x Data..:? "additionalMessage")
            Prelude.<*> (x Data..:? "subject")
      )

instance Prelude.Hashable EmailContent where
  hashWithSalt _salt EmailContent' {..} =
    _salt
      `Prelude.hashWithSalt` additionalMessage
      `Prelude.hashWithSalt` subject

instance Prelude.NFData EmailContent where
  rnf EmailContent' {..} =
    Prelude.rnf additionalMessage
      `Prelude.seq` Prelude.rnf subject

instance Data.ToJSON EmailContent where
  toJSON EmailContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("additionalMessage" Data..=)
              Prelude.<$> additionalMessage,
            ("subject" Data..=) Prelude.<$> subject
          ]
      )
