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
-- Module      : Network.AWS.IoTEvents.Types.EmailContent
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoTEvents.Types.EmailContent where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the subject and message of an email.
--
-- /See:/ 'newEmailContent' smart constructor.
data EmailContent = EmailContent'
  { -- | The subject of the email.
    subject :: Prelude.Maybe Prelude.Text,
    -- | The message that you want to send. The message can be up to 200
    -- characters.
    additionalMessage :: Prelude.Maybe Prelude.Text
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
-- 'subject', 'emailContent_subject' - The subject of the email.
--
-- 'additionalMessage', 'emailContent_additionalMessage' - The message that you want to send. The message can be up to 200
-- characters.
newEmailContent ::
  EmailContent
newEmailContent =
  EmailContent'
    { subject = Prelude.Nothing,
      additionalMessage = Prelude.Nothing
    }

-- | The subject of the email.
emailContent_subject :: Lens.Lens' EmailContent (Prelude.Maybe Prelude.Text)
emailContent_subject = Lens.lens (\EmailContent' {subject} -> subject) (\s@EmailContent' {} a -> s {subject = a} :: EmailContent)

-- | The message that you want to send. The message can be up to 200
-- characters.
emailContent_additionalMessage :: Lens.Lens' EmailContent (Prelude.Maybe Prelude.Text)
emailContent_additionalMessage = Lens.lens (\EmailContent' {additionalMessage} -> additionalMessage) (\s@EmailContent' {} a -> s {additionalMessage = a} :: EmailContent)

instance Core.FromJSON EmailContent where
  parseJSON =
    Core.withObject
      "EmailContent"
      ( \x ->
          EmailContent'
            Prelude.<$> (x Core..:? "subject")
            Prelude.<*> (x Core..:? "additionalMessage")
      )

instance Prelude.Hashable EmailContent

instance Prelude.NFData EmailContent

instance Core.ToJSON EmailContent where
  toJSON EmailContent' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("subject" Core..=) Prelude.<$> subject,
            ("additionalMessage" Core..=)
              Prelude.<$> additionalMessage
          ]
      )
