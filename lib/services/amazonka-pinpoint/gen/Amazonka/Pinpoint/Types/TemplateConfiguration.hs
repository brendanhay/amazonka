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
-- Module      : Amazonka.Pinpoint.Types.TemplateConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.TemplateConfiguration where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.Template
import qualified Amazonka.Prelude as Prelude

-- | Specifies the message template to use for the message, for each type of
-- channel.
--
-- /See:/ 'newTemplateConfiguration' smart constructor.
data TemplateConfiguration = TemplateConfiguration'
  { -- | The email template to use for the message.
    emailTemplate :: Prelude.Maybe Template,
    -- | The push notification template to use for the message.
    pushTemplate :: Prelude.Maybe Template,
    -- | The SMS template to use for the message.
    sMSTemplate :: Prelude.Maybe Template,
    -- | The voice template to use for the message. This object isn\'t supported
    -- for campaigns.
    voiceTemplate :: Prelude.Maybe Template
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'emailTemplate', 'templateConfiguration_emailTemplate' - The email template to use for the message.
--
-- 'pushTemplate', 'templateConfiguration_pushTemplate' - The push notification template to use for the message.
--
-- 'sMSTemplate', 'templateConfiguration_sMSTemplate' - The SMS template to use for the message.
--
-- 'voiceTemplate', 'templateConfiguration_voiceTemplate' - The voice template to use for the message. This object isn\'t supported
-- for campaigns.
newTemplateConfiguration ::
  TemplateConfiguration
newTemplateConfiguration =
  TemplateConfiguration'
    { emailTemplate =
        Prelude.Nothing,
      pushTemplate = Prelude.Nothing,
      sMSTemplate = Prelude.Nothing,
      voiceTemplate = Prelude.Nothing
    }

-- | The email template to use for the message.
templateConfiguration_emailTemplate :: Lens.Lens' TemplateConfiguration (Prelude.Maybe Template)
templateConfiguration_emailTemplate = Lens.lens (\TemplateConfiguration' {emailTemplate} -> emailTemplate) (\s@TemplateConfiguration' {} a -> s {emailTemplate = a} :: TemplateConfiguration)

-- | The push notification template to use for the message.
templateConfiguration_pushTemplate :: Lens.Lens' TemplateConfiguration (Prelude.Maybe Template)
templateConfiguration_pushTemplate = Lens.lens (\TemplateConfiguration' {pushTemplate} -> pushTemplate) (\s@TemplateConfiguration' {} a -> s {pushTemplate = a} :: TemplateConfiguration)

-- | The SMS template to use for the message.
templateConfiguration_sMSTemplate :: Lens.Lens' TemplateConfiguration (Prelude.Maybe Template)
templateConfiguration_sMSTemplate = Lens.lens (\TemplateConfiguration' {sMSTemplate} -> sMSTemplate) (\s@TemplateConfiguration' {} a -> s {sMSTemplate = a} :: TemplateConfiguration)

-- | The voice template to use for the message. This object isn\'t supported
-- for campaigns.
templateConfiguration_voiceTemplate :: Lens.Lens' TemplateConfiguration (Prelude.Maybe Template)
templateConfiguration_voiceTemplate = Lens.lens (\TemplateConfiguration' {voiceTemplate} -> voiceTemplate) (\s@TemplateConfiguration' {} a -> s {voiceTemplate = a} :: TemplateConfiguration)

instance Data.FromJSON TemplateConfiguration where
  parseJSON =
    Data.withObject
      "TemplateConfiguration"
      ( \x ->
          TemplateConfiguration'
            Prelude.<$> (x Data..:? "EmailTemplate")
            Prelude.<*> (x Data..:? "PushTemplate")
            Prelude.<*> (x Data..:? "SMSTemplate")
            Prelude.<*> (x Data..:? "VoiceTemplate")
      )

instance Prelude.Hashable TemplateConfiguration where
  hashWithSalt _salt TemplateConfiguration' {..} =
    _salt
      `Prelude.hashWithSalt` emailTemplate
      `Prelude.hashWithSalt` pushTemplate
      `Prelude.hashWithSalt` sMSTemplate
      `Prelude.hashWithSalt` voiceTemplate

instance Prelude.NFData TemplateConfiguration where
  rnf TemplateConfiguration' {..} =
    Prelude.rnf emailTemplate `Prelude.seq`
      Prelude.rnf pushTemplate `Prelude.seq`
        Prelude.rnf sMSTemplate `Prelude.seq`
          Prelude.rnf voiceTemplate

instance Data.ToJSON TemplateConfiguration where
  toJSON TemplateConfiguration' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EmailTemplate" Data..=) Prelude.<$> emailTemplate,
            ("PushTemplate" Data..=) Prelude.<$> pushTemplate,
            ("SMSTemplate" Data..=) Prelude.<$> sMSTemplate,
            ("VoiceTemplate" Data..=) Prelude.<$> voiceTemplate
          ]
      )
