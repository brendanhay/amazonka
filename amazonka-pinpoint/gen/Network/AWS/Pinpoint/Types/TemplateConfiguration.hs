{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.Pinpoint.Types.TemplateConfiguration
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TemplateConfiguration where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Template
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the message template to use for the message, for each type of
-- channel.
--
-- /See:/ 'newTemplateConfiguration' smart constructor.
data TemplateConfiguration = TemplateConfiguration'
  { -- | The email template to use for the message.
    emailTemplate :: Prelude.Maybe Template,
    -- | The voice template to use for the message. This object isn\'t supported
    -- for campaigns.
    voiceTemplate :: Prelude.Maybe Template,
    -- | The SMS template to use for the message.
    sMSTemplate :: Prelude.Maybe Template,
    -- | The push notification template to use for the message.
    pushTemplate :: Prelude.Maybe Template
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'voiceTemplate', 'templateConfiguration_voiceTemplate' - The voice template to use for the message. This object isn\'t supported
-- for campaigns.
--
-- 'sMSTemplate', 'templateConfiguration_sMSTemplate' - The SMS template to use for the message.
--
-- 'pushTemplate', 'templateConfiguration_pushTemplate' - The push notification template to use for the message.
newTemplateConfiguration ::
  TemplateConfiguration
newTemplateConfiguration =
  TemplateConfiguration'
    { emailTemplate =
        Prelude.Nothing,
      voiceTemplate = Prelude.Nothing,
      sMSTemplate = Prelude.Nothing,
      pushTemplate = Prelude.Nothing
    }

-- | The email template to use for the message.
templateConfiguration_emailTemplate :: Lens.Lens' TemplateConfiguration (Prelude.Maybe Template)
templateConfiguration_emailTemplate = Lens.lens (\TemplateConfiguration' {emailTemplate} -> emailTemplate) (\s@TemplateConfiguration' {} a -> s {emailTemplate = a} :: TemplateConfiguration)

-- | The voice template to use for the message. This object isn\'t supported
-- for campaigns.
templateConfiguration_voiceTemplate :: Lens.Lens' TemplateConfiguration (Prelude.Maybe Template)
templateConfiguration_voiceTemplate = Lens.lens (\TemplateConfiguration' {voiceTemplate} -> voiceTemplate) (\s@TemplateConfiguration' {} a -> s {voiceTemplate = a} :: TemplateConfiguration)

-- | The SMS template to use for the message.
templateConfiguration_sMSTemplate :: Lens.Lens' TemplateConfiguration (Prelude.Maybe Template)
templateConfiguration_sMSTemplate = Lens.lens (\TemplateConfiguration' {sMSTemplate} -> sMSTemplate) (\s@TemplateConfiguration' {} a -> s {sMSTemplate = a} :: TemplateConfiguration)

-- | The push notification template to use for the message.
templateConfiguration_pushTemplate :: Lens.Lens' TemplateConfiguration (Prelude.Maybe Template)
templateConfiguration_pushTemplate = Lens.lens (\TemplateConfiguration' {pushTemplate} -> pushTemplate) (\s@TemplateConfiguration' {} a -> s {pushTemplate = a} :: TemplateConfiguration)

instance Prelude.FromJSON TemplateConfiguration where
  parseJSON =
    Prelude.withObject
      "TemplateConfiguration"
      ( \x ->
          TemplateConfiguration'
            Prelude.<$> (x Prelude..:? "EmailTemplate")
            Prelude.<*> (x Prelude..:? "VoiceTemplate")
            Prelude.<*> (x Prelude..:? "SMSTemplate")
            Prelude.<*> (x Prelude..:? "PushTemplate")
      )

instance Prelude.Hashable TemplateConfiguration

instance Prelude.NFData TemplateConfiguration

instance Prelude.ToJSON TemplateConfiguration where
  toJSON TemplateConfiguration' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("EmailTemplate" Prelude..=)
              Prelude.<$> emailTemplate,
            ("VoiceTemplate" Prelude..=)
              Prelude.<$> voiceTemplate,
            ("SMSTemplate" Prelude..=) Prelude.<$> sMSTemplate,
            ("PushTemplate" Prelude..=)
              Prelude.<$> pushTemplate
          ]
      )
