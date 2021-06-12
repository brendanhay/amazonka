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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.Template

-- | Specifies the message template to use for the message, for each type of
-- channel.
--
-- /See:/ 'newTemplateConfiguration' smart constructor.
data TemplateConfiguration = TemplateConfiguration'
  { -- | The email template to use for the message.
    emailTemplate :: Core.Maybe Template,
    -- | The voice template to use for the message. This object isn\'t supported
    -- for campaigns.
    voiceTemplate :: Core.Maybe Template,
    -- | The SMS template to use for the message.
    sMSTemplate :: Core.Maybe Template,
    -- | The push notification template to use for the message.
    pushTemplate :: Core.Maybe Template
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
        Core.Nothing,
      voiceTemplate = Core.Nothing,
      sMSTemplate = Core.Nothing,
      pushTemplate = Core.Nothing
    }

-- | The email template to use for the message.
templateConfiguration_emailTemplate :: Lens.Lens' TemplateConfiguration (Core.Maybe Template)
templateConfiguration_emailTemplate = Lens.lens (\TemplateConfiguration' {emailTemplate} -> emailTemplate) (\s@TemplateConfiguration' {} a -> s {emailTemplate = a} :: TemplateConfiguration)

-- | The voice template to use for the message. This object isn\'t supported
-- for campaigns.
templateConfiguration_voiceTemplate :: Lens.Lens' TemplateConfiguration (Core.Maybe Template)
templateConfiguration_voiceTemplate = Lens.lens (\TemplateConfiguration' {voiceTemplate} -> voiceTemplate) (\s@TemplateConfiguration' {} a -> s {voiceTemplate = a} :: TemplateConfiguration)

-- | The SMS template to use for the message.
templateConfiguration_sMSTemplate :: Lens.Lens' TemplateConfiguration (Core.Maybe Template)
templateConfiguration_sMSTemplate = Lens.lens (\TemplateConfiguration' {sMSTemplate} -> sMSTemplate) (\s@TemplateConfiguration' {} a -> s {sMSTemplate = a} :: TemplateConfiguration)

-- | The push notification template to use for the message.
templateConfiguration_pushTemplate :: Lens.Lens' TemplateConfiguration (Core.Maybe Template)
templateConfiguration_pushTemplate = Lens.lens (\TemplateConfiguration' {pushTemplate} -> pushTemplate) (\s@TemplateConfiguration' {} a -> s {pushTemplate = a} :: TemplateConfiguration)

instance Core.FromJSON TemplateConfiguration where
  parseJSON =
    Core.withObject
      "TemplateConfiguration"
      ( \x ->
          TemplateConfiguration'
            Core.<$> (x Core..:? "EmailTemplate")
            Core.<*> (x Core..:? "VoiceTemplate")
            Core.<*> (x Core..:? "SMSTemplate")
            Core.<*> (x Core..:? "PushTemplate")
      )

instance Core.Hashable TemplateConfiguration

instance Core.NFData TemplateConfiguration

instance Core.ToJSON TemplateConfiguration where
  toJSON TemplateConfiguration' {..} =
    Core.object
      ( Core.catMaybes
          [ ("EmailTemplate" Core..=) Core.<$> emailTemplate,
            ("VoiceTemplate" Core..=) Core.<$> voiceTemplate,
            ("SMSTemplate" Core..=) Core.<$> sMSTemplate,
            ("PushTemplate" Core..=) Core.<$> pushTemplate
          ]
      )
