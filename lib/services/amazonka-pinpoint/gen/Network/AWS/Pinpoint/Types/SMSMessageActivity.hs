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
-- Module      : Network.AWS.Pinpoint.Types.SMSMessageActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SMSMessageActivity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.JourneySMSMessage
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the settings for an SMS activity in a journey. This type of
-- activity sends a text message to participants.
--
-- /See:/ 'newSMSMessageActivity' smart constructor.
data SMSMessageActivity = SMSMessageActivity'
  { -- | The name of the SMS message template to use for the message. If
    -- specified, this value must match the name of an existing message
    -- template.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the sender ID and message type for an SMS message that\'s sent
    -- to participants in a journey.
    messageConfig :: Prelude.Maybe JourneySMSMessage,
    -- | The unique identifier for the version of the SMS template to use for the
    -- message. If specified, this value must match the identifier for an
    -- existing template version. To retrieve a list of versions and version
    -- identifiers for a template, use the Template Versions resource.
    --
    -- If you don\'t specify a value for this property, Amazon Pinpoint uses
    -- the /active version/ of the template. The /active version/ is typically
    -- the version of a template that\'s been most recently reviewed and
    -- approved for use, depending on your workflow. It isn\'t necessarily the
    -- latest version of a template.
    templateVersion :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the next activity to perform, after the
    -- message is sent.
    nextActivity :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SMSMessageActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'sMSMessageActivity_templateName' - The name of the SMS message template to use for the message. If
-- specified, this value must match the name of an existing message
-- template.
--
-- 'messageConfig', 'sMSMessageActivity_messageConfig' - Specifies the sender ID and message type for an SMS message that\'s sent
-- to participants in a journey.
--
-- 'templateVersion', 'sMSMessageActivity_templateVersion' - The unique identifier for the version of the SMS template to use for the
-- message. If specified, this value must match the identifier for an
-- existing template version. To retrieve a list of versions and version
-- identifiers for a template, use the Template Versions resource.
--
-- If you don\'t specify a value for this property, Amazon Pinpoint uses
-- the /active version/ of the template. The /active version/ is typically
-- the version of a template that\'s been most recently reviewed and
-- approved for use, depending on your workflow. It isn\'t necessarily the
-- latest version of a template.
--
-- 'nextActivity', 'sMSMessageActivity_nextActivity' - The unique identifier for the next activity to perform, after the
-- message is sent.
newSMSMessageActivity ::
  SMSMessageActivity
newSMSMessageActivity =
  SMSMessageActivity'
    { templateName = Prelude.Nothing,
      messageConfig = Prelude.Nothing,
      templateVersion = Prelude.Nothing,
      nextActivity = Prelude.Nothing
    }

-- | The name of the SMS message template to use for the message. If
-- specified, this value must match the name of an existing message
-- template.
sMSMessageActivity_templateName :: Lens.Lens' SMSMessageActivity (Prelude.Maybe Prelude.Text)
sMSMessageActivity_templateName = Lens.lens (\SMSMessageActivity' {templateName} -> templateName) (\s@SMSMessageActivity' {} a -> s {templateName = a} :: SMSMessageActivity)

-- | Specifies the sender ID and message type for an SMS message that\'s sent
-- to participants in a journey.
sMSMessageActivity_messageConfig :: Lens.Lens' SMSMessageActivity (Prelude.Maybe JourneySMSMessage)
sMSMessageActivity_messageConfig = Lens.lens (\SMSMessageActivity' {messageConfig} -> messageConfig) (\s@SMSMessageActivity' {} a -> s {messageConfig = a} :: SMSMessageActivity)

-- | The unique identifier for the version of the SMS template to use for the
-- message. If specified, this value must match the identifier for an
-- existing template version. To retrieve a list of versions and version
-- identifiers for a template, use the Template Versions resource.
--
-- If you don\'t specify a value for this property, Amazon Pinpoint uses
-- the /active version/ of the template. The /active version/ is typically
-- the version of a template that\'s been most recently reviewed and
-- approved for use, depending on your workflow. It isn\'t necessarily the
-- latest version of a template.
sMSMessageActivity_templateVersion :: Lens.Lens' SMSMessageActivity (Prelude.Maybe Prelude.Text)
sMSMessageActivity_templateVersion = Lens.lens (\SMSMessageActivity' {templateVersion} -> templateVersion) (\s@SMSMessageActivity' {} a -> s {templateVersion = a} :: SMSMessageActivity)

-- | The unique identifier for the next activity to perform, after the
-- message is sent.
sMSMessageActivity_nextActivity :: Lens.Lens' SMSMessageActivity (Prelude.Maybe Prelude.Text)
sMSMessageActivity_nextActivity = Lens.lens (\SMSMessageActivity' {nextActivity} -> nextActivity) (\s@SMSMessageActivity' {} a -> s {nextActivity = a} :: SMSMessageActivity)

instance Core.FromJSON SMSMessageActivity where
  parseJSON =
    Core.withObject
      "SMSMessageActivity"
      ( \x ->
          SMSMessageActivity'
            Prelude.<$> (x Core..:? "TemplateName")
            Prelude.<*> (x Core..:? "MessageConfig")
            Prelude.<*> (x Core..:? "TemplateVersion")
            Prelude.<*> (x Core..:? "NextActivity")
      )

instance Prelude.Hashable SMSMessageActivity

instance Prelude.NFData SMSMessageActivity

instance Core.ToJSON SMSMessageActivity where
  toJSON SMSMessageActivity' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TemplateName" Core..=) Prelude.<$> templateName,
            ("MessageConfig" Core..=) Prelude.<$> messageConfig,
            ("TemplateVersion" Core..=)
              Prelude.<$> templateVersion,
            ("NextActivity" Core..=) Prelude.<$> nextActivity
          ]
      )
