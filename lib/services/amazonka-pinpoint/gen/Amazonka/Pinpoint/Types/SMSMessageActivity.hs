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
-- Module      : Amazonka.Pinpoint.Types.SMSMessageActivity
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.SMSMessageActivity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.JourneySMSMessage
import qualified Amazonka.Prelude as Prelude

-- | Specifies the settings for an SMS activity in a journey. This type of
-- activity sends a text message to participants.
--
-- /See:/ 'newSMSMessageActivity' smart constructor.
data SMSMessageActivity = SMSMessageActivity'
  { -- | Specifies the sender ID and message type for an SMS message that\'s sent
    -- to participants in a journey.
    messageConfig :: Prelude.Maybe JourneySMSMessage,
    -- | The unique identifier for the next activity to perform, after the
    -- message is sent.
    nextActivity :: Prelude.Maybe Prelude.Text,
    -- | The name of the SMS message template to use for the message. If
    -- specified, this value must match the name of an existing message
    -- template.
    templateName :: Prelude.Maybe Prelude.Text,
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
    templateVersion :: Prelude.Maybe Prelude.Text
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
-- 'messageConfig', 'sMSMessageActivity_messageConfig' - Specifies the sender ID and message type for an SMS message that\'s sent
-- to participants in a journey.
--
-- 'nextActivity', 'sMSMessageActivity_nextActivity' - The unique identifier for the next activity to perform, after the
-- message is sent.
--
-- 'templateName', 'sMSMessageActivity_templateName' - The name of the SMS message template to use for the message. If
-- specified, this value must match the name of an existing message
-- template.
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
newSMSMessageActivity ::
  SMSMessageActivity
newSMSMessageActivity =
  SMSMessageActivity'
    { messageConfig =
        Prelude.Nothing,
      nextActivity = Prelude.Nothing,
      templateName = Prelude.Nothing,
      templateVersion = Prelude.Nothing
    }

-- | Specifies the sender ID and message type for an SMS message that\'s sent
-- to participants in a journey.
sMSMessageActivity_messageConfig :: Lens.Lens' SMSMessageActivity (Prelude.Maybe JourneySMSMessage)
sMSMessageActivity_messageConfig = Lens.lens (\SMSMessageActivity' {messageConfig} -> messageConfig) (\s@SMSMessageActivity' {} a -> s {messageConfig = a} :: SMSMessageActivity)

-- | The unique identifier for the next activity to perform, after the
-- message is sent.
sMSMessageActivity_nextActivity :: Lens.Lens' SMSMessageActivity (Prelude.Maybe Prelude.Text)
sMSMessageActivity_nextActivity = Lens.lens (\SMSMessageActivity' {nextActivity} -> nextActivity) (\s@SMSMessageActivity' {} a -> s {nextActivity = a} :: SMSMessageActivity)

-- | The name of the SMS message template to use for the message. If
-- specified, this value must match the name of an existing message
-- template.
sMSMessageActivity_templateName :: Lens.Lens' SMSMessageActivity (Prelude.Maybe Prelude.Text)
sMSMessageActivity_templateName = Lens.lens (\SMSMessageActivity' {templateName} -> templateName) (\s@SMSMessageActivity' {} a -> s {templateName = a} :: SMSMessageActivity)

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

instance Data.FromJSON SMSMessageActivity where
  parseJSON =
    Data.withObject
      "SMSMessageActivity"
      ( \x ->
          SMSMessageActivity'
            Prelude.<$> (x Data..:? "MessageConfig")
            Prelude.<*> (x Data..:? "NextActivity")
            Prelude.<*> (x Data..:? "TemplateName")
            Prelude.<*> (x Data..:? "TemplateVersion")
      )

instance Prelude.Hashable SMSMessageActivity where
  hashWithSalt _salt SMSMessageActivity' {..} =
    _salt
      `Prelude.hashWithSalt` messageConfig
      `Prelude.hashWithSalt` nextActivity
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateVersion

instance Prelude.NFData SMSMessageActivity where
  rnf SMSMessageActivity' {..} =
    Prelude.rnf messageConfig
      `Prelude.seq` Prelude.rnf nextActivity
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateVersion

instance Data.ToJSON SMSMessageActivity where
  toJSON SMSMessageActivity' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MessageConfig" Data..=) Prelude.<$> messageConfig,
            ("NextActivity" Data..=) Prelude.<$> nextActivity,
            ("TemplateName" Data..=) Prelude.<$> templateName,
            ("TemplateVersion" Data..=)
              Prelude.<$> templateVersion
          ]
      )
