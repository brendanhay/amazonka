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
-- Module      : Network.AWS.Pinpoint.Types.EmailMessageActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EmailMessageActivity where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.JourneyEmailMessage

-- | Specifies the settings for an email activity in a journey. This type of
-- activity sends an email message to participants.
--
-- /See:/ 'newEmailMessageActivity' smart constructor.
data EmailMessageActivity = EmailMessageActivity'
  { -- | The name of the email message template to use for the message. If
    -- specified, this value must match the name of an existing message
    -- template.
    templateName :: Core.Maybe Core.Text,
    -- | Specifies the sender address for an email message that\'s sent to
    -- participants in the journey.
    messageConfig :: Core.Maybe JourneyEmailMessage,
    -- | The unique identifier for the next activity to perform, after the
    -- message is sent.
    nextActivity :: Core.Maybe Core.Text,
    -- | The unique identifier for the version of the email template to use for
    -- the message. If specified, this value must match the identifier for an
    -- existing template version. To retrieve a list of versions and version
    -- identifiers for a template, use the Template Versions resource.
    --
    -- If you don\'t specify a value for this property, Amazon Pinpoint uses
    -- the /active version/ of the template. The /active version/ is typically
    -- the version of a template that\'s been most recently reviewed and
    -- approved for use, depending on your workflow. It isn\'t necessarily the
    -- latest version of a template.
    templateVersion :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'EmailMessageActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'emailMessageActivity_templateName' - The name of the email message template to use for the message. If
-- specified, this value must match the name of an existing message
-- template.
--
-- 'messageConfig', 'emailMessageActivity_messageConfig' - Specifies the sender address for an email message that\'s sent to
-- participants in the journey.
--
-- 'nextActivity', 'emailMessageActivity_nextActivity' - The unique identifier for the next activity to perform, after the
-- message is sent.
--
-- 'templateVersion', 'emailMessageActivity_templateVersion' - The unique identifier for the version of the email template to use for
-- the message. If specified, this value must match the identifier for an
-- existing template version. To retrieve a list of versions and version
-- identifiers for a template, use the Template Versions resource.
--
-- If you don\'t specify a value for this property, Amazon Pinpoint uses
-- the /active version/ of the template. The /active version/ is typically
-- the version of a template that\'s been most recently reviewed and
-- approved for use, depending on your workflow. It isn\'t necessarily the
-- latest version of a template.
newEmailMessageActivity ::
  EmailMessageActivity
newEmailMessageActivity =
  EmailMessageActivity'
    { templateName = Core.Nothing,
      messageConfig = Core.Nothing,
      nextActivity = Core.Nothing,
      templateVersion = Core.Nothing
    }

-- | The name of the email message template to use for the message. If
-- specified, this value must match the name of an existing message
-- template.
emailMessageActivity_templateName :: Lens.Lens' EmailMessageActivity (Core.Maybe Core.Text)
emailMessageActivity_templateName = Lens.lens (\EmailMessageActivity' {templateName} -> templateName) (\s@EmailMessageActivity' {} a -> s {templateName = a} :: EmailMessageActivity)

-- | Specifies the sender address for an email message that\'s sent to
-- participants in the journey.
emailMessageActivity_messageConfig :: Lens.Lens' EmailMessageActivity (Core.Maybe JourneyEmailMessage)
emailMessageActivity_messageConfig = Lens.lens (\EmailMessageActivity' {messageConfig} -> messageConfig) (\s@EmailMessageActivity' {} a -> s {messageConfig = a} :: EmailMessageActivity)

-- | The unique identifier for the next activity to perform, after the
-- message is sent.
emailMessageActivity_nextActivity :: Lens.Lens' EmailMessageActivity (Core.Maybe Core.Text)
emailMessageActivity_nextActivity = Lens.lens (\EmailMessageActivity' {nextActivity} -> nextActivity) (\s@EmailMessageActivity' {} a -> s {nextActivity = a} :: EmailMessageActivity)

-- | The unique identifier for the version of the email template to use for
-- the message. If specified, this value must match the identifier for an
-- existing template version. To retrieve a list of versions and version
-- identifiers for a template, use the Template Versions resource.
--
-- If you don\'t specify a value for this property, Amazon Pinpoint uses
-- the /active version/ of the template. The /active version/ is typically
-- the version of a template that\'s been most recently reviewed and
-- approved for use, depending on your workflow. It isn\'t necessarily the
-- latest version of a template.
emailMessageActivity_templateVersion :: Lens.Lens' EmailMessageActivity (Core.Maybe Core.Text)
emailMessageActivity_templateVersion = Lens.lens (\EmailMessageActivity' {templateVersion} -> templateVersion) (\s@EmailMessageActivity' {} a -> s {templateVersion = a} :: EmailMessageActivity)

instance Core.FromJSON EmailMessageActivity where
  parseJSON =
    Core.withObject
      "EmailMessageActivity"
      ( \x ->
          EmailMessageActivity'
            Core.<$> (x Core..:? "TemplateName")
            Core.<*> (x Core..:? "MessageConfig")
            Core.<*> (x Core..:? "NextActivity")
            Core.<*> (x Core..:? "TemplateVersion")
      )

instance Core.Hashable EmailMessageActivity

instance Core.NFData EmailMessageActivity

instance Core.ToJSON EmailMessageActivity where
  toJSON EmailMessageActivity' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TemplateName" Core..=) Core.<$> templateName,
            ("MessageConfig" Core..=) Core.<$> messageConfig,
            ("NextActivity" Core..=) Core.<$> nextActivity,
            ("TemplateVersion" Core..=)
              Core.<$> templateVersion
          ]
      )
