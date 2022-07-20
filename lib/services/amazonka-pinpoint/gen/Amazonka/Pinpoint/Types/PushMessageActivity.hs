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
-- Module      : Amazonka.Pinpoint.Types.PushMessageActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.PushMessageActivity where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.Pinpoint.Types.JourneyPushMessage
import qualified Amazonka.Prelude as Prelude

-- | Specifies the settings for a push notification activity in a journey.
-- This type of activity sends a push notification to participants.
--
-- /See:/ 'newPushMessageActivity' smart constructor.
data PushMessageActivity = PushMessageActivity'
  { -- | The name of the push notification template to use for the message. If
    -- specified, this value must match the name of an existing message
    -- template.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the next activity to perform, after the
    -- message is sent.
    nextActivity :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the version of the push notification template
    -- to use for the message. If specified, this value must match the
    -- identifier for an existing template version. To retrieve a list of
    -- versions and version identifiers for a template, use the Template
    -- Versions resource.
    --
    -- If you don\'t specify a value for this property, Amazon Pinpoint uses
    -- the /active version/ of the template. The /active version/ is typically
    -- the version of a template that\'s been most recently reviewed and
    -- approved for use, depending on your workflow. It isn\'t necessarily the
    -- latest version of a template.
    templateVersion :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time to live (TTL) value for push notifications that are
    -- sent to participants in a journey.
    messageConfig :: Prelude.Maybe JourneyPushMessage
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PushMessageActivity' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'pushMessageActivity_templateName' - The name of the push notification template to use for the message. If
-- specified, this value must match the name of an existing message
-- template.
--
-- 'nextActivity', 'pushMessageActivity_nextActivity' - The unique identifier for the next activity to perform, after the
-- message is sent.
--
-- 'templateVersion', 'pushMessageActivity_templateVersion' - The unique identifier for the version of the push notification template
-- to use for the message. If specified, this value must match the
-- identifier for an existing template version. To retrieve a list of
-- versions and version identifiers for a template, use the Template
-- Versions resource.
--
-- If you don\'t specify a value for this property, Amazon Pinpoint uses
-- the /active version/ of the template. The /active version/ is typically
-- the version of a template that\'s been most recently reviewed and
-- approved for use, depending on your workflow. It isn\'t necessarily the
-- latest version of a template.
--
-- 'messageConfig', 'pushMessageActivity_messageConfig' - Specifies the time to live (TTL) value for push notifications that are
-- sent to participants in a journey.
newPushMessageActivity ::
  PushMessageActivity
newPushMessageActivity =
  PushMessageActivity'
    { templateName =
        Prelude.Nothing,
      nextActivity = Prelude.Nothing,
      templateVersion = Prelude.Nothing,
      messageConfig = Prelude.Nothing
    }

-- | The name of the push notification template to use for the message. If
-- specified, this value must match the name of an existing message
-- template.
pushMessageActivity_templateName :: Lens.Lens' PushMessageActivity (Prelude.Maybe Prelude.Text)
pushMessageActivity_templateName = Lens.lens (\PushMessageActivity' {templateName} -> templateName) (\s@PushMessageActivity' {} a -> s {templateName = a} :: PushMessageActivity)

-- | The unique identifier for the next activity to perform, after the
-- message is sent.
pushMessageActivity_nextActivity :: Lens.Lens' PushMessageActivity (Prelude.Maybe Prelude.Text)
pushMessageActivity_nextActivity = Lens.lens (\PushMessageActivity' {nextActivity} -> nextActivity) (\s@PushMessageActivity' {} a -> s {nextActivity = a} :: PushMessageActivity)

-- | The unique identifier for the version of the push notification template
-- to use for the message. If specified, this value must match the
-- identifier for an existing template version. To retrieve a list of
-- versions and version identifiers for a template, use the Template
-- Versions resource.
--
-- If you don\'t specify a value for this property, Amazon Pinpoint uses
-- the /active version/ of the template. The /active version/ is typically
-- the version of a template that\'s been most recently reviewed and
-- approved for use, depending on your workflow. It isn\'t necessarily the
-- latest version of a template.
pushMessageActivity_templateVersion :: Lens.Lens' PushMessageActivity (Prelude.Maybe Prelude.Text)
pushMessageActivity_templateVersion = Lens.lens (\PushMessageActivity' {templateVersion} -> templateVersion) (\s@PushMessageActivity' {} a -> s {templateVersion = a} :: PushMessageActivity)

-- | Specifies the time to live (TTL) value for push notifications that are
-- sent to participants in a journey.
pushMessageActivity_messageConfig :: Lens.Lens' PushMessageActivity (Prelude.Maybe JourneyPushMessage)
pushMessageActivity_messageConfig = Lens.lens (\PushMessageActivity' {messageConfig} -> messageConfig) (\s@PushMessageActivity' {} a -> s {messageConfig = a} :: PushMessageActivity)

instance Core.FromJSON PushMessageActivity where
  parseJSON =
    Core.withObject
      "PushMessageActivity"
      ( \x ->
          PushMessageActivity'
            Prelude.<$> (x Core..:? "TemplateName")
            Prelude.<*> (x Core..:? "NextActivity")
            Prelude.<*> (x Core..:? "TemplateVersion")
            Prelude.<*> (x Core..:? "MessageConfig")
      )

instance Prelude.Hashable PushMessageActivity where
  hashWithSalt _salt PushMessageActivity' {..} =
    _salt `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` nextActivity
      `Prelude.hashWithSalt` templateVersion
      `Prelude.hashWithSalt` messageConfig

instance Prelude.NFData PushMessageActivity where
  rnf PushMessageActivity' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf nextActivity
      `Prelude.seq` Prelude.rnf templateVersion
      `Prelude.seq` Prelude.rnf messageConfig

instance Core.ToJSON PushMessageActivity where
  toJSON PushMessageActivity' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TemplateName" Core..=) Prelude.<$> templateName,
            ("NextActivity" Core..=) Prelude.<$> nextActivity,
            ("TemplateVersion" Core..=)
              Prelude.<$> templateVersion,
            ("MessageConfig" Core..=) Prelude.<$> messageConfig
          ]
      )
