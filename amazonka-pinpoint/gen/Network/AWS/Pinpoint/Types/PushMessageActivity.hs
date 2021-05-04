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
-- Module      : Network.AWS.Pinpoint.Types.PushMessageActivity
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.PushMessageActivity where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.JourneyPushMessage
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the settings for a push notification activity in a journey.
-- This type of activity sends a push notification to participants.
--
-- /See:/ 'newPushMessageActivity' smart constructor.
data PushMessageActivity = PushMessageActivity'
  { -- | The name of the push notification template to use for the message. If
    -- specified, this value must match the name of an existing message
    -- template.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | Specifies the time to live (TTL) value for push notifications that are
    -- sent to participants in a journey.
    messageConfig :: Prelude.Maybe JourneyPushMessage,
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
    templateVersion :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'messageConfig', 'pushMessageActivity_messageConfig' - Specifies the time to live (TTL) value for push notifications that are
-- sent to participants in a journey.
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
newPushMessageActivity ::
  PushMessageActivity
newPushMessageActivity =
  PushMessageActivity'
    { templateName =
        Prelude.Nothing,
      messageConfig = Prelude.Nothing,
      nextActivity = Prelude.Nothing,
      templateVersion = Prelude.Nothing
    }

-- | The name of the push notification template to use for the message. If
-- specified, this value must match the name of an existing message
-- template.
pushMessageActivity_templateName :: Lens.Lens' PushMessageActivity (Prelude.Maybe Prelude.Text)
pushMessageActivity_templateName = Lens.lens (\PushMessageActivity' {templateName} -> templateName) (\s@PushMessageActivity' {} a -> s {templateName = a} :: PushMessageActivity)

-- | Specifies the time to live (TTL) value for push notifications that are
-- sent to participants in a journey.
pushMessageActivity_messageConfig :: Lens.Lens' PushMessageActivity (Prelude.Maybe JourneyPushMessage)
pushMessageActivity_messageConfig = Lens.lens (\PushMessageActivity' {messageConfig} -> messageConfig) (\s@PushMessageActivity' {} a -> s {messageConfig = a} :: PushMessageActivity)

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

instance Prelude.FromJSON PushMessageActivity where
  parseJSON =
    Prelude.withObject
      "PushMessageActivity"
      ( \x ->
          PushMessageActivity'
            Prelude.<$> (x Prelude..:? "TemplateName")
            Prelude.<*> (x Prelude..:? "MessageConfig")
            Prelude.<*> (x Prelude..:? "NextActivity")
            Prelude.<*> (x Prelude..:? "TemplateVersion")
      )

instance Prelude.Hashable PushMessageActivity

instance Prelude.NFData PushMessageActivity

instance Prelude.ToJSON PushMessageActivity where
  toJSON PushMessageActivity' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("TemplateName" Prelude..=)
              Prelude.<$> templateName,
            ("MessageConfig" Prelude..=)
              Prelude.<$> messageConfig,
            ("NextActivity" Prelude..=) Prelude.<$> nextActivity,
            ("TemplateVersion" Prelude..=)
              Prelude.<$> templateVersion
          ]
      )
