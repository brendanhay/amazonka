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
-- Module      : Network.AWS.Pinpoint.Types.SMSTemplateRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.SMSTemplateRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Specifies the content and settings for a message template that can be
-- used in text messages that are sent through the SMS channel.
--
-- /See:/ 'newSMSTemplateRequest' smart constructor.
data SMSTemplateRequest = SMSTemplateRequest'
  { -- | A custom description of the message template.
    templateDescription :: Core.Maybe Core.Text,
    -- | The message body to use in text messages that are based on the message
    -- template.
    body :: Core.Maybe Core.Text,
    -- | A JSON object that specifies the default values to use for message
    -- variables in the message template. This object is a set of key-value
    -- pairs. Each key defines a message variable in the template. The
    -- corresponding value defines the default value for that variable. When
    -- you create a message that\'s based on the template, you can override
    -- these defaults with message-specific and address-specific variables and
    -- values.
    defaultSubstitutions :: Core.Maybe Core.Text,
    -- | A string-to-string map of key-value pairs that defines the tags to
    -- associate with the message template. Each tag consists of a required tag
    -- key and an associated tag value.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The unique identifier for the recommender model to use for the message
    -- template. Amazon Pinpoint uses this value to determine how to retrieve
    -- and process data from a recommender model when it sends messages that
    -- use the template, if the template contains message variables for
    -- recommendation data.
    recommenderId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'SMSTemplateRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateDescription', 'sMSTemplateRequest_templateDescription' - A custom description of the message template.
--
-- 'body', 'sMSTemplateRequest_body' - The message body to use in text messages that are based on the message
-- template.
--
-- 'defaultSubstitutions', 'sMSTemplateRequest_defaultSubstitutions' - A JSON object that specifies the default values to use for message
-- variables in the message template. This object is a set of key-value
-- pairs. Each key defines a message variable in the template. The
-- corresponding value defines the default value for that variable. When
-- you create a message that\'s based on the template, you can override
-- these defaults with message-specific and address-specific variables and
-- values.
--
-- 'tags', 'sMSTemplateRequest_tags' - A string-to-string map of key-value pairs that defines the tags to
-- associate with the message template. Each tag consists of a required tag
-- key and an associated tag value.
--
-- 'recommenderId', 'sMSTemplateRequest_recommenderId' - The unique identifier for the recommender model to use for the message
-- template. Amazon Pinpoint uses this value to determine how to retrieve
-- and process data from a recommender model when it sends messages that
-- use the template, if the template contains message variables for
-- recommendation data.
newSMSTemplateRequest ::
  SMSTemplateRequest
newSMSTemplateRequest =
  SMSTemplateRequest'
    { templateDescription =
        Core.Nothing,
      body = Core.Nothing,
      defaultSubstitutions = Core.Nothing,
      tags = Core.Nothing,
      recommenderId = Core.Nothing
    }

-- | A custom description of the message template.
sMSTemplateRequest_templateDescription :: Lens.Lens' SMSTemplateRequest (Core.Maybe Core.Text)
sMSTemplateRequest_templateDescription = Lens.lens (\SMSTemplateRequest' {templateDescription} -> templateDescription) (\s@SMSTemplateRequest' {} a -> s {templateDescription = a} :: SMSTemplateRequest)

-- | The message body to use in text messages that are based on the message
-- template.
sMSTemplateRequest_body :: Lens.Lens' SMSTemplateRequest (Core.Maybe Core.Text)
sMSTemplateRequest_body = Lens.lens (\SMSTemplateRequest' {body} -> body) (\s@SMSTemplateRequest' {} a -> s {body = a} :: SMSTemplateRequest)

-- | A JSON object that specifies the default values to use for message
-- variables in the message template. This object is a set of key-value
-- pairs. Each key defines a message variable in the template. The
-- corresponding value defines the default value for that variable. When
-- you create a message that\'s based on the template, you can override
-- these defaults with message-specific and address-specific variables and
-- values.
sMSTemplateRequest_defaultSubstitutions :: Lens.Lens' SMSTemplateRequest (Core.Maybe Core.Text)
sMSTemplateRequest_defaultSubstitutions = Lens.lens (\SMSTemplateRequest' {defaultSubstitutions} -> defaultSubstitutions) (\s@SMSTemplateRequest' {} a -> s {defaultSubstitutions = a} :: SMSTemplateRequest)

-- | A string-to-string map of key-value pairs that defines the tags to
-- associate with the message template. Each tag consists of a required tag
-- key and an associated tag value.
sMSTemplateRequest_tags :: Lens.Lens' SMSTemplateRequest (Core.Maybe (Core.HashMap Core.Text Core.Text))
sMSTemplateRequest_tags = Lens.lens (\SMSTemplateRequest' {tags} -> tags) (\s@SMSTemplateRequest' {} a -> s {tags = a} :: SMSTemplateRequest) Core.. Lens.mapping Lens._Coerce

-- | The unique identifier for the recommender model to use for the message
-- template. Amazon Pinpoint uses this value to determine how to retrieve
-- and process data from a recommender model when it sends messages that
-- use the template, if the template contains message variables for
-- recommendation data.
sMSTemplateRequest_recommenderId :: Lens.Lens' SMSTemplateRequest (Core.Maybe Core.Text)
sMSTemplateRequest_recommenderId = Lens.lens (\SMSTemplateRequest' {recommenderId} -> recommenderId) (\s@SMSTemplateRequest' {} a -> s {recommenderId = a} :: SMSTemplateRequest)

instance Core.Hashable SMSTemplateRequest

instance Core.NFData SMSTemplateRequest

instance Core.ToJSON SMSTemplateRequest where
  toJSON SMSTemplateRequest' {..} =
    Core.object
      ( Core.catMaybes
          [ ("TemplateDescription" Core..=)
              Core.<$> templateDescription,
            ("Body" Core..=) Core.<$> body,
            ("DefaultSubstitutions" Core..=)
              Core.<$> defaultSubstitutions,
            ("tags" Core..=) Core.<$> tags,
            ("RecommenderId" Core..=) Core.<$> recommenderId
          ]
      )
