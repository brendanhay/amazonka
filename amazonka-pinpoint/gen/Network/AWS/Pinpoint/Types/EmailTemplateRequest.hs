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
-- Module      : Network.AWS.Pinpoint.Types.EmailTemplateRequest
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.EmailTemplateRequest where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the content and settings for a message template that can be
-- used in messages that are sent through the email channel.
--
-- /See:/ 'newEmailTemplateRequest' smart constructor.
data EmailTemplateRequest = EmailTemplateRequest'
  { -- | A custom description of the message template.
    templateDescription :: Prelude.Maybe Prelude.Text,
    -- | The message body, in plain text format, to use in email messages that
    -- are based on the message template. We recommend using plain text format
    -- for email clients that don\'t render HTML content and clients that are
    -- connected to high-latency networks, such as mobile devices.
    textPart :: Prelude.Maybe Prelude.Text,
    -- | A JSON object that specifies the default values to use for message
    -- variables in the message template. This object is a set of key-value
    -- pairs. Each key defines a message variable in the template. The
    -- corresponding value defines the default value for that variable. When
    -- you create a message that\'s based on the template, you can override
    -- these defaults with message-specific and address-specific variables and
    -- values.
    defaultSubstitutions :: Prelude.Maybe Prelude.Text,
    -- | The subject line, or title, to use in email messages that are based on
    -- the message template.
    subject :: Prelude.Maybe Prelude.Text,
    -- | A string-to-string map of key-value pairs that defines the tags to
    -- associate with the message template. Each tag consists of a required tag
    -- key and an associated tag value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The unique identifier for the recommender model to use for the message
    -- template. Amazon Pinpoint uses this value to determine how to retrieve
    -- and process data from a recommender model when it sends messages that
    -- use the template, if the template contains message variables for
    -- recommendation data.
    recommenderId :: Prelude.Maybe Prelude.Text,
    -- | The message body, in HTML format, to use in email messages that are
    -- based on the message template. We recommend using HTML format for email
    -- clients that render HTML content. You can include links, formatted text,
    -- and more in an HTML message.
    htmlPart :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EmailTemplateRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateDescription', 'emailTemplateRequest_templateDescription' - A custom description of the message template.
--
-- 'textPart', 'emailTemplateRequest_textPart' - The message body, in plain text format, to use in email messages that
-- are based on the message template. We recommend using plain text format
-- for email clients that don\'t render HTML content and clients that are
-- connected to high-latency networks, such as mobile devices.
--
-- 'defaultSubstitutions', 'emailTemplateRequest_defaultSubstitutions' - A JSON object that specifies the default values to use for message
-- variables in the message template. This object is a set of key-value
-- pairs. Each key defines a message variable in the template. The
-- corresponding value defines the default value for that variable. When
-- you create a message that\'s based on the template, you can override
-- these defaults with message-specific and address-specific variables and
-- values.
--
-- 'subject', 'emailTemplateRequest_subject' - The subject line, or title, to use in email messages that are based on
-- the message template.
--
-- 'tags', 'emailTemplateRequest_tags' - A string-to-string map of key-value pairs that defines the tags to
-- associate with the message template. Each tag consists of a required tag
-- key and an associated tag value.
--
-- 'recommenderId', 'emailTemplateRequest_recommenderId' - The unique identifier for the recommender model to use for the message
-- template. Amazon Pinpoint uses this value to determine how to retrieve
-- and process data from a recommender model when it sends messages that
-- use the template, if the template contains message variables for
-- recommendation data.
--
-- 'htmlPart', 'emailTemplateRequest_htmlPart' - The message body, in HTML format, to use in email messages that are
-- based on the message template. We recommend using HTML format for email
-- clients that render HTML content. You can include links, formatted text,
-- and more in an HTML message.
newEmailTemplateRequest ::
  EmailTemplateRequest
newEmailTemplateRequest =
  EmailTemplateRequest'
    { templateDescription =
        Prelude.Nothing,
      textPart = Prelude.Nothing,
      defaultSubstitutions = Prelude.Nothing,
      subject = Prelude.Nothing,
      tags = Prelude.Nothing,
      recommenderId = Prelude.Nothing,
      htmlPart = Prelude.Nothing
    }

-- | A custom description of the message template.
emailTemplateRequest_templateDescription :: Lens.Lens' EmailTemplateRequest (Prelude.Maybe Prelude.Text)
emailTemplateRequest_templateDescription = Lens.lens (\EmailTemplateRequest' {templateDescription} -> templateDescription) (\s@EmailTemplateRequest' {} a -> s {templateDescription = a} :: EmailTemplateRequest)

-- | The message body, in plain text format, to use in email messages that
-- are based on the message template. We recommend using plain text format
-- for email clients that don\'t render HTML content and clients that are
-- connected to high-latency networks, such as mobile devices.
emailTemplateRequest_textPart :: Lens.Lens' EmailTemplateRequest (Prelude.Maybe Prelude.Text)
emailTemplateRequest_textPart = Lens.lens (\EmailTemplateRequest' {textPart} -> textPart) (\s@EmailTemplateRequest' {} a -> s {textPart = a} :: EmailTemplateRequest)

-- | A JSON object that specifies the default values to use for message
-- variables in the message template. This object is a set of key-value
-- pairs. Each key defines a message variable in the template. The
-- corresponding value defines the default value for that variable. When
-- you create a message that\'s based on the template, you can override
-- these defaults with message-specific and address-specific variables and
-- values.
emailTemplateRequest_defaultSubstitutions :: Lens.Lens' EmailTemplateRequest (Prelude.Maybe Prelude.Text)
emailTemplateRequest_defaultSubstitutions = Lens.lens (\EmailTemplateRequest' {defaultSubstitutions} -> defaultSubstitutions) (\s@EmailTemplateRequest' {} a -> s {defaultSubstitutions = a} :: EmailTemplateRequest)

-- | The subject line, or title, to use in email messages that are based on
-- the message template.
emailTemplateRequest_subject :: Lens.Lens' EmailTemplateRequest (Prelude.Maybe Prelude.Text)
emailTemplateRequest_subject = Lens.lens (\EmailTemplateRequest' {subject} -> subject) (\s@EmailTemplateRequest' {} a -> s {subject = a} :: EmailTemplateRequest)

-- | A string-to-string map of key-value pairs that defines the tags to
-- associate with the message template. Each tag consists of a required tag
-- key and an associated tag value.
emailTemplateRequest_tags :: Lens.Lens' EmailTemplateRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
emailTemplateRequest_tags = Lens.lens (\EmailTemplateRequest' {tags} -> tags) (\s@EmailTemplateRequest' {} a -> s {tags = a} :: EmailTemplateRequest) Prelude.. Lens.mapping Lens._Coerce

-- | The unique identifier for the recommender model to use for the message
-- template. Amazon Pinpoint uses this value to determine how to retrieve
-- and process data from a recommender model when it sends messages that
-- use the template, if the template contains message variables for
-- recommendation data.
emailTemplateRequest_recommenderId :: Lens.Lens' EmailTemplateRequest (Prelude.Maybe Prelude.Text)
emailTemplateRequest_recommenderId = Lens.lens (\EmailTemplateRequest' {recommenderId} -> recommenderId) (\s@EmailTemplateRequest' {} a -> s {recommenderId = a} :: EmailTemplateRequest)

-- | The message body, in HTML format, to use in email messages that are
-- based on the message template. We recommend using HTML format for email
-- clients that render HTML content. You can include links, formatted text,
-- and more in an HTML message.
emailTemplateRequest_htmlPart :: Lens.Lens' EmailTemplateRequest (Prelude.Maybe Prelude.Text)
emailTemplateRequest_htmlPart = Lens.lens (\EmailTemplateRequest' {htmlPart} -> htmlPart) (\s@EmailTemplateRequest' {} a -> s {htmlPart = a} :: EmailTemplateRequest)

instance Prelude.Hashable EmailTemplateRequest

instance Prelude.NFData EmailTemplateRequest

instance Core.ToJSON EmailTemplateRequest where
  toJSON EmailTemplateRequest' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TemplateDescription" Core..=)
              Prelude.<$> templateDescription,
            ("TextPart" Core..=) Prelude.<$> textPart,
            ("DefaultSubstitutions" Core..=)
              Prelude.<$> defaultSubstitutions,
            ("Subject" Core..=) Prelude.<$> subject,
            ("tags" Core..=) Prelude.<$> tags,
            ("RecommenderId" Core..=) Prelude.<$> recommenderId,
            ("HtmlPart" Core..=) Prelude.<$> htmlPart
          ]
      )
