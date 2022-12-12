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
-- Module      : Amazonka.Pinpoint.Types.SMSTemplateRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.SMSTemplateRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Specifies the content and settings for a message template that can be
-- used in text messages that are sent through the SMS channel.
--
-- /See:/ 'newSMSTemplateRequest' smart constructor.
data SMSTemplateRequest = SMSTemplateRequest'
  { -- | The message body to use in text messages that are based on the message
    -- template.
    body :: Prelude.Maybe Prelude.Text,
    -- | A JSON object that specifies the default values to use for message
    -- variables in the message template. This object is a set of key-value
    -- pairs. Each key defines a message variable in the template. The
    -- corresponding value defines the default value for that variable. When
    -- you create a message that\'s based on the template, you can override
    -- these defaults with message-specific and address-specific variables and
    -- values.
    defaultSubstitutions :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the recommender model to use for the message
    -- template. Amazon Pinpoint uses this value to determine how to retrieve
    -- and process data from a recommender model when it sends messages that
    -- use the template, if the template contains message variables for
    -- recommendation data.
    recommenderId :: Prelude.Maybe Prelude.Text,
    -- | A custom description of the message template.
    templateDescription :: Prelude.Maybe Prelude.Text,
    -- | A string-to-string map of key-value pairs that defines the tags to
    -- associate with the message template. Each tag consists of a required tag
    -- key and an associated tag value.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SMSTemplateRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'recommenderId', 'sMSTemplateRequest_recommenderId' - The unique identifier for the recommender model to use for the message
-- template. Amazon Pinpoint uses this value to determine how to retrieve
-- and process data from a recommender model when it sends messages that
-- use the template, if the template contains message variables for
-- recommendation data.
--
-- 'templateDescription', 'sMSTemplateRequest_templateDescription' - A custom description of the message template.
--
-- 'tags', 'sMSTemplateRequest_tags' - A string-to-string map of key-value pairs that defines the tags to
-- associate with the message template. Each tag consists of a required tag
-- key and an associated tag value.
newSMSTemplateRequest ::
  SMSTemplateRequest
newSMSTemplateRequest =
  SMSTemplateRequest'
    { body = Prelude.Nothing,
      defaultSubstitutions = Prelude.Nothing,
      recommenderId = Prelude.Nothing,
      templateDescription = Prelude.Nothing,
      tags = Prelude.Nothing
    }

-- | The message body to use in text messages that are based on the message
-- template.
sMSTemplateRequest_body :: Lens.Lens' SMSTemplateRequest (Prelude.Maybe Prelude.Text)
sMSTemplateRequest_body = Lens.lens (\SMSTemplateRequest' {body} -> body) (\s@SMSTemplateRequest' {} a -> s {body = a} :: SMSTemplateRequest)

-- | A JSON object that specifies the default values to use for message
-- variables in the message template. This object is a set of key-value
-- pairs. Each key defines a message variable in the template. The
-- corresponding value defines the default value for that variable. When
-- you create a message that\'s based on the template, you can override
-- these defaults with message-specific and address-specific variables and
-- values.
sMSTemplateRequest_defaultSubstitutions :: Lens.Lens' SMSTemplateRequest (Prelude.Maybe Prelude.Text)
sMSTemplateRequest_defaultSubstitutions = Lens.lens (\SMSTemplateRequest' {defaultSubstitutions} -> defaultSubstitutions) (\s@SMSTemplateRequest' {} a -> s {defaultSubstitutions = a} :: SMSTemplateRequest)

-- | The unique identifier for the recommender model to use for the message
-- template. Amazon Pinpoint uses this value to determine how to retrieve
-- and process data from a recommender model when it sends messages that
-- use the template, if the template contains message variables for
-- recommendation data.
sMSTemplateRequest_recommenderId :: Lens.Lens' SMSTemplateRequest (Prelude.Maybe Prelude.Text)
sMSTemplateRequest_recommenderId = Lens.lens (\SMSTemplateRequest' {recommenderId} -> recommenderId) (\s@SMSTemplateRequest' {} a -> s {recommenderId = a} :: SMSTemplateRequest)

-- | A custom description of the message template.
sMSTemplateRequest_templateDescription :: Lens.Lens' SMSTemplateRequest (Prelude.Maybe Prelude.Text)
sMSTemplateRequest_templateDescription = Lens.lens (\SMSTemplateRequest' {templateDescription} -> templateDescription) (\s@SMSTemplateRequest' {} a -> s {templateDescription = a} :: SMSTemplateRequest)

-- | A string-to-string map of key-value pairs that defines the tags to
-- associate with the message template. Each tag consists of a required tag
-- key and an associated tag value.
sMSTemplateRequest_tags :: Lens.Lens' SMSTemplateRequest (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
sMSTemplateRequest_tags = Lens.lens (\SMSTemplateRequest' {tags} -> tags) (\s@SMSTemplateRequest' {} a -> s {tags = a} :: SMSTemplateRequest) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable SMSTemplateRequest where
  hashWithSalt _salt SMSTemplateRequest' {..} =
    _salt `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` defaultSubstitutions
      `Prelude.hashWithSalt` recommenderId
      `Prelude.hashWithSalt` templateDescription
      `Prelude.hashWithSalt` tags

instance Prelude.NFData SMSTemplateRequest where
  rnf SMSTemplateRequest' {..} =
    Prelude.rnf body
      `Prelude.seq` Prelude.rnf defaultSubstitutions
      `Prelude.seq` Prelude.rnf recommenderId
      `Prelude.seq` Prelude.rnf templateDescription
      `Prelude.seq` Prelude.rnf tags

instance Data.ToJSON SMSTemplateRequest where
  toJSON SMSTemplateRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Body" Data..=) Prelude.<$> body,
            ("DefaultSubstitutions" Data..=)
              Prelude.<$> defaultSubstitutions,
            ("RecommenderId" Data..=) Prelude.<$> recommenderId,
            ("TemplateDescription" Data..=)
              Prelude.<$> templateDescription,
            ("tags" Data..=) Prelude.<$> tags
          ]
      )
