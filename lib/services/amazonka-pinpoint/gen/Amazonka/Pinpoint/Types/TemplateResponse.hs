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
-- Module      : Amazonka.Pinpoint.Types.TemplateResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.TemplateResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pinpoint.Types.TemplateType
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a message template that\'s associated with
-- your Amazon Pinpoint account.
--
-- /See:/ 'newTemplateResponse' smart constructor.
data TemplateResponse = TemplateResponse'
  { -- | The Amazon Resource Name (ARN) of the message template. This value
    -- isn\'t included in a TemplateResponse object. To retrieve the ARN of a
    -- template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or
    -- GetVoiceTemplate operation, depending on the type of template that you
    -- want to retrieve the ARN for.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The JSON object that specifies the default values that are used for
    -- message variables in the message template. This object isn\'t included
    -- in a TemplateResponse object. To retrieve this object for a template,
    -- use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or
    -- GetVoiceTemplate operation, depending on the type of template that you
    -- want to retrieve the object for.
    defaultSubstitutions :: Prelude.Maybe Prelude.Text,
    -- | The custom description of the message template. This value isn\'t
    -- included in a TemplateResponse object. To retrieve the description of a
    -- template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or
    -- GetVoiceTemplate operation, depending on the type of template that you
    -- want to retrieve the description for.
    templateDescription :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier, as an integer, for the active version of the
    -- message template.
    version :: Prelude.Maybe Prelude.Text,
    -- | A map of key-value pairs that identifies the tags that are associated
    -- with the message template. This object isn\'t included in a
    -- TemplateResponse object. To retrieve this object for a template, use the
    -- GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate
    -- operation, depending on the type of template that you want to retrieve
    -- the object for.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The date, in ISO 8601 format, when the message template was last
    -- modified.
    lastModifiedDate :: Prelude.Text,
    -- | The date, in ISO 8601 format, when the message template was created.
    creationDate :: Prelude.Text,
    -- | The name of the message template.
    templateName :: Prelude.Text,
    -- | The type of channel that the message template is designed for. Possible
    -- values are: EMAIL, PUSH, SMS, and VOICE.
    templateType :: TemplateType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'templateResponse_arn' - The Amazon Resource Name (ARN) of the message template. This value
-- isn\'t included in a TemplateResponse object. To retrieve the ARN of a
-- template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or
-- GetVoiceTemplate operation, depending on the type of template that you
-- want to retrieve the ARN for.
--
-- 'defaultSubstitutions', 'templateResponse_defaultSubstitutions' - The JSON object that specifies the default values that are used for
-- message variables in the message template. This object isn\'t included
-- in a TemplateResponse object. To retrieve this object for a template,
-- use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or
-- GetVoiceTemplate operation, depending on the type of template that you
-- want to retrieve the object for.
--
-- 'templateDescription', 'templateResponse_templateDescription' - The custom description of the message template. This value isn\'t
-- included in a TemplateResponse object. To retrieve the description of a
-- template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or
-- GetVoiceTemplate operation, depending on the type of template that you
-- want to retrieve the description for.
--
-- 'version', 'templateResponse_version' - The unique identifier, as an integer, for the active version of the
-- message template.
--
-- 'tags', 'templateResponse_tags' - A map of key-value pairs that identifies the tags that are associated
-- with the message template. This object isn\'t included in a
-- TemplateResponse object. To retrieve this object for a template, use the
-- GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate
-- operation, depending on the type of template that you want to retrieve
-- the object for.
--
-- 'lastModifiedDate', 'templateResponse_lastModifiedDate' - The date, in ISO 8601 format, when the message template was last
-- modified.
--
-- 'creationDate', 'templateResponse_creationDate' - The date, in ISO 8601 format, when the message template was created.
--
-- 'templateName', 'templateResponse_templateName' - The name of the message template.
--
-- 'templateType', 'templateResponse_templateType' - The type of channel that the message template is designed for. Possible
-- values are: EMAIL, PUSH, SMS, and VOICE.
newTemplateResponse ::
  -- | 'lastModifiedDate'
  Prelude.Text ->
  -- | 'creationDate'
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  -- | 'templateType'
  TemplateType ->
  TemplateResponse
newTemplateResponse
  pLastModifiedDate_
  pCreationDate_
  pTemplateName_
  pTemplateType_ =
    TemplateResponse'
      { arn = Prelude.Nothing,
        defaultSubstitutions = Prelude.Nothing,
        templateDescription = Prelude.Nothing,
        version = Prelude.Nothing,
        tags = Prelude.Nothing,
        lastModifiedDate = pLastModifiedDate_,
        creationDate = pCreationDate_,
        templateName = pTemplateName_,
        templateType = pTemplateType_
      }

-- | The Amazon Resource Name (ARN) of the message template. This value
-- isn\'t included in a TemplateResponse object. To retrieve the ARN of a
-- template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or
-- GetVoiceTemplate operation, depending on the type of template that you
-- want to retrieve the ARN for.
templateResponse_arn :: Lens.Lens' TemplateResponse (Prelude.Maybe Prelude.Text)
templateResponse_arn = Lens.lens (\TemplateResponse' {arn} -> arn) (\s@TemplateResponse' {} a -> s {arn = a} :: TemplateResponse)

-- | The JSON object that specifies the default values that are used for
-- message variables in the message template. This object isn\'t included
-- in a TemplateResponse object. To retrieve this object for a template,
-- use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or
-- GetVoiceTemplate operation, depending on the type of template that you
-- want to retrieve the object for.
templateResponse_defaultSubstitutions :: Lens.Lens' TemplateResponse (Prelude.Maybe Prelude.Text)
templateResponse_defaultSubstitutions = Lens.lens (\TemplateResponse' {defaultSubstitutions} -> defaultSubstitutions) (\s@TemplateResponse' {} a -> s {defaultSubstitutions = a} :: TemplateResponse)

-- | The custom description of the message template. This value isn\'t
-- included in a TemplateResponse object. To retrieve the description of a
-- template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or
-- GetVoiceTemplate operation, depending on the type of template that you
-- want to retrieve the description for.
templateResponse_templateDescription :: Lens.Lens' TemplateResponse (Prelude.Maybe Prelude.Text)
templateResponse_templateDescription = Lens.lens (\TemplateResponse' {templateDescription} -> templateDescription) (\s@TemplateResponse' {} a -> s {templateDescription = a} :: TemplateResponse)

-- | The unique identifier, as an integer, for the active version of the
-- message template.
templateResponse_version :: Lens.Lens' TemplateResponse (Prelude.Maybe Prelude.Text)
templateResponse_version = Lens.lens (\TemplateResponse' {version} -> version) (\s@TemplateResponse' {} a -> s {version = a} :: TemplateResponse)

-- | A map of key-value pairs that identifies the tags that are associated
-- with the message template. This object isn\'t included in a
-- TemplateResponse object. To retrieve this object for a template, use the
-- GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate
-- operation, depending on the type of template that you want to retrieve
-- the object for.
templateResponse_tags :: Lens.Lens' TemplateResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
templateResponse_tags = Lens.lens (\TemplateResponse' {tags} -> tags) (\s@TemplateResponse' {} a -> s {tags = a} :: TemplateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date, in ISO 8601 format, when the message template was last
-- modified.
templateResponse_lastModifiedDate :: Lens.Lens' TemplateResponse Prelude.Text
templateResponse_lastModifiedDate = Lens.lens (\TemplateResponse' {lastModifiedDate} -> lastModifiedDate) (\s@TemplateResponse' {} a -> s {lastModifiedDate = a} :: TemplateResponse)

-- | The date, in ISO 8601 format, when the message template was created.
templateResponse_creationDate :: Lens.Lens' TemplateResponse Prelude.Text
templateResponse_creationDate = Lens.lens (\TemplateResponse' {creationDate} -> creationDate) (\s@TemplateResponse' {} a -> s {creationDate = a} :: TemplateResponse)

-- | The name of the message template.
templateResponse_templateName :: Lens.Lens' TemplateResponse Prelude.Text
templateResponse_templateName = Lens.lens (\TemplateResponse' {templateName} -> templateName) (\s@TemplateResponse' {} a -> s {templateName = a} :: TemplateResponse)

-- | The type of channel that the message template is designed for. Possible
-- values are: EMAIL, PUSH, SMS, and VOICE.
templateResponse_templateType :: Lens.Lens' TemplateResponse TemplateType
templateResponse_templateType = Lens.lens (\TemplateResponse' {templateType} -> templateType) (\s@TemplateResponse' {} a -> s {templateType = a} :: TemplateResponse)

instance Data.FromJSON TemplateResponse where
  parseJSON =
    Data.withObject
      "TemplateResponse"
      ( \x ->
          TemplateResponse'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "DefaultSubstitutions")
            Prelude.<*> (x Data..:? "TemplateDescription")
            Prelude.<*> (x Data..:? "Version")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..: "LastModifiedDate")
            Prelude.<*> (x Data..: "CreationDate")
            Prelude.<*> (x Data..: "TemplateName")
            Prelude.<*> (x Data..: "TemplateType")
      )

instance Prelude.Hashable TemplateResponse where
  hashWithSalt _salt TemplateResponse' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` defaultSubstitutions
      `Prelude.hashWithSalt` templateDescription
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateType

instance Prelude.NFData TemplateResponse where
  rnf TemplateResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf defaultSubstitutions
      `Prelude.seq` Prelude.rnf templateDescription
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateType
