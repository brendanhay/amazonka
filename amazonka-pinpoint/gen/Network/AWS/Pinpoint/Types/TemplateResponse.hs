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
-- Module      : Network.AWS.Pinpoint.Types.TemplateResponse
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pinpoint.Types.TemplateResponse where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Pinpoint.Types.TemplateType

-- | Provides information about a message template that\'s associated with
-- your Amazon Pinpoint account.
--
-- /See:/ 'newTemplateResponse' smart constructor.
data TemplateResponse = TemplateResponse'
  { -- | The custom description of the message template. This value isn\'t
    -- included in a TemplateResponse object. To retrieve the description of a
    -- template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or
    -- GetVoiceTemplate operation, depending on the type of template that you
    -- want to retrieve the description for.
    templateDescription :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the message template. This value
    -- isn\'t included in a TemplateResponse object. To retrieve the ARN of a
    -- template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or
    -- GetVoiceTemplate operation, depending on the type of template that you
    -- want to retrieve the ARN for.
    arn :: Core.Maybe Core.Text,
    -- | The unique identifier, as an integer, for the active version of the
    -- message template.
    version :: Core.Maybe Core.Text,
    -- | The JSON object that specifies the default values that are used for
    -- message variables in the message template. This object isn\'t included
    -- in a TemplateResponse object. To retrieve this object for a template,
    -- use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or
    -- GetVoiceTemplate operation, depending on the type of template that you
    -- want to retrieve the object for.
    defaultSubstitutions :: Core.Maybe Core.Text,
    -- | A map of key-value pairs that identifies the tags that are associated
    -- with the message template. This object isn\'t included in a
    -- TemplateResponse object. To retrieve this object for a template, use the
    -- GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate
    -- operation, depending on the type of template that you want to retrieve
    -- the object for.
    tags :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The date, in ISO 8601 format, when the message template was last
    -- modified.
    lastModifiedDate :: Core.Text,
    -- | The date, in ISO 8601 format, when the message template was created.
    creationDate :: Core.Text,
    -- | The name of the message template.
    templateName :: Core.Text,
    -- | The type of channel that the message template is designed for. Possible
    -- values are: EMAIL, PUSH, SMS, and VOICE.
    templateType :: TemplateType
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateDescription', 'templateResponse_templateDescription' - The custom description of the message template. This value isn\'t
-- included in a TemplateResponse object. To retrieve the description of a
-- template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or
-- GetVoiceTemplate operation, depending on the type of template that you
-- want to retrieve the description for.
--
-- 'arn', 'templateResponse_arn' - The Amazon Resource Name (ARN) of the message template. This value
-- isn\'t included in a TemplateResponse object. To retrieve the ARN of a
-- template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or
-- GetVoiceTemplate operation, depending on the type of template that you
-- want to retrieve the ARN for.
--
-- 'version', 'templateResponse_version' - The unique identifier, as an integer, for the active version of the
-- message template.
--
-- 'defaultSubstitutions', 'templateResponse_defaultSubstitutions' - The JSON object that specifies the default values that are used for
-- message variables in the message template. This object isn\'t included
-- in a TemplateResponse object. To retrieve this object for a template,
-- use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or
-- GetVoiceTemplate operation, depending on the type of template that you
-- want to retrieve the object for.
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
  Core.Text ->
  -- | 'creationDate'
  Core.Text ->
  -- | 'templateName'
  Core.Text ->
  -- | 'templateType'
  TemplateType ->
  TemplateResponse
newTemplateResponse
  pLastModifiedDate_
  pCreationDate_
  pTemplateName_
  pTemplateType_ =
    TemplateResponse'
      { templateDescription =
          Core.Nothing,
        arn = Core.Nothing,
        version = Core.Nothing,
        defaultSubstitutions = Core.Nothing,
        tags = Core.Nothing,
        lastModifiedDate = pLastModifiedDate_,
        creationDate = pCreationDate_,
        templateName = pTemplateName_,
        templateType = pTemplateType_
      }

-- | The custom description of the message template. This value isn\'t
-- included in a TemplateResponse object. To retrieve the description of a
-- template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or
-- GetVoiceTemplate operation, depending on the type of template that you
-- want to retrieve the description for.
templateResponse_templateDescription :: Lens.Lens' TemplateResponse (Core.Maybe Core.Text)
templateResponse_templateDescription = Lens.lens (\TemplateResponse' {templateDescription} -> templateDescription) (\s@TemplateResponse' {} a -> s {templateDescription = a} :: TemplateResponse)

-- | The Amazon Resource Name (ARN) of the message template. This value
-- isn\'t included in a TemplateResponse object. To retrieve the ARN of a
-- template, use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or
-- GetVoiceTemplate operation, depending on the type of template that you
-- want to retrieve the ARN for.
templateResponse_arn :: Lens.Lens' TemplateResponse (Core.Maybe Core.Text)
templateResponse_arn = Lens.lens (\TemplateResponse' {arn} -> arn) (\s@TemplateResponse' {} a -> s {arn = a} :: TemplateResponse)

-- | The unique identifier, as an integer, for the active version of the
-- message template.
templateResponse_version :: Lens.Lens' TemplateResponse (Core.Maybe Core.Text)
templateResponse_version = Lens.lens (\TemplateResponse' {version} -> version) (\s@TemplateResponse' {} a -> s {version = a} :: TemplateResponse)

-- | The JSON object that specifies the default values that are used for
-- message variables in the message template. This object isn\'t included
-- in a TemplateResponse object. To retrieve this object for a template,
-- use the GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or
-- GetVoiceTemplate operation, depending on the type of template that you
-- want to retrieve the object for.
templateResponse_defaultSubstitutions :: Lens.Lens' TemplateResponse (Core.Maybe Core.Text)
templateResponse_defaultSubstitutions = Lens.lens (\TemplateResponse' {defaultSubstitutions} -> defaultSubstitutions) (\s@TemplateResponse' {} a -> s {defaultSubstitutions = a} :: TemplateResponse)

-- | A map of key-value pairs that identifies the tags that are associated
-- with the message template. This object isn\'t included in a
-- TemplateResponse object. To retrieve this object for a template, use the
-- GetEmailTemplate, GetPushTemplate, GetSmsTemplate, or GetVoiceTemplate
-- operation, depending on the type of template that you want to retrieve
-- the object for.
templateResponse_tags :: Lens.Lens' TemplateResponse (Core.Maybe (Core.HashMap Core.Text Core.Text))
templateResponse_tags = Lens.lens (\TemplateResponse' {tags} -> tags) (\s@TemplateResponse' {} a -> s {tags = a} :: TemplateResponse) Core.. Lens.mapping Lens._Coerce

-- | The date, in ISO 8601 format, when the message template was last
-- modified.
templateResponse_lastModifiedDate :: Lens.Lens' TemplateResponse Core.Text
templateResponse_lastModifiedDate = Lens.lens (\TemplateResponse' {lastModifiedDate} -> lastModifiedDate) (\s@TemplateResponse' {} a -> s {lastModifiedDate = a} :: TemplateResponse)

-- | The date, in ISO 8601 format, when the message template was created.
templateResponse_creationDate :: Lens.Lens' TemplateResponse Core.Text
templateResponse_creationDate = Lens.lens (\TemplateResponse' {creationDate} -> creationDate) (\s@TemplateResponse' {} a -> s {creationDate = a} :: TemplateResponse)

-- | The name of the message template.
templateResponse_templateName :: Lens.Lens' TemplateResponse Core.Text
templateResponse_templateName = Lens.lens (\TemplateResponse' {templateName} -> templateName) (\s@TemplateResponse' {} a -> s {templateName = a} :: TemplateResponse)

-- | The type of channel that the message template is designed for. Possible
-- values are: EMAIL, PUSH, SMS, and VOICE.
templateResponse_templateType :: Lens.Lens' TemplateResponse TemplateType
templateResponse_templateType = Lens.lens (\TemplateResponse' {templateType} -> templateType) (\s@TemplateResponse' {} a -> s {templateType = a} :: TemplateResponse)

instance Core.FromJSON TemplateResponse where
  parseJSON =
    Core.withObject
      "TemplateResponse"
      ( \x ->
          TemplateResponse'
            Core.<$> (x Core..:? "TemplateDescription")
            Core.<*> (x Core..:? "Arn")
            Core.<*> (x Core..:? "Version")
            Core.<*> (x Core..:? "DefaultSubstitutions")
            Core.<*> (x Core..:? "tags" Core..!= Core.mempty)
            Core.<*> (x Core..: "LastModifiedDate")
            Core.<*> (x Core..: "CreationDate")
            Core.<*> (x Core..: "TemplateName")
            Core.<*> (x Core..: "TemplateType")
      )

instance Core.Hashable TemplateResponse

instance Core.NFData TemplateResponse
