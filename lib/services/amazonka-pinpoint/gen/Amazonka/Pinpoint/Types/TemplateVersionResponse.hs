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
-- Module      : Amazonka.Pinpoint.Types.TemplateVersionResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pinpoint.Types.TemplateVersionResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides information about a specific version of a message template.
--
-- /See:/ 'newTemplateVersionResponse' smart constructor.
data TemplateVersionResponse = TemplateVersionResponse'
  { -- | A JSON object that specifies the default values that are used for
    -- message variables in the version of the message template. This object is
    -- a set of key-value pairs. Each key defines a message variable in the
    -- template. The corresponding value defines the default value for that
    -- variable.
    defaultSubstitutions :: Prelude.Maybe Prelude.Text,
    -- | The custom description of the version of the message template.
    templateDescription :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the version of the message template. This
    -- value is an integer that Amazon Pinpoint automatically increments and
    -- assigns to each new version of a template.
    version :: Prelude.Maybe Prelude.Text,
    -- | The date, in ISO 8601 format, when the version of the message template
    -- was last modified.
    lastModifiedDate :: Prelude.Text,
    -- | The date, in ISO 8601 format, when the version of the message template
    -- was created.
    creationDate :: Prelude.Text,
    -- | The name of the message template.
    templateName :: Prelude.Text,
    -- | The type of channel that the message template is designed for. Possible
    -- values are: EMAIL, PUSH, SMS, and VOICE.
    templateType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TemplateVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'defaultSubstitutions', 'templateVersionResponse_defaultSubstitutions' - A JSON object that specifies the default values that are used for
-- message variables in the version of the message template. This object is
-- a set of key-value pairs. Each key defines a message variable in the
-- template. The corresponding value defines the default value for that
-- variable.
--
-- 'templateDescription', 'templateVersionResponse_templateDescription' - The custom description of the version of the message template.
--
-- 'version', 'templateVersionResponse_version' - The unique identifier for the version of the message template. This
-- value is an integer that Amazon Pinpoint automatically increments and
-- assigns to each new version of a template.
--
-- 'lastModifiedDate', 'templateVersionResponse_lastModifiedDate' - The date, in ISO 8601 format, when the version of the message template
-- was last modified.
--
-- 'creationDate', 'templateVersionResponse_creationDate' - The date, in ISO 8601 format, when the version of the message template
-- was created.
--
-- 'templateName', 'templateVersionResponse_templateName' - The name of the message template.
--
-- 'templateType', 'templateVersionResponse_templateType' - The type of channel that the message template is designed for. Possible
-- values are: EMAIL, PUSH, SMS, and VOICE.
newTemplateVersionResponse ::
  -- | 'lastModifiedDate'
  Prelude.Text ->
  -- | 'creationDate'
  Prelude.Text ->
  -- | 'templateName'
  Prelude.Text ->
  -- | 'templateType'
  Prelude.Text ->
  TemplateVersionResponse
newTemplateVersionResponse
  pLastModifiedDate_
  pCreationDate_
  pTemplateName_
  pTemplateType_ =
    TemplateVersionResponse'
      { defaultSubstitutions =
          Prelude.Nothing,
        templateDescription = Prelude.Nothing,
        version = Prelude.Nothing,
        lastModifiedDate = pLastModifiedDate_,
        creationDate = pCreationDate_,
        templateName = pTemplateName_,
        templateType = pTemplateType_
      }

-- | A JSON object that specifies the default values that are used for
-- message variables in the version of the message template. This object is
-- a set of key-value pairs. Each key defines a message variable in the
-- template. The corresponding value defines the default value for that
-- variable.
templateVersionResponse_defaultSubstitutions :: Lens.Lens' TemplateVersionResponse (Prelude.Maybe Prelude.Text)
templateVersionResponse_defaultSubstitutions = Lens.lens (\TemplateVersionResponse' {defaultSubstitutions} -> defaultSubstitutions) (\s@TemplateVersionResponse' {} a -> s {defaultSubstitutions = a} :: TemplateVersionResponse)

-- | The custom description of the version of the message template.
templateVersionResponse_templateDescription :: Lens.Lens' TemplateVersionResponse (Prelude.Maybe Prelude.Text)
templateVersionResponse_templateDescription = Lens.lens (\TemplateVersionResponse' {templateDescription} -> templateDescription) (\s@TemplateVersionResponse' {} a -> s {templateDescription = a} :: TemplateVersionResponse)

-- | The unique identifier for the version of the message template. This
-- value is an integer that Amazon Pinpoint automatically increments and
-- assigns to each new version of a template.
templateVersionResponse_version :: Lens.Lens' TemplateVersionResponse (Prelude.Maybe Prelude.Text)
templateVersionResponse_version = Lens.lens (\TemplateVersionResponse' {version} -> version) (\s@TemplateVersionResponse' {} a -> s {version = a} :: TemplateVersionResponse)

-- | The date, in ISO 8601 format, when the version of the message template
-- was last modified.
templateVersionResponse_lastModifiedDate :: Lens.Lens' TemplateVersionResponse Prelude.Text
templateVersionResponse_lastModifiedDate = Lens.lens (\TemplateVersionResponse' {lastModifiedDate} -> lastModifiedDate) (\s@TemplateVersionResponse' {} a -> s {lastModifiedDate = a} :: TemplateVersionResponse)

-- | The date, in ISO 8601 format, when the version of the message template
-- was created.
templateVersionResponse_creationDate :: Lens.Lens' TemplateVersionResponse Prelude.Text
templateVersionResponse_creationDate = Lens.lens (\TemplateVersionResponse' {creationDate} -> creationDate) (\s@TemplateVersionResponse' {} a -> s {creationDate = a} :: TemplateVersionResponse)

-- | The name of the message template.
templateVersionResponse_templateName :: Lens.Lens' TemplateVersionResponse Prelude.Text
templateVersionResponse_templateName = Lens.lens (\TemplateVersionResponse' {templateName} -> templateName) (\s@TemplateVersionResponse' {} a -> s {templateName = a} :: TemplateVersionResponse)

-- | The type of channel that the message template is designed for. Possible
-- values are: EMAIL, PUSH, SMS, and VOICE.
templateVersionResponse_templateType :: Lens.Lens' TemplateVersionResponse Prelude.Text
templateVersionResponse_templateType = Lens.lens (\TemplateVersionResponse' {templateType} -> templateType) (\s@TemplateVersionResponse' {} a -> s {templateType = a} :: TemplateVersionResponse)

instance Data.FromJSON TemplateVersionResponse where
  parseJSON =
    Data.withObject
      "TemplateVersionResponse"
      ( \x ->
          TemplateVersionResponse'
            Prelude.<$> (x Data..:? "DefaultSubstitutions")
            Prelude.<*> (x Data..:? "TemplateDescription")
            Prelude.<*> (x Data..:? "Version")
            Prelude.<*> (x Data..: "LastModifiedDate")
            Prelude.<*> (x Data..: "CreationDate")
            Prelude.<*> (x Data..: "TemplateName")
            Prelude.<*> (x Data..: "TemplateType")
      )

instance Prelude.Hashable TemplateVersionResponse where
  hashWithSalt _salt TemplateVersionResponse' {..} =
    _salt
      `Prelude.hashWithSalt` defaultSubstitutions
      `Prelude.hashWithSalt` templateDescription
      `Prelude.hashWithSalt` version
      `Prelude.hashWithSalt` lastModifiedDate
      `Prelude.hashWithSalt` creationDate
      `Prelude.hashWithSalt` templateName
      `Prelude.hashWithSalt` templateType

instance Prelude.NFData TemplateVersionResponse where
  rnf TemplateVersionResponse' {..} =
    Prelude.rnf defaultSubstitutions
      `Prelude.seq` Prelude.rnf templateDescription
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf lastModifiedDate
      `Prelude.seq` Prelude.rnf creationDate
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateType
