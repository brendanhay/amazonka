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
-- Module      : Amazonka.SESV2.Types.CustomVerificationEmailTemplateMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.CustomVerificationEmailTemplateMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a custom verification email template.
--
-- /See:/ 'newCustomVerificationEmailTemplateMetadata' smart constructor.
data CustomVerificationEmailTemplateMetadata = CustomVerificationEmailTemplateMetadata'
  { -- | The name of the custom verification email template.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The URL that the recipient of the verification email is sent to if his
    -- or her address is successfully verified.
    successRedirectionURL :: Prelude.Maybe Prelude.Text,
    -- | The email address that the custom verification email is sent from.
    fromEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | The subject line of the custom verification email.
    templateSubject :: Prelude.Maybe Prelude.Text,
    -- | The URL that the recipient of the verification email is sent to if his
    -- or her address is not successfully verified.
    failureRedirectionURL :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomVerificationEmailTemplateMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'customVerificationEmailTemplateMetadata_templateName' - The name of the custom verification email template.
--
-- 'successRedirectionURL', 'customVerificationEmailTemplateMetadata_successRedirectionURL' - The URL that the recipient of the verification email is sent to if his
-- or her address is successfully verified.
--
-- 'fromEmailAddress', 'customVerificationEmailTemplateMetadata_fromEmailAddress' - The email address that the custom verification email is sent from.
--
-- 'templateSubject', 'customVerificationEmailTemplateMetadata_templateSubject' - The subject line of the custom verification email.
--
-- 'failureRedirectionURL', 'customVerificationEmailTemplateMetadata_failureRedirectionURL' - The URL that the recipient of the verification email is sent to if his
-- or her address is not successfully verified.
newCustomVerificationEmailTemplateMetadata ::
  CustomVerificationEmailTemplateMetadata
newCustomVerificationEmailTemplateMetadata =
  CustomVerificationEmailTemplateMetadata'
    { templateName =
        Prelude.Nothing,
      successRedirectionURL =
        Prelude.Nothing,
      fromEmailAddress = Prelude.Nothing,
      templateSubject = Prelude.Nothing,
      failureRedirectionURL =
        Prelude.Nothing
    }

-- | The name of the custom verification email template.
customVerificationEmailTemplateMetadata_templateName :: Lens.Lens' CustomVerificationEmailTemplateMetadata (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplateMetadata_templateName = Lens.lens (\CustomVerificationEmailTemplateMetadata' {templateName} -> templateName) (\s@CustomVerificationEmailTemplateMetadata' {} a -> s {templateName = a} :: CustomVerificationEmailTemplateMetadata)

-- | The URL that the recipient of the verification email is sent to if his
-- or her address is successfully verified.
customVerificationEmailTemplateMetadata_successRedirectionURL :: Lens.Lens' CustomVerificationEmailTemplateMetadata (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplateMetadata_successRedirectionURL = Lens.lens (\CustomVerificationEmailTemplateMetadata' {successRedirectionURL} -> successRedirectionURL) (\s@CustomVerificationEmailTemplateMetadata' {} a -> s {successRedirectionURL = a} :: CustomVerificationEmailTemplateMetadata)

-- | The email address that the custom verification email is sent from.
customVerificationEmailTemplateMetadata_fromEmailAddress :: Lens.Lens' CustomVerificationEmailTemplateMetadata (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplateMetadata_fromEmailAddress = Lens.lens (\CustomVerificationEmailTemplateMetadata' {fromEmailAddress} -> fromEmailAddress) (\s@CustomVerificationEmailTemplateMetadata' {} a -> s {fromEmailAddress = a} :: CustomVerificationEmailTemplateMetadata)

-- | The subject line of the custom verification email.
customVerificationEmailTemplateMetadata_templateSubject :: Lens.Lens' CustomVerificationEmailTemplateMetadata (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplateMetadata_templateSubject = Lens.lens (\CustomVerificationEmailTemplateMetadata' {templateSubject} -> templateSubject) (\s@CustomVerificationEmailTemplateMetadata' {} a -> s {templateSubject = a} :: CustomVerificationEmailTemplateMetadata)

-- | The URL that the recipient of the verification email is sent to if his
-- or her address is not successfully verified.
customVerificationEmailTemplateMetadata_failureRedirectionURL :: Lens.Lens' CustomVerificationEmailTemplateMetadata (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplateMetadata_failureRedirectionURL = Lens.lens (\CustomVerificationEmailTemplateMetadata' {failureRedirectionURL} -> failureRedirectionURL) (\s@CustomVerificationEmailTemplateMetadata' {} a -> s {failureRedirectionURL = a} :: CustomVerificationEmailTemplateMetadata)

instance
  Core.FromJSON
    CustomVerificationEmailTemplateMetadata
  where
  parseJSON =
    Core.withObject
      "CustomVerificationEmailTemplateMetadata"
      ( \x ->
          CustomVerificationEmailTemplateMetadata'
            Prelude.<$> (x Core..:? "TemplateName")
            Prelude.<*> (x Core..:? "SuccessRedirectionURL")
            Prelude.<*> (x Core..:? "FromEmailAddress")
            Prelude.<*> (x Core..:? "TemplateSubject")
            Prelude.<*> (x Core..:? "FailureRedirectionURL")
      )

instance
  Prelude.Hashable
    CustomVerificationEmailTemplateMetadata
  where
  hashWithSalt
    _salt
    CustomVerificationEmailTemplateMetadata' {..} =
      _salt `Prelude.hashWithSalt` templateName
        `Prelude.hashWithSalt` successRedirectionURL
        `Prelude.hashWithSalt` fromEmailAddress
        `Prelude.hashWithSalt` templateSubject
        `Prelude.hashWithSalt` failureRedirectionURL

instance
  Prelude.NFData
    CustomVerificationEmailTemplateMetadata
  where
  rnf CustomVerificationEmailTemplateMetadata' {..} =
    Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf successRedirectionURL
      `Prelude.seq` Prelude.rnf fromEmailAddress
      `Prelude.seq` Prelude.rnf templateSubject
      `Prelude.seq` Prelude.rnf failureRedirectionURL
