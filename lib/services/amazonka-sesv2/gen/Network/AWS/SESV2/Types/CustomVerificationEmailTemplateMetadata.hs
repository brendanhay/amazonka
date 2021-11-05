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
-- Module      : Network.AWS.SESV2.Types.CustomVerificationEmailTemplateMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SESV2.Types.CustomVerificationEmailTemplateMetadata where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a custom verification email template.
--
-- /See:/ 'newCustomVerificationEmailTemplateMetadata' smart constructor.
data CustomVerificationEmailTemplateMetadata = CustomVerificationEmailTemplateMetadata'
  { -- | The email address that the custom verification email is sent from.
    fromEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom verification email template.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The URL that the recipient of the verification email is sent to if his
    -- or her address is not successfully verified.
    failureRedirectionURL :: Prelude.Maybe Prelude.Text,
    -- | The subject line of the custom verification email.
    templateSubject :: Prelude.Maybe Prelude.Text,
    -- | The URL that the recipient of the verification email is sent to if his
    -- or her address is successfully verified.
    successRedirectionURL :: Prelude.Maybe Prelude.Text
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
-- 'fromEmailAddress', 'customVerificationEmailTemplateMetadata_fromEmailAddress' - The email address that the custom verification email is sent from.
--
-- 'templateName', 'customVerificationEmailTemplateMetadata_templateName' - The name of the custom verification email template.
--
-- 'failureRedirectionURL', 'customVerificationEmailTemplateMetadata_failureRedirectionURL' - The URL that the recipient of the verification email is sent to if his
-- or her address is not successfully verified.
--
-- 'templateSubject', 'customVerificationEmailTemplateMetadata_templateSubject' - The subject line of the custom verification email.
--
-- 'successRedirectionURL', 'customVerificationEmailTemplateMetadata_successRedirectionURL' - The URL that the recipient of the verification email is sent to if his
-- or her address is successfully verified.
newCustomVerificationEmailTemplateMetadata ::
  CustomVerificationEmailTemplateMetadata
newCustomVerificationEmailTemplateMetadata =
  CustomVerificationEmailTemplateMetadata'
    { fromEmailAddress =
        Prelude.Nothing,
      templateName = Prelude.Nothing,
      failureRedirectionURL =
        Prelude.Nothing,
      templateSubject = Prelude.Nothing,
      successRedirectionURL =
        Prelude.Nothing
    }

-- | The email address that the custom verification email is sent from.
customVerificationEmailTemplateMetadata_fromEmailAddress :: Lens.Lens' CustomVerificationEmailTemplateMetadata (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplateMetadata_fromEmailAddress = Lens.lens (\CustomVerificationEmailTemplateMetadata' {fromEmailAddress} -> fromEmailAddress) (\s@CustomVerificationEmailTemplateMetadata' {} a -> s {fromEmailAddress = a} :: CustomVerificationEmailTemplateMetadata)

-- | The name of the custom verification email template.
customVerificationEmailTemplateMetadata_templateName :: Lens.Lens' CustomVerificationEmailTemplateMetadata (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplateMetadata_templateName = Lens.lens (\CustomVerificationEmailTemplateMetadata' {templateName} -> templateName) (\s@CustomVerificationEmailTemplateMetadata' {} a -> s {templateName = a} :: CustomVerificationEmailTemplateMetadata)

-- | The URL that the recipient of the verification email is sent to if his
-- or her address is not successfully verified.
customVerificationEmailTemplateMetadata_failureRedirectionURL :: Lens.Lens' CustomVerificationEmailTemplateMetadata (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplateMetadata_failureRedirectionURL = Lens.lens (\CustomVerificationEmailTemplateMetadata' {failureRedirectionURL} -> failureRedirectionURL) (\s@CustomVerificationEmailTemplateMetadata' {} a -> s {failureRedirectionURL = a} :: CustomVerificationEmailTemplateMetadata)

-- | The subject line of the custom verification email.
customVerificationEmailTemplateMetadata_templateSubject :: Lens.Lens' CustomVerificationEmailTemplateMetadata (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplateMetadata_templateSubject = Lens.lens (\CustomVerificationEmailTemplateMetadata' {templateSubject} -> templateSubject) (\s@CustomVerificationEmailTemplateMetadata' {} a -> s {templateSubject = a} :: CustomVerificationEmailTemplateMetadata)

-- | The URL that the recipient of the verification email is sent to if his
-- or her address is successfully verified.
customVerificationEmailTemplateMetadata_successRedirectionURL :: Lens.Lens' CustomVerificationEmailTemplateMetadata (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplateMetadata_successRedirectionURL = Lens.lens (\CustomVerificationEmailTemplateMetadata' {successRedirectionURL} -> successRedirectionURL) (\s@CustomVerificationEmailTemplateMetadata' {} a -> s {successRedirectionURL = a} :: CustomVerificationEmailTemplateMetadata)

instance
  Core.FromJSON
    CustomVerificationEmailTemplateMetadata
  where
  parseJSON =
    Core.withObject
      "CustomVerificationEmailTemplateMetadata"
      ( \x ->
          CustomVerificationEmailTemplateMetadata'
            Prelude.<$> (x Core..:? "FromEmailAddress")
            Prelude.<*> (x Core..:? "TemplateName")
            Prelude.<*> (x Core..:? "FailureRedirectionURL")
            Prelude.<*> (x Core..:? "TemplateSubject")
            Prelude.<*> (x Core..:? "SuccessRedirectionURL")
      )

instance
  Prelude.Hashable
    CustomVerificationEmailTemplateMetadata

instance
  Prelude.NFData
    CustomVerificationEmailTemplateMetadata
