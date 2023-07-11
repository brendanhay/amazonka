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
-- Module      : Amazonka.SES.Types.CustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SES.Types.CustomVerificationEmailTemplate where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a custom verification email template.
--
-- /See:/ 'newCustomVerificationEmailTemplate' smart constructor.
data CustomVerificationEmailTemplate = CustomVerificationEmailTemplate'
  { -- | The URL that the recipient of the verification email is sent to if his
    -- or her address is not successfully verified.
    failureRedirectionURL :: Prelude.Maybe Prelude.Text,
    -- | The email address that the custom verification email is sent from.
    fromEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | The URL that the recipient of the verification email is sent to if his
    -- or her address is successfully verified.
    successRedirectionURL :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom verification email template.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The subject line of the custom verification email.
    templateSubject :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CustomVerificationEmailTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'failureRedirectionURL', 'customVerificationEmailTemplate_failureRedirectionURL' - The URL that the recipient of the verification email is sent to if his
-- or her address is not successfully verified.
--
-- 'fromEmailAddress', 'customVerificationEmailTemplate_fromEmailAddress' - The email address that the custom verification email is sent from.
--
-- 'successRedirectionURL', 'customVerificationEmailTemplate_successRedirectionURL' - The URL that the recipient of the verification email is sent to if his
-- or her address is successfully verified.
--
-- 'templateName', 'customVerificationEmailTemplate_templateName' - The name of the custom verification email template.
--
-- 'templateSubject', 'customVerificationEmailTemplate_templateSubject' - The subject line of the custom verification email.
newCustomVerificationEmailTemplate ::
  CustomVerificationEmailTemplate
newCustomVerificationEmailTemplate =
  CustomVerificationEmailTemplate'
    { failureRedirectionURL =
        Prelude.Nothing,
      fromEmailAddress = Prelude.Nothing,
      successRedirectionURL = Prelude.Nothing,
      templateName = Prelude.Nothing,
      templateSubject = Prelude.Nothing
    }

-- | The URL that the recipient of the verification email is sent to if his
-- or her address is not successfully verified.
customVerificationEmailTemplate_failureRedirectionURL :: Lens.Lens' CustomVerificationEmailTemplate (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplate_failureRedirectionURL = Lens.lens (\CustomVerificationEmailTemplate' {failureRedirectionURL} -> failureRedirectionURL) (\s@CustomVerificationEmailTemplate' {} a -> s {failureRedirectionURL = a} :: CustomVerificationEmailTemplate)

-- | The email address that the custom verification email is sent from.
customVerificationEmailTemplate_fromEmailAddress :: Lens.Lens' CustomVerificationEmailTemplate (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplate_fromEmailAddress = Lens.lens (\CustomVerificationEmailTemplate' {fromEmailAddress} -> fromEmailAddress) (\s@CustomVerificationEmailTemplate' {} a -> s {fromEmailAddress = a} :: CustomVerificationEmailTemplate)

-- | The URL that the recipient of the verification email is sent to if his
-- or her address is successfully verified.
customVerificationEmailTemplate_successRedirectionURL :: Lens.Lens' CustomVerificationEmailTemplate (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplate_successRedirectionURL = Lens.lens (\CustomVerificationEmailTemplate' {successRedirectionURL} -> successRedirectionURL) (\s@CustomVerificationEmailTemplate' {} a -> s {successRedirectionURL = a} :: CustomVerificationEmailTemplate)

-- | The name of the custom verification email template.
customVerificationEmailTemplate_templateName :: Lens.Lens' CustomVerificationEmailTemplate (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplate_templateName = Lens.lens (\CustomVerificationEmailTemplate' {templateName} -> templateName) (\s@CustomVerificationEmailTemplate' {} a -> s {templateName = a} :: CustomVerificationEmailTemplate)

-- | The subject line of the custom verification email.
customVerificationEmailTemplate_templateSubject :: Lens.Lens' CustomVerificationEmailTemplate (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplate_templateSubject = Lens.lens (\CustomVerificationEmailTemplate' {templateSubject} -> templateSubject) (\s@CustomVerificationEmailTemplate' {} a -> s {templateSubject = a} :: CustomVerificationEmailTemplate)

instance Data.FromXML CustomVerificationEmailTemplate where
  parseXML x =
    CustomVerificationEmailTemplate'
      Prelude.<$> (x Data..@? "FailureRedirectionURL")
      Prelude.<*> (x Data..@? "FromEmailAddress")
      Prelude.<*> (x Data..@? "SuccessRedirectionURL")
      Prelude.<*> (x Data..@? "TemplateName")
      Prelude.<*> (x Data..@? "TemplateSubject")

instance
  Prelude.Hashable
    CustomVerificationEmailTemplate
  where
  hashWithSalt
    _salt
    CustomVerificationEmailTemplate' {..} =
      _salt
        `Prelude.hashWithSalt` failureRedirectionURL
        `Prelude.hashWithSalt` fromEmailAddress
        `Prelude.hashWithSalt` successRedirectionURL
        `Prelude.hashWithSalt` templateName
        `Prelude.hashWithSalt` templateSubject

instance
  Prelude.NFData
    CustomVerificationEmailTemplate
  where
  rnf CustomVerificationEmailTemplate' {..} =
    Prelude.rnf failureRedirectionURL
      `Prelude.seq` Prelude.rnf fromEmailAddress
      `Prelude.seq` Prelude.rnf successRedirectionURL
      `Prelude.seq` Prelude.rnf templateName
      `Prelude.seq` Prelude.rnf templateSubject
