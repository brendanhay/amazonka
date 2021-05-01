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
-- Module      : Network.AWS.SES.Types.CustomVerificationEmailTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SES.Types.CustomVerificationEmailTemplate where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about a custom verification email template.
--
-- /See:/ 'newCustomVerificationEmailTemplate' smart constructor.
data CustomVerificationEmailTemplate = CustomVerificationEmailTemplate'
  { -- | The name of the custom verification email template.
    templateName :: Prelude.Maybe Prelude.Text,
    -- | The subject line of the custom verification email.
    templateSubject :: Prelude.Maybe Prelude.Text,
    -- | The email address that the custom verification email is sent from.
    fromEmailAddress :: Prelude.Maybe Prelude.Text,
    -- | The URL that the recipient of the verification email is sent to if his
    -- or her address is successfully verified.
    successRedirectionURL :: Prelude.Maybe Prelude.Text,
    -- | The URL that the recipient of the verification email is sent to if his
    -- or her address is not successfully verified.
    failureRedirectionURL :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'CustomVerificationEmailTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'templateName', 'customVerificationEmailTemplate_templateName' - The name of the custom verification email template.
--
-- 'templateSubject', 'customVerificationEmailTemplate_templateSubject' - The subject line of the custom verification email.
--
-- 'fromEmailAddress', 'customVerificationEmailTemplate_fromEmailAddress' - The email address that the custom verification email is sent from.
--
-- 'successRedirectionURL', 'customVerificationEmailTemplate_successRedirectionURL' - The URL that the recipient of the verification email is sent to if his
-- or her address is successfully verified.
--
-- 'failureRedirectionURL', 'customVerificationEmailTemplate_failureRedirectionURL' - The URL that the recipient of the verification email is sent to if his
-- or her address is not successfully verified.
newCustomVerificationEmailTemplate ::
  CustomVerificationEmailTemplate
newCustomVerificationEmailTemplate =
  CustomVerificationEmailTemplate'
    { templateName =
        Prelude.Nothing,
      templateSubject = Prelude.Nothing,
      fromEmailAddress = Prelude.Nothing,
      successRedirectionURL = Prelude.Nothing,
      failureRedirectionURL = Prelude.Nothing
    }

-- | The name of the custom verification email template.
customVerificationEmailTemplate_templateName :: Lens.Lens' CustomVerificationEmailTemplate (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplate_templateName = Lens.lens (\CustomVerificationEmailTemplate' {templateName} -> templateName) (\s@CustomVerificationEmailTemplate' {} a -> s {templateName = a} :: CustomVerificationEmailTemplate)

-- | The subject line of the custom verification email.
customVerificationEmailTemplate_templateSubject :: Lens.Lens' CustomVerificationEmailTemplate (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplate_templateSubject = Lens.lens (\CustomVerificationEmailTemplate' {templateSubject} -> templateSubject) (\s@CustomVerificationEmailTemplate' {} a -> s {templateSubject = a} :: CustomVerificationEmailTemplate)

-- | The email address that the custom verification email is sent from.
customVerificationEmailTemplate_fromEmailAddress :: Lens.Lens' CustomVerificationEmailTemplate (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplate_fromEmailAddress = Lens.lens (\CustomVerificationEmailTemplate' {fromEmailAddress} -> fromEmailAddress) (\s@CustomVerificationEmailTemplate' {} a -> s {fromEmailAddress = a} :: CustomVerificationEmailTemplate)

-- | The URL that the recipient of the verification email is sent to if his
-- or her address is successfully verified.
customVerificationEmailTemplate_successRedirectionURL :: Lens.Lens' CustomVerificationEmailTemplate (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplate_successRedirectionURL = Lens.lens (\CustomVerificationEmailTemplate' {successRedirectionURL} -> successRedirectionURL) (\s@CustomVerificationEmailTemplate' {} a -> s {successRedirectionURL = a} :: CustomVerificationEmailTemplate)

-- | The URL that the recipient of the verification email is sent to if his
-- or her address is not successfully verified.
customVerificationEmailTemplate_failureRedirectionURL :: Lens.Lens' CustomVerificationEmailTemplate (Prelude.Maybe Prelude.Text)
customVerificationEmailTemplate_failureRedirectionURL = Lens.lens (\CustomVerificationEmailTemplate' {failureRedirectionURL} -> failureRedirectionURL) (\s@CustomVerificationEmailTemplate' {} a -> s {failureRedirectionURL = a} :: CustomVerificationEmailTemplate)

instance
  Prelude.FromXML
    CustomVerificationEmailTemplate
  where
  parseXML x =
    CustomVerificationEmailTemplate'
      Prelude.<$> (x Prelude..@? "TemplateName")
      Prelude.<*> (x Prelude..@? "TemplateSubject")
      Prelude.<*> (x Prelude..@? "FromEmailAddress")
      Prelude.<*> (x Prelude..@? "SuccessRedirectionURL")
      Prelude.<*> (x Prelude..@? "FailureRedirectionURL")

instance
  Prelude.Hashable
    CustomVerificationEmailTemplate

instance
  Prelude.NFData
    CustomVerificationEmailTemplate
