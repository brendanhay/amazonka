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
-- Module      : Amazonka.WorkSpaces.Types.DefaultImportClientBrandingAttributes
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.DefaultImportClientBrandingAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The default client branding attributes to be imported. These attributes
-- display on the client login screen.
--
-- Client branding attributes are public facing. Ensure that you do not
-- include sensitive information.
--
-- /See:/ 'newDefaultImportClientBrandingAttributes' smart constructor.
data DefaultImportClientBrandingAttributes = DefaultImportClientBrandingAttributes'
  { -- | The forgotten password link. This is the web address that users can go
    -- to if they forget the password for their WorkSpace.
    forgotPasswordLink :: Prelude.Maybe Prelude.Text,
    -- | The login message. Specified as a key value pair, in which the key is a
    -- locale and the value is the localized message for that locale. The only
    -- key supported is @en_US@. The HTML tags supported include the following:
    -- @a, b, blockquote, br, cite, code, dd, dl, dt, div, em, i, li, ol, p, pre, q, small, span, strike, strong, sub, sup, u, ul@.
    loginMessage :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The logo. The only image format accepted is a binary data object that is
    -- converted from a @.png@ file.
    logo :: Prelude.Maybe Data.Base64,
    -- | The support email. The company\'s customer support email address.
    --
    -- -   In each platform type, the @SupportEmail@ and @SupportLink@
    --     parameters are mutually exclusive. You can specify one parameter for
    --     each platform type, but not both.
    --
    -- -   The default email is @workspaces-feedback\@amazon.com@.
    supportEmail :: Prelude.Maybe Prelude.Text,
    -- | The support link. The link for the company\'s customer support page for
    -- their WorkSpace.
    --
    -- -   In each platform type, the @SupportEmail@ and @SupportLink@
    --     parameters are mutually exclusive. You can specify one parameter for
    --     each platform type, but not both.
    --
    -- -   The default support link is @workspaces-feedback\@amazon.com@.
    supportLink :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultImportClientBrandingAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forgotPasswordLink', 'defaultImportClientBrandingAttributes_forgotPasswordLink' - The forgotten password link. This is the web address that users can go
-- to if they forget the password for their WorkSpace.
--
-- 'loginMessage', 'defaultImportClientBrandingAttributes_loginMessage' - The login message. Specified as a key value pair, in which the key is a
-- locale and the value is the localized message for that locale. The only
-- key supported is @en_US@. The HTML tags supported include the following:
-- @a, b, blockquote, br, cite, code, dd, dl, dt, div, em, i, li, ol, p, pre, q, small, span, strike, strong, sub, sup, u, ul@.
--
-- 'logo', 'defaultImportClientBrandingAttributes_logo' - The logo. The only image format accepted is a binary data object that is
-- converted from a @.png@ file.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
--
-- 'supportEmail', 'defaultImportClientBrandingAttributes_supportEmail' - The support email. The company\'s customer support email address.
--
-- -   In each platform type, the @SupportEmail@ and @SupportLink@
--     parameters are mutually exclusive. You can specify one parameter for
--     each platform type, but not both.
--
-- -   The default email is @workspaces-feedback\@amazon.com@.
--
-- 'supportLink', 'defaultImportClientBrandingAttributes_supportLink' - The support link. The link for the company\'s customer support page for
-- their WorkSpace.
--
-- -   In each platform type, the @SupportEmail@ and @SupportLink@
--     parameters are mutually exclusive. You can specify one parameter for
--     each platform type, but not both.
--
-- -   The default support link is @workspaces-feedback\@amazon.com@.
newDefaultImportClientBrandingAttributes ::
  DefaultImportClientBrandingAttributes
newDefaultImportClientBrandingAttributes =
  DefaultImportClientBrandingAttributes'
    { forgotPasswordLink =
        Prelude.Nothing,
      loginMessage = Prelude.Nothing,
      logo = Prelude.Nothing,
      supportEmail = Prelude.Nothing,
      supportLink = Prelude.Nothing
    }

-- | The forgotten password link. This is the web address that users can go
-- to if they forget the password for their WorkSpace.
defaultImportClientBrandingAttributes_forgotPasswordLink :: Lens.Lens' DefaultImportClientBrandingAttributes (Prelude.Maybe Prelude.Text)
defaultImportClientBrandingAttributes_forgotPasswordLink = Lens.lens (\DefaultImportClientBrandingAttributes' {forgotPasswordLink} -> forgotPasswordLink) (\s@DefaultImportClientBrandingAttributes' {} a -> s {forgotPasswordLink = a} :: DefaultImportClientBrandingAttributes)

-- | The login message. Specified as a key value pair, in which the key is a
-- locale and the value is the localized message for that locale. The only
-- key supported is @en_US@. The HTML tags supported include the following:
-- @a, b, blockquote, br, cite, code, dd, dl, dt, div, em, i, li, ol, p, pre, q, small, span, strike, strong, sub, sup, u, ul@.
defaultImportClientBrandingAttributes_loginMessage :: Lens.Lens' DefaultImportClientBrandingAttributes (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
defaultImportClientBrandingAttributes_loginMessage = Lens.lens (\DefaultImportClientBrandingAttributes' {loginMessage} -> loginMessage) (\s@DefaultImportClientBrandingAttributes' {} a -> s {loginMessage = a} :: DefaultImportClientBrandingAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The logo. The only image format accepted is a binary data object that is
-- converted from a @.png@ file.--
-- -- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- -- The underlying isomorphism will encode to Base64 representation during
-- -- serialisation, and decode from Base64 representation during deserialisation.
-- -- This 'Lens' accepts and returns only raw unencoded data.
defaultImportClientBrandingAttributes_logo :: Lens.Lens' DefaultImportClientBrandingAttributes (Prelude.Maybe Prelude.ByteString)
defaultImportClientBrandingAttributes_logo = Lens.lens (\DefaultImportClientBrandingAttributes' {logo} -> logo) (\s@DefaultImportClientBrandingAttributes' {} a -> s {logo = a} :: DefaultImportClientBrandingAttributes) Prelude.. Lens.mapping Data._Base64

-- | The support email. The company\'s customer support email address.
--
-- -   In each platform type, the @SupportEmail@ and @SupportLink@
--     parameters are mutually exclusive. You can specify one parameter for
--     each platform type, but not both.
--
-- -   The default email is @workspaces-feedback\@amazon.com@.
defaultImportClientBrandingAttributes_supportEmail :: Lens.Lens' DefaultImportClientBrandingAttributes (Prelude.Maybe Prelude.Text)
defaultImportClientBrandingAttributes_supportEmail = Lens.lens (\DefaultImportClientBrandingAttributes' {supportEmail} -> supportEmail) (\s@DefaultImportClientBrandingAttributes' {} a -> s {supportEmail = a} :: DefaultImportClientBrandingAttributes)

-- | The support link. The link for the company\'s customer support page for
-- their WorkSpace.
--
-- -   In each platform type, the @SupportEmail@ and @SupportLink@
--     parameters are mutually exclusive. You can specify one parameter for
--     each platform type, but not both.
--
-- -   The default support link is @workspaces-feedback\@amazon.com@.
defaultImportClientBrandingAttributes_supportLink :: Lens.Lens' DefaultImportClientBrandingAttributes (Prelude.Maybe Prelude.Text)
defaultImportClientBrandingAttributes_supportLink = Lens.lens (\DefaultImportClientBrandingAttributes' {supportLink} -> supportLink) (\s@DefaultImportClientBrandingAttributes' {} a -> s {supportLink = a} :: DefaultImportClientBrandingAttributes)

instance
  Prelude.Hashable
    DefaultImportClientBrandingAttributes
  where
  hashWithSalt
    _salt
    DefaultImportClientBrandingAttributes' {..} =
      _salt
        `Prelude.hashWithSalt` forgotPasswordLink
        `Prelude.hashWithSalt` loginMessage
        `Prelude.hashWithSalt` logo
        `Prelude.hashWithSalt` supportEmail
        `Prelude.hashWithSalt` supportLink

instance
  Prelude.NFData
    DefaultImportClientBrandingAttributes
  where
  rnf DefaultImportClientBrandingAttributes' {..} =
    Prelude.rnf forgotPasswordLink `Prelude.seq`
      Prelude.rnf loginMessage `Prelude.seq`
        Prelude.rnf logo `Prelude.seq`
          Prelude.rnf supportEmail `Prelude.seq`
            Prelude.rnf supportLink

instance
  Data.ToJSON
    DefaultImportClientBrandingAttributes
  where
  toJSON DefaultImportClientBrandingAttributes' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ForgotPasswordLink" Data..=)
              Prelude.<$> forgotPasswordLink,
            ("LoginMessage" Data..=) Prelude.<$> loginMessage,
            ("Logo" Data..=) Prelude.<$> logo,
            ("SupportEmail" Data..=) Prelude.<$> supportEmail,
            ("SupportLink" Data..=) Prelude.<$> supportLink
          ]
      )
