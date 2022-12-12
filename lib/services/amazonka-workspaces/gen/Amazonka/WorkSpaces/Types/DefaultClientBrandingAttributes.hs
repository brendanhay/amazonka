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
-- Module      : Amazonka.WorkSpaces.Types.DefaultClientBrandingAttributes
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.WorkSpaces.Types.DefaultClientBrandingAttributes where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Returns default client branding attributes that were imported. These
-- attributes display on the client login screen.
--
-- Client branding attributes are public facing. Ensure that you don\'t
-- include sensitive information.
--
-- /See:/ 'newDefaultClientBrandingAttributes' smart constructor.
data DefaultClientBrandingAttributes = DefaultClientBrandingAttributes'
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
    logoUrl :: Prelude.Maybe Prelude.Text,
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
    --     parameters are mutually exclusive.You can specify one parameter for
    --     each platform type, but not both.
    --
    -- -   The default support link is @workspaces-feedback\@amazon.com@.
    supportLink :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DefaultClientBrandingAttributes' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'forgotPasswordLink', 'defaultClientBrandingAttributes_forgotPasswordLink' - The forgotten password link. This is the web address that users can go
-- to if they forget the password for their WorkSpace.
--
-- 'loginMessage', 'defaultClientBrandingAttributes_loginMessage' - The login message. Specified as a key value pair, in which the key is a
-- locale and the value is the localized message for that locale. The only
-- key supported is @en_US@. The HTML tags supported include the following:
-- @a, b, blockquote, br, cite, code, dd, dl, dt, div, em, i, li, ol, p, pre, q, small, span, strike, strong, sub, sup, u, ul@.
--
-- 'logoUrl', 'defaultClientBrandingAttributes_logoUrl' - The logo. The only image format accepted is a binary data object that is
-- converted from a @.png@ file.
--
-- 'supportEmail', 'defaultClientBrandingAttributes_supportEmail' - The support email. The company\'s customer support email address.
--
-- -   In each platform type, the @SupportEmail@ and @SupportLink@
--     parameters are mutually exclusive. You can specify one parameter for
--     each platform type, but not both.
--
-- -   The default email is @workspaces-feedback\@amazon.com@.
--
-- 'supportLink', 'defaultClientBrandingAttributes_supportLink' - The support link. The link for the company\'s customer support page for
-- their WorkSpace.
--
-- -   In each platform type, the @SupportEmail@ and @SupportLink@
--     parameters are mutually exclusive.You can specify one parameter for
--     each platform type, but not both.
--
-- -   The default support link is @workspaces-feedback\@amazon.com@.
newDefaultClientBrandingAttributes ::
  DefaultClientBrandingAttributes
newDefaultClientBrandingAttributes =
  DefaultClientBrandingAttributes'
    { forgotPasswordLink =
        Prelude.Nothing,
      loginMessage = Prelude.Nothing,
      logoUrl = Prelude.Nothing,
      supportEmail = Prelude.Nothing,
      supportLink = Prelude.Nothing
    }

-- | The forgotten password link. This is the web address that users can go
-- to if they forget the password for their WorkSpace.
defaultClientBrandingAttributes_forgotPasswordLink :: Lens.Lens' DefaultClientBrandingAttributes (Prelude.Maybe Prelude.Text)
defaultClientBrandingAttributes_forgotPasswordLink = Lens.lens (\DefaultClientBrandingAttributes' {forgotPasswordLink} -> forgotPasswordLink) (\s@DefaultClientBrandingAttributes' {} a -> s {forgotPasswordLink = a} :: DefaultClientBrandingAttributes)

-- | The login message. Specified as a key value pair, in which the key is a
-- locale and the value is the localized message for that locale. The only
-- key supported is @en_US@. The HTML tags supported include the following:
-- @a, b, blockquote, br, cite, code, dd, dl, dt, div, em, i, li, ol, p, pre, q, small, span, strike, strong, sub, sup, u, ul@.
defaultClientBrandingAttributes_loginMessage :: Lens.Lens' DefaultClientBrandingAttributes (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
defaultClientBrandingAttributes_loginMessage = Lens.lens (\DefaultClientBrandingAttributes' {loginMessage} -> loginMessage) (\s@DefaultClientBrandingAttributes' {} a -> s {loginMessage = a} :: DefaultClientBrandingAttributes) Prelude.. Lens.mapping Lens.coerced

-- | The logo. The only image format accepted is a binary data object that is
-- converted from a @.png@ file.
defaultClientBrandingAttributes_logoUrl :: Lens.Lens' DefaultClientBrandingAttributes (Prelude.Maybe Prelude.Text)
defaultClientBrandingAttributes_logoUrl = Lens.lens (\DefaultClientBrandingAttributes' {logoUrl} -> logoUrl) (\s@DefaultClientBrandingAttributes' {} a -> s {logoUrl = a} :: DefaultClientBrandingAttributes)

-- | The support email. The company\'s customer support email address.
--
-- -   In each platform type, the @SupportEmail@ and @SupportLink@
--     parameters are mutually exclusive. You can specify one parameter for
--     each platform type, but not both.
--
-- -   The default email is @workspaces-feedback\@amazon.com@.
defaultClientBrandingAttributes_supportEmail :: Lens.Lens' DefaultClientBrandingAttributes (Prelude.Maybe Prelude.Text)
defaultClientBrandingAttributes_supportEmail = Lens.lens (\DefaultClientBrandingAttributes' {supportEmail} -> supportEmail) (\s@DefaultClientBrandingAttributes' {} a -> s {supportEmail = a} :: DefaultClientBrandingAttributes)

-- | The support link. The link for the company\'s customer support page for
-- their WorkSpace.
--
-- -   In each platform type, the @SupportEmail@ and @SupportLink@
--     parameters are mutually exclusive.You can specify one parameter for
--     each platform type, but not both.
--
-- -   The default support link is @workspaces-feedback\@amazon.com@.
defaultClientBrandingAttributes_supportLink :: Lens.Lens' DefaultClientBrandingAttributes (Prelude.Maybe Prelude.Text)
defaultClientBrandingAttributes_supportLink = Lens.lens (\DefaultClientBrandingAttributes' {supportLink} -> supportLink) (\s@DefaultClientBrandingAttributes' {} a -> s {supportLink = a} :: DefaultClientBrandingAttributes)

instance
  Data.FromJSON
    DefaultClientBrandingAttributes
  where
  parseJSON =
    Data.withObject
      "DefaultClientBrandingAttributes"
      ( \x ->
          DefaultClientBrandingAttributes'
            Prelude.<$> (x Data..:? "ForgotPasswordLink")
            Prelude.<*> (x Data..:? "LoginMessage" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "LogoUrl")
            Prelude.<*> (x Data..:? "SupportEmail")
            Prelude.<*> (x Data..:? "SupportLink")
      )

instance
  Prelude.Hashable
    DefaultClientBrandingAttributes
  where
  hashWithSalt
    _salt
    DefaultClientBrandingAttributes' {..} =
      _salt `Prelude.hashWithSalt` forgotPasswordLink
        `Prelude.hashWithSalt` loginMessage
        `Prelude.hashWithSalt` logoUrl
        `Prelude.hashWithSalt` supportEmail
        `Prelude.hashWithSalt` supportLink

instance
  Prelude.NFData
    DefaultClientBrandingAttributes
  where
  rnf DefaultClientBrandingAttributes' {..} =
    Prelude.rnf forgotPasswordLink
      `Prelude.seq` Prelude.rnf loginMessage
      `Prelude.seq` Prelude.rnf logoUrl
      `Prelude.seq` Prelude.rnf supportEmail
      `Prelude.seq` Prelude.rnf supportLink
