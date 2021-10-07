{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.GetRandomPassword
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a random password of the specified complexity. This operation
-- is intended for use in the Lambda rotation function. Per best practice,
-- we recommend that you specify the maximum length and include every
-- character type that the system you are generating a password for can
-- support.
--
-- __Minimum permissions__
--
-- To run this command, you must have the following permissions:
--
-- -   secretsmanager:GetRandomPassword
module Network.AWS.SecretsManager.GetRandomPassword
  ( -- * Creating a Request
    GetRandomPassword (..),
    newGetRandomPassword,

    -- * Request Lenses
    getRandomPassword_excludeCharacters,
    getRandomPassword_excludeLowercase,
    getRandomPassword_includeSpace,
    getRandomPassword_requireEachIncludedType,
    getRandomPassword_excludeNumbers,
    getRandomPassword_passwordLength,
    getRandomPassword_excludeUppercase,
    getRandomPassword_excludePunctuation,

    -- * Destructuring the Response
    GetRandomPasswordResponse (..),
    newGetRandomPasswordResponse,

    -- * Response Lenses
    getRandomPasswordResponse_randomPassword,
    getRandomPasswordResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SecretsManager.Types

-- | /See:/ 'newGetRandomPassword' smart constructor.
data GetRandomPassword = GetRandomPassword'
  { -- | A string that includes characters that should not be included in the
    -- generated password. The default is that all characters from the included
    -- sets can be used.
    excludeCharacters :: Prelude.Maybe Prelude.Text,
    -- | Specifies that the generated password should not include lowercase
    -- letters. The default if you do not include this switch parameter is that
    -- lowercase letters can be included.
    excludeLowercase :: Prelude.Maybe Prelude.Bool,
    -- | Specifies that the generated password can include the space character.
    -- The default if you do not include this switch parameter is that the
    -- space character is not included.
    includeSpace :: Prelude.Maybe Prelude.Bool,
    -- | A boolean value that specifies whether the generated password must
    -- include at least one of every allowed character type. The default value
    -- is @True@ and the operation requires at least one of every character
    -- type.
    requireEachIncludedType :: Prelude.Maybe Prelude.Bool,
    -- | Specifies that the generated password should not include digits. The
    -- default if you do not include this switch parameter is that digits can
    -- be included.
    excludeNumbers :: Prelude.Maybe Prelude.Bool,
    -- | The desired length of the generated password. The default value if you
    -- do not include this parameter is 32 characters.
    passwordLength :: Prelude.Maybe Prelude.Natural,
    -- | Specifies that the generated password should not include uppercase
    -- letters. The default if you do not include this switch parameter is that
    -- uppercase letters can be included.
    excludeUppercase :: Prelude.Maybe Prelude.Bool,
    -- | Specifies that the generated password should not include punctuation
    -- characters. The default if you do not include this switch parameter is
    -- that punctuation characters can be included.
    --
    -- The following are the punctuation characters that /can/ be included in
    -- the generated password if you don\'t explicitly exclude them with
    -- @ExcludeCharacters@ or @ExcludePunctuation@:
    --
    -- @! \" # $ % & \' ( ) * + , - . \/ : ; \< = > ? \@ [ \\ ] ^ _ \` { | } ~@
    excludePunctuation :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRandomPassword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'excludeCharacters', 'getRandomPassword_excludeCharacters' - A string that includes characters that should not be included in the
-- generated password. The default is that all characters from the included
-- sets can be used.
--
-- 'excludeLowercase', 'getRandomPassword_excludeLowercase' - Specifies that the generated password should not include lowercase
-- letters. The default if you do not include this switch parameter is that
-- lowercase letters can be included.
--
-- 'includeSpace', 'getRandomPassword_includeSpace' - Specifies that the generated password can include the space character.
-- The default if you do not include this switch parameter is that the
-- space character is not included.
--
-- 'requireEachIncludedType', 'getRandomPassword_requireEachIncludedType' - A boolean value that specifies whether the generated password must
-- include at least one of every allowed character type. The default value
-- is @True@ and the operation requires at least one of every character
-- type.
--
-- 'excludeNumbers', 'getRandomPassword_excludeNumbers' - Specifies that the generated password should not include digits. The
-- default if you do not include this switch parameter is that digits can
-- be included.
--
-- 'passwordLength', 'getRandomPassword_passwordLength' - The desired length of the generated password. The default value if you
-- do not include this parameter is 32 characters.
--
-- 'excludeUppercase', 'getRandomPassword_excludeUppercase' - Specifies that the generated password should not include uppercase
-- letters. The default if you do not include this switch parameter is that
-- uppercase letters can be included.
--
-- 'excludePunctuation', 'getRandomPassword_excludePunctuation' - Specifies that the generated password should not include punctuation
-- characters. The default if you do not include this switch parameter is
-- that punctuation characters can be included.
--
-- The following are the punctuation characters that /can/ be included in
-- the generated password if you don\'t explicitly exclude them with
-- @ExcludeCharacters@ or @ExcludePunctuation@:
--
-- @! \" # $ % & \' ( ) * + , - . \/ : ; \< = > ? \@ [ \\ ] ^ _ \` { | } ~@
newGetRandomPassword ::
  GetRandomPassword
newGetRandomPassword =
  GetRandomPassword'
    { excludeCharacters =
        Prelude.Nothing,
      excludeLowercase = Prelude.Nothing,
      includeSpace = Prelude.Nothing,
      requireEachIncludedType = Prelude.Nothing,
      excludeNumbers = Prelude.Nothing,
      passwordLength = Prelude.Nothing,
      excludeUppercase = Prelude.Nothing,
      excludePunctuation = Prelude.Nothing
    }

-- | A string that includes characters that should not be included in the
-- generated password. The default is that all characters from the included
-- sets can be used.
getRandomPassword_excludeCharacters :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Text)
getRandomPassword_excludeCharacters = Lens.lens (\GetRandomPassword' {excludeCharacters} -> excludeCharacters) (\s@GetRandomPassword' {} a -> s {excludeCharacters = a} :: GetRandomPassword)

-- | Specifies that the generated password should not include lowercase
-- letters. The default if you do not include this switch parameter is that
-- lowercase letters can be included.
getRandomPassword_excludeLowercase :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Bool)
getRandomPassword_excludeLowercase = Lens.lens (\GetRandomPassword' {excludeLowercase} -> excludeLowercase) (\s@GetRandomPassword' {} a -> s {excludeLowercase = a} :: GetRandomPassword)

-- | Specifies that the generated password can include the space character.
-- The default if you do not include this switch parameter is that the
-- space character is not included.
getRandomPassword_includeSpace :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Bool)
getRandomPassword_includeSpace = Lens.lens (\GetRandomPassword' {includeSpace} -> includeSpace) (\s@GetRandomPassword' {} a -> s {includeSpace = a} :: GetRandomPassword)

-- | A boolean value that specifies whether the generated password must
-- include at least one of every allowed character type. The default value
-- is @True@ and the operation requires at least one of every character
-- type.
getRandomPassword_requireEachIncludedType :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Bool)
getRandomPassword_requireEachIncludedType = Lens.lens (\GetRandomPassword' {requireEachIncludedType} -> requireEachIncludedType) (\s@GetRandomPassword' {} a -> s {requireEachIncludedType = a} :: GetRandomPassword)

-- | Specifies that the generated password should not include digits. The
-- default if you do not include this switch parameter is that digits can
-- be included.
getRandomPassword_excludeNumbers :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Bool)
getRandomPassword_excludeNumbers = Lens.lens (\GetRandomPassword' {excludeNumbers} -> excludeNumbers) (\s@GetRandomPassword' {} a -> s {excludeNumbers = a} :: GetRandomPassword)

-- | The desired length of the generated password. The default value if you
-- do not include this parameter is 32 characters.
getRandomPassword_passwordLength :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Natural)
getRandomPassword_passwordLength = Lens.lens (\GetRandomPassword' {passwordLength} -> passwordLength) (\s@GetRandomPassword' {} a -> s {passwordLength = a} :: GetRandomPassword)

-- | Specifies that the generated password should not include uppercase
-- letters. The default if you do not include this switch parameter is that
-- uppercase letters can be included.
getRandomPassword_excludeUppercase :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Bool)
getRandomPassword_excludeUppercase = Lens.lens (\GetRandomPassword' {excludeUppercase} -> excludeUppercase) (\s@GetRandomPassword' {} a -> s {excludeUppercase = a} :: GetRandomPassword)

-- | Specifies that the generated password should not include punctuation
-- characters. The default if you do not include this switch parameter is
-- that punctuation characters can be included.
--
-- The following are the punctuation characters that /can/ be included in
-- the generated password if you don\'t explicitly exclude them with
-- @ExcludeCharacters@ or @ExcludePunctuation@:
--
-- @! \" # $ % & \' ( ) * + , - . \/ : ; \< = > ? \@ [ \\ ] ^ _ \` { | } ~@
getRandomPassword_excludePunctuation :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Bool)
getRandomPassword_excludePunctuation = Lens.lens (\GetRandomPassword' {excludePunctuation} -> excludePunctuation) (\s@GetRandomPassword' {} a -> s {excludePunctuation = a} :: GetRandomPassword)

instance Core.AWSRequest GetRandomPassword where
  type
    AWSResponse GetRandomPassword =
      GetRandomPasswordResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRandomPasswordResponse'
            Prelude.<$> (x Core..?> "RandomPassword")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRandomPassword

instance Prelude.NFData GetRandomPassword

instance Core.ToHeaders GetRandomPassword where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "secretsmanager.GetRandomPassword" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetRandomPassword where
  toJSON GetRandomPassword' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ExcludeCharacters" Core..=)
              Prelude.<$> excludeCharacters,
            ("ExcludeLowercase" Core..=)
              Prelude.<$> excludeLowercase,
            ("IncludeSpace" Core..=) Prelude.<$> includeSpace,
            ("RequireEachIncludedType" Core..=)
              Prelude.<$> requireEachIncludedType,
            ("ExcludeNumbers" Core..=)
              Prelude.<$> excludeNumbers,
            ("PasswordLength" Core..=)
              Prelude.<$> passwordLength,
            ("ExcludeUppercase" Core..=)
              Prelude.<$> excludeUppercase,
            ("ExcludePunctuation" Core..=)
              Prelude.<$> excludePunctuation
          ]
      )

instance Core.ToPath GetRandomPassword where
  toPath = Prelude.const "/"

instance Core.ToQuery GetRandomPassword where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRandomPasswordResponse' smart constructor.
data GetRandomPasswordResponse = GetRandomPasswordResponse'
  { -- | A string with the generated password.
    randomPassword :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetRandomPasswordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'randomPassword', 'getRandomPasswordResponse_randomPassword' - A string with the generated password.
--
-- 'httpStatus', 'getRandomPasswordResponse_httpStatus' - The response's http status code.
newGetRandomPasswordResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetRandomPasswordResponse
newGetRandomPasswordResponse pHttpStatus_ =
  GetRandomPasswordResponse'
    { randomPassword =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A string with the generated password.
getRandomPasswordResponse_randomPassword :: Lens.Lens' GetRandomPasswordResponse (Prelude.Maybe Prelude.Text)
getRandomPasswordResponse_randomPassword = Lens.lens (\GetRandomPasswordResponse' {randomPassword} -> randomPassword) (\s@GetRandomPasswordResponse' {} a -> s {randomPassword = a} :: GetRandomPasswordResponse) Prelude.. Lens.mapping Core._Sensitive

-- | The response's http status code.
getRandomPasswordResponse_httpStatus :: Lens.Lens' GetRandomPasswordResponse Prelude.Int
getRandomPasswordResponse_httpStatus = Lens.lens (\GetRandomPasswordResponse' {httpStatus} -> httpStatus) (\s@GetRandomPasswordResponse' {} a -> s {httpStatus = a} :: GetRandomPasswordResponse)

instance Prelude.NFData GetRandomPasswordResponse
