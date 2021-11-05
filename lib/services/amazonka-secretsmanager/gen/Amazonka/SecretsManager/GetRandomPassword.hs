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
-- Module      : Amazonka.SecretsManager.GetRandomPassword
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
module Amazonka.SecretsManager.GetRandomPassword
  ( -- * Creating a Request
    GetRandomPassword (..),
    newGetRandomPassword,

    -- * Request Lenses
    getRandomPassword_includeSpace,
    getRandomPassword_excludeNumbers,
    getRandomPassword_excludeLowercase,
    getRandomPassword_excludeCharacters,
    getRandomPassword_excludePunctuation,
    getRandomPassword_requireEachIncludedType,
    getRandomPassword_excludeUppercase,
    getRandomPassword_passwordLength,

    -- * Destructuring the Response
    GetRandomPasswordResponse (..),
    newGetRandomPasswordResponse,

    -- * Response Lenses
    getRandomPasswordResponse_randomPassword,
    getRandomPasswordResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newGetRandomPassword' smart constructor.
data GetRandomPassword = GetRandomPassword'
  { -- | Specifies that the generated password can include the space character.
    -- The default if you do not include this switch parameter is that the
    -- space character is not included.
    includeSpace :: Prelude.Maybe Prelude.Bool,
    -- | Specifies that the generated password should not include digits. The
    -- default if you do not include this switch parameter is that digits can
    -- be included.
    excludeNumbers :: Prelude.Maybe Prelude.Bool,
    -- | Specifies that the generated password should not include lowercase
    -- letters. The default if you do not include this switch parameter is that
    -- lowercase letters can be included.
    excludeLowercase :: Prelude.Maybe Prelude.Bool,
    -- | A string that includes characters that should not be included in the
    -- generated password. The default is that all characters from the included
    -- sets can be used.
    excludeCharacters :: Prelude.Maybe Prelude.Text,
    -- | Specifies that the generated password should not include punctuation
    -- characters. The default if you do not include this switch parameter is
    -- that punctuation characters can be included.
    --
    -- The following are the punctuation characters that /can/ be included in
    -- the generated password if you don\'t explicitly exclude them with
    -- @ExcludeCharacters@ or @ExcludePunctuation@:
    --
    -- @! \" # $ % & \' ( ) * + , - . \/ : ; \< = > ? \@ [ \\ ] ^ _ \` { | } ~@
    excludePunctuation :: Prelude.Maybe Prelude.Bool,
    -- | A boolean value that specifies whether the generated password must
    -- include at least one of every allowed character type. The default value
    -- is @True@ and the operation requires at least one of every character
    -- type.
    requireEachIncludedType :: Prelude.Maybe Prelude.Bool,
    -- | Specifies that the generated password should not include uppercase
    -- letters. The default if you do not include this switch parameter is that
    -- uppercase letters can be included.
    excludeUppercase :: Prelude.Maybe Prelude.Bool,
    -- | The desired length of the generated password. The default value if you
    -- do not include this parameter is 32 characters.
    passwordLength :: Prelude.Maybe Prelude.Natural
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
-- 'includeSpace', 'getRandomPassword_includeSpace' - Specifies that the generated password can include the space character.
-- The default if you do not include this switch parameter is that the
-- space character is not included.
--
-- 'excludeNumbers', 'getRandomPassword_excludeNumbers' - Specifies that the generated password should not include digits. The
-- default if you do not include this switch parameter is that digits can
-- be included.
--
-- 'excludeLowercase', 'getRandomPassword_excludeLowercase' - Specifies that the generated password should not include lowercase
-- letters. The default if you do not include this switch parameter is that
-- lowercase letters can be included.
--
-- 'excludeCharacters', 'getRandomPassword_excludeCharacters' - A string that includes characters that should not be included in the
-- generated password. The default is that all characters from the included
-- sets can be used.
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
--
-- 'requireEachIncludedType', 'getRandomPassword_requireEachIncludedType' - A boolean value that specifies whether the generated password must
-- include at least one of every allowed character type. The default value
-- is @True@ and the operation requires at least one of every character
-- type.
--
-- 'excludeUppercase', 'getRandomPassword_excludeUppercase' - Specifies that the generated password should not include uppercase
-- letters. The default if you do not include this switch parameter is that
-- uppercase letters can be included.
--
-- 'passwordLength', 'getRandomPassword_passwordLength' - The desired length of the generated password. The default value if you
-- do not include this parameter is 32 characters.
newGetRandomPassword ::
  GetRandomPassword
newGetRandomPassword =
  GetRandomPassword'
    { includeSpace = Prelude.Nothing,
      excludeNumbers = Prelude.Nothing,
      excludeLowercase = Prelude.Nothing,
      excludeCharacters = Prelude.Nothing,
      excludePunctuation = Prelude.Nothing,
      requireEachIncludedType = Prelude.Nothing,
      excludeUppercase = Prelude.Nothing,
      passwordLength = Prelude.Nothing
    }

-- | Specifies that the generated password can include the space character.
-- The default if you do not include this switch parameter is that the
-- space character is not included.
getRandomPassword_includeSpace :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Bool)
getRandomPassword_includeSpace = Lens.lens (\GetRandomPassword' {includeSpace} -> includeSpace) (\s@GetRandomPassword' {} a -> s {includeSpace = a} :: GetRandomPassword)

-- | Specifies that the generated password should not include digits. The
-- default if you do not include this switch parameter is that digits can
-- be included.
getRandomPassword_excludeNumbers :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Bool)
getRandomPassword_excludeNumbers = Lens.lens (\GetRandomPassword' {excludeNumbers} -> excludeNumbers) (\s@GetRandomPassword' {} a -> s {excludeNumbers = a} :: GetRandomPassword)

-- | Specifies that the generated password should not include lowercase
-- letters. The default if you do not include this switch parameter is that
-- lowercase letters can be included.
getRandomPassword_excludeLowercase :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Bool)
getRandomPassword_excludeLowercase = Lens.lens (\GetRandomPassword' {excludeLowercase} -> excludeLowercase) (\s@GetRandomPassword' {} a -> s {excludeLowercase = a} :: GetRandomPassword)

-- | A string that includes characters that should not be included in the
-- generated password. The default is that all characters from the included
-- sets can be used.
getRandomPassword_excludeCharacters :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Text)
getRandomPassword_excludeCharacters = Lens.lens (\GetRandomPassword' {excludeCharacters} -> excludeCharacters) (\s@GetRandomPassword' {} a -> s {excludeCharacters = a} :: GetRandomPassword)

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

-- | A boolean value that specifies whether the generated password must
-- include at least one of every allowed character type. The default value
-- is @True@ and the operation requires at least one of every character
-- type.
getRandomPassword_requireEachIncludedType :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Bool)
getRandomPassword_requireEachIncludedType = Lens.lens (\GetRandomPassword' {requireEachIncludedType} -> requireEachIncludedType) (\s@GetRandomPassword' {} a -> s {requireEachIncludedType = a} :: GetRandomPassword)

-- | Specifies that the generated password should not include uppercase
-- letters. The default if you do not include this switch parameter is that
-- uppercase letters can be included.
getRandomPassword_excludeUppercase :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Bool)
getRandomPassword_excludeUppercase = Lens.lens (\GetRandomPassword' {excludeUppercase} -> excludeUppercase) (\s@GetRandomPassword' {} a -> s {excludeUppercase = a} :: GetRandomPassword)

-- | The desired length of the generated password. The default value if you
-- do not include this parameter is 32 characters.
getRandomPassword_passwordLength :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Natural)
getRandomPassword_passwordLength = Lens.lens (\GetRandomPassword' {passwordLength} -> passwordLength) (\s@GetRandomPassword' {} a -> s {passwordLength = a} :: GetRandomPassword)

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
          [ ("IncludeSpace" Core..=) Prelude.<$> includeSpace,
            ("ExcludeNumbers" Core..=)
              Prelude.<$> excludeNumbers,
            ("ExcludeLowercase" Core..=)
              Prelude.<$> excludeLowercase,
            ("ExcludeCharacters" Core..=)
              Prelude.<$> excludeCharacters,
            ("ExcludePunctuation" Core..=)
              Prelude.<$> excludePunctuation,
            ("RequireEachIncludedType" Core..=)
              Prelude.<$> requireEachIncludedType,
            ("ExcludeUppercase" Core..=)
              Prelude.<$> excludeUppercase,
            ("PasswordLength" Core..=)
              Prelude.<$> passwordLength
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
