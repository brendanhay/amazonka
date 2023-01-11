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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a random password. We recommend that you specify the maximum
-- length and include every character type that the system you are
-- generating a password for can support.
--
-- Secrets Manager generates a CloudTrail log entry when you call this
-- action. Do not include sensitive information in request parameters
-- because it might be logged. For more information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/retrieve-ct-entries.html Logging Secrets Manager events with CloudTrail>.
--
-- __Required permissions:__ @secretsmanager:GetRandomPassword@. For more
-- information, see
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/reference_iam-permissions.html#reference_iam-permissions_actions IAM policy actions for Secrets Manager>
-- and
-- <https://docs.aws.amazon.com/secretsmanager/latest/userguide/auth-and-access.html Authentication and access control in Secrets Manager>.
module Amazonka.SecretsManager.GetRandomPassword
  ( -- * Creating a Request
    GetRandomPassword (..),
    newGetRandomPassword,

    -- * Request Lenses
    getRandomPassword_excludeCharacters,
    getRandomPassword_excludeLowercase,
    getRandomPassword_excludeNumbers,
    getRandomPassword_excludePunctuation,
    getRandomPassword_excludeUppercase,
    getRandomPassword_includeSpace,
    getRandomPassword_passwordLength,
    getRandomPassword_requireEachIncludedType,

    -- * Destructuring the Response
    GetRandomPasswordResponse (..),
    newGetRandomPasswordResponse,

    -- * Response Lenses
    getRandomPasswordResponse_randomPassword,
    getRandomPasswordResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecretsManager.Types

-- | /See:/ 'newGetRandomPassword' smart constructor.
data GetRandomPassword = GetRandomPassword'
  { -- | A string of the characters that you don\'t want in the password.
    excludeCharacters :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether to exclude lowercase letters from the password. If you
    -- don\'t include this switch, the password can contain lowercase letters.
    excludeLowercase :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether to exclude numbers from the password. If you don\'t
    -- include this switch, the password can contain numbers.
    excludeNumbers :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether to exclude the following punctuation characters from
    -- the password:
    -- @! \" # $ % & \' ( ) * + , - . \/ : ; \< = > ? \@ [ \\ ] ^ _ \` { | } ~@.
    -- If you don\'t include this switch, the password can contain punctuation.
    excludePunctuation :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether to exclude uppercase letters from the password. If you
    -- don\'t include this switch, the password can contain uppercase letters.
    excludeUppercase :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether to include the space character. If you include this
    -- switch, the password can contain space characters.
    includeSpace :: Prelude.Maybe Prelude.Bool,
    -- | The length of the password. If you don\'t include this parameter, the
    -- default length is 32 characters.
    passwordLength :: Prelude.Maybe Prelude.Natural,
    -- | Specifies whether to include at least one upper and lowercase letter,
    -- one number, and one punctuation. If you don\'t include this switch, the
    -- password contains at least one of every character type.
    requireEachIncludedType :: Prelude.Maybe Prelude.Bool
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
-- 'excludeCharacters', 'getRandomPassword_excludeCharacters' - A string of the characters that you don\'t want in the password.
--
-- 'excludeLowercase', 'getRandomPassword_excludeLowercase' - Specifies whether to exclude lowercase letters from the password. If you
-- don\'t include this switch, the password can contain lowercase letters.
--
-- 'excludeNumbers', 'getRandomPassword_excludeNumbers' - Specifies whether to exclude numbers from the password. If you don\'t
-- include this switch, the password can contain numbers.
--
-- 'excludePunctuation', 'getRandomPassword_excludePunctuation' - Specifies whether to exclude the following punctuation characters from
-- the password:
-- @! \" # $ % & \' ( ) * + , - . \/ : ; \< = > ? \@ [ \\ ] ^ _ \` { | } ~@.
-- If you don\'t include this switch, the password can contain punctuation.
--
-- 'excludeUppercase', 'getRandomPassword_excludeUppercase' - Specifies whether to exclude uppercase letters from the password. If you
-- don\'t include this switch, the password can contain uppercase letters.
--
-- 'includeSpace', 'getRandomPassword_includeSpace' - Specifies whether to include the space character. If you include this
-- switch, the password can contain space characters.
--
-- 'passwordLength', 'getRandomPassword_passwordLength' - The length of the password. If you don\'t include this parameter, the
-- default length is 32 characters.
--
-- 'requireEachIncludedType', 'getRandomPassword_requireEachIncludedType' - Specifies whether to include at least one upper and lowercase letter,
-- one number, and one punctuation. If you don\'t include this switch, the
-- password contains at least one of every character type.
newGetRandomPassword ::
  GetRandomPassword
newGetRandomPassword =
  GetRandomPassword'
    { excludeCharacters =
        Prelude.Nothing,
      excludeLowercase = Prelude.Nothing,
      excludeNumbers = Prelude.Nothing,
      excludePunctuation = Prelude.Nothing,
      excludeUppercase = Prelude.Nothing,
      includeSpace = Prelude.Nothing,
      passwordLength = Prelude.Nothing,
      requireEachIncludedType = Prelude.Nothing
    }

-- | A string of the characters that you don\'t want in the password.
getRandomPassword_excludeCharacters :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Text)
getRandomPassword_excludeCharacters = Lens.lens (\GetRandomPassword' {excludeCharacters} -> excludeCharacters) (\s@GetRandomPassword' {} a -> s {excludeCharacters = a} :: GetRandomPassword)

-- | Specifies whether to exclude lowercase letters from the password. If you
-- don\'t include this switch, the password can contain lowercase letters.
getRandomPassword_excludeLowercase :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Bool)
getRandomPassword_excludeLowercase = Lens.lens (\GetRandomPassword' {excludeLowercase} -> excludeLowercase) (\s@GetRandomPassword' {} a -> s {excludeLowercase = a} :: GetRandomPassword)

-- | Specifies whether to exclude numbers from the password. If you don\'t
-- include this switch, the password can contain numbers.
getRandomPassword_excludeNumbers :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Bool)
getRandomPassword_excludeNumbers = Lens.lens (\GetRandomPassword' {excludeNumbers} -> excludeNumbers) (\s@GetRandomPassword' {} a -> s {excludeNumbers = a} :: GetRandomPassword)

-- | Specifies whether to exclude the following punctuation characters from
-- the password:
-- @! \" # $ % & \' ( ) * + , - . \/ : ; \< = > ? \@ [ \\ ] ^ _ \` { | } ~@.
-- If you don\'t include this switch, the password can contain punctuation.
getRandomPassword_excludePunctuation :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Bool)
getRandomPassword_excludePunctuation = Lens.lens (\GetRandomPassword' {excludePunctuation} -> excludePunctuation) (\s@GetRandomPassword' {} a -> s {excludePunctuation = a} :: GetRandomPassword)

-- | Specifies whether to exclude uppercase letters from the password. If you
-- don\'t include this switch, the password can contain uppercase letters.
getRandomPassword_excludeUppercase :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Bool)
getRandomPassword_excludeUppercase = Lens.lens (\GetRandomPassword' {excludeUppercase} -> excludeUppercase) (\s@GetRandomPassword' {} a -> s {excludeUppercase = a} :: GetRandomPassword)

-- | Specifies whether to include the space character. If you include this
-- switch, the password can contain space characters.
getRandomPassword_includeSpace :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Bool)
getRandomPassword_includeSpace = Lens.lens (\GetRandomPassword' {includeSpace} -> includeSpace) (\s@GetRandomPassword' {} a -> s {includeSpace = a} :: GetRandomPassword)

-- | The length of the password. If you don\'t include this parameter, the
-- default length is 32 characters.
getRandomPassword_passwordLength :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Natural)
getRandomPassword_passwordLength = Lens.lens (\GetRandomPassword' {passwordLength} -> passwordLength) (\s@GetRandomPassword' {} a -> s {passwordLength = a} :: GetRandomPassword)

-- | Specifies whether to include at least one upper and lowercase letter,
-- one number, and one punctuation. If you don\'t include this switch, the
-- password contains at least one of every character type.
getRandomPassword_requireEachIncludedType :: Lens.Lens' GetRandomPassword (Prelude.Maybe Prelude.Bool)
getRandomPassword_requireEachIncludedType = Lens.lens (\GetRandomPassword' {requireEachIncludedType} -> requireEachIncludedType) (\s@GetRandomPassword' {} a -> s {requireEachIncludedType = a} :: GetRandomPassword)

instance Core.AWSRequest GetRandomPassword where
  type
    AWSResponse GetRandomPassword =
      GetRandomPasswordResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRandomPasswordResponse'
            Prelude.<$> (x Data..?> "RandomPassword")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetRandomPassword where
  hashWithSalt _salt GetRandomPassword' {..} =
    _salt `Prelude.hashWithSalt` excludeCharacters
      `Prelude.hashWithSalt` excludeLowercase
      `Prelude.hashWithSalt` excludeNumbers
      `Prelude.hashWithSalt` excludePunctuation
      `Prelude.hashWithSalt` excludeUppercase
      `Prelude.hashWithSalt` includeSpace
      `Prelude.hashWithSalt` passwordLength
      `Prelude.hashWithSalt` requireEachIncludedType

instance Prelude.NFData GetRandomPassword where
  rnf GetRandomPassword' {..} =
    Prelude.rnf excludeCharacters
      `Prelude.seq` Prelude.rnf excludeLowercase
      `Prelude.seq` Prelude.rnf excludeNumbers
      `Prelude.seq` Prelude.rnf excludePunctuation
      `Prelude.seq` Prelude.rnf excludeUppercase
      `Prelude.seq` Prelude.rnf includeSpace
      `Prelude.seq` Prelude.rnf passwordLength
      `Prelude.seq` Prelude.rnf requireEachIncludedType

instance Data.ToHeaders GetRandomPassword where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "secretsmanager.GetRandomPassword" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetRandomPassword where
  toJSON GetRandomPassword' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ExcludeCharacters" Data..=)
              Prelude.<$> excludeCharacters,
            ("ExcludeLowercase" Data..=)
              Prelude.<$> excludeLowercase,
            ("ExcludeNumbers" Data..=)
              Prelude.<$> excludeNumbers,
            ("ExcludePunctuation" Data..=)
              Prelude.<$> excludePunctuation,
            ("ExcludeUppercase" Data..=)
              Prelude.<$> excludeUppercase,
            ("IncludeSpace" Data..=) Prelude.<$> includeSpace,
            ("PasswordLength" Data..=)
              Prelude.<$> passwordLength,
            ("RequireEachIncludedType" Data..=)
              Prelude.<$> requireEachIncludedType
          ]
      )

instance Data.ToPath GetRandomPassword where
  toPath = Prelude.const "/"

instance Data.ToQuery GetRandomPassword where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetRandomPasswordResponse' smart constructor.
data GetRandomPasswordResponse = GetRandomPasswordResponse'
  { -- | A string with the password.
    randomPassword :: Prelude.Maybe (Data.Sensitive Prelude.Text),
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
-- 'randomPassword', 'getRandomPasswordResponse_randomPassword' - A string with the password.
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

-- | A string with the password.
getRandomPasswordResponse_randomPassword :: Lens.Lens' GetRandomPasswordResponse (Prelude.Maybe Prelude.Text)
getRandomPasswordResponse_randomPassword = Lens.lens (\GetRandomPasswordResponse' {randomPassword} -> randomPassword) (\s@GetRandomPasswordResponse' {} a -> s {randomPassword = a} :: GetRandomPasswordResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The response's http status code.
getRandomPasswordResponse_httpStatus :: Lens.Lens' GetRandomPasswordResponse Prelude.Int
getRandomPasswordResponse_httpStatus = Lens.lens (\GetRandomPasswordResponse' {httpStatus} -> httpStatus) (\s@GetRandomPasswordResponse' {} a -> s {httpStatus = a} :: GetRandomPasswordResponse)

instance Prelude.NFData GetRandomPasswordResponse where
  rnf GetRandomPasswordResponse' {..} =
    Prelude.rnf randomPassword
      `Prelude.seq` Prelude.rnf httpStatus
