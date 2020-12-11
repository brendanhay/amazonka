{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SecretsManager.GetRandomPassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Generates a random password of the specified complexity. This operation is intended for use in the Lambda rotation function. Per best practice, we recommend that you specify the maximum length and include every character type that the system you are generating a password for can support.
--
-- __Minimum permissions__
-- To run this command, you must have the following permissions:
--
--     * secretsmanager:GetRandomPassword
module Network.AWS.SecretsManager.GetRandomPassword
  ( -- * Creating a request
    GetRandomPassword (..),
    mkGetRandomPassword,

    -- ** Request lenses
    grpIncludeSpace,
    grpExcludeNumbers,
    grpExcludeLowercase,
    grpExcludeCharacters,
    grpExcludePunctuation,
    grpRequireEachIncludedType,
    grpExcludeUppercase,
    grpPasswordLength,

    -- * Destructuring the response
    GetRandomPasswordResponse (..),
    mkGetRandomPasswordResponse,

    -- ** Response lenses
    grsRandomPassword,
    grsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.SecretsManager.Types

-- | /See:/ 'mkGetRandomPassword' smart constructor.
data GetRandomPassword = GetRandomPassword'
  { includeSpace ::
      Lude.Maybe Lude.Bool,
    excludeNumbers :: Lude.Maybe Lude.Bool,
    excludeLowercase :: Lude.Maybe Lude.Bool,
    excludeCharacters :: Lude.Maybe Lude.Text,
    excludePunctuation :: Lude.Maybe Lude.Bool,
    requireEachIncludedType :: Lude.Maybe Lude.Bool,
    excludeUppercase :: Lude.Maybe Lude.Bool,
    passwordLength :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRandomPassword' with the minimum fields required to make a request.
--
-- * 'excludeCharacters' - A string that includes characters that should not be included in the generated password. The default is that all characters from the included sets can be used.
-- * 'excludeLowercase' - Specifies that the generated password should not include lowercase letters. The default if you do not include this switch parameter is that lowercase letters can be included.
-- * 'excludeNumbers' - Specifies that the generated password should not include digits. The default if you do not include this switch parameter is that digits can be included.
-- * 'excludePunctuation' - Specifies that the generated password should not include punctuation characters. The default if you do not include this switch parameter is that punctuation characters can be included.
--
-- The following are the punctuation characters that /can/ be included in the generated password if you don't explicitly exclude them with @ExcludeCharacters@ or @ExcludePunctuation@ :
-- @! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~@
-- * 'excludeUppercase' - Specifies that the generated password should not include uppercase letters. The default if you do not include this switch parameter is that uppercase letters can be included.
-- * 'includeSpace' - Specifies that the generated password can include the space character. The default if you do not include this switch parameter is that the space character is not included.
-- * 'passwordLength' - The desired length of the generated password. The default value if you do not include this parameter is 32 characters.
-- * 'requireEachIncludedType' - A boolean value that specifies whether the generated password must include at least one of every allowed character type. The default value is @True@ and the operation requires at least one of every character type.
mkGetRandomPassword ::
  GetRandomPassword
mkGetRandomPassword =
  GetRandomPassword'
    { includeSpace = Lude.Nothing,
      excludeNumbers = Lude.Nothing,
      excludeLowercase = Lude.Nothing,
      excludeCharacters = Lude.Nothing,
      excludePunctuation = Lude.Nothing,
      requireEachIncludedType = Lude.Nothing,
      excludeUppercase = Lude.Nothing,
      passwordLength = Lude.Nothing
    }

-- | Specifies that the generated password can include the space character. The default if you do not include this switch parameter is that the space character is not included.
--
-- /Note:/ Consider using 'includeSpace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpIncludeSpace :: Lens.Lens' GetRandomPassword (Lude.Maybe Lude.Bool)
grpIncludeSpace = Lens.lens (includeSpace :: GetRandomPassword -> Lude.Maybe Lude.Bool) (\s a -> s {includeSpace = a} :: GetRandomPassword)
{-# DEPRECATED grpIncludeSpace "Use generic-lens or generic-optics with 'includeSpace' instead." #-}

-- | Specifies that the generated password should not include digits. The default if you do not include this switch parameter is that digits can be included.
--
-- /Note:/ Consider using 'excludeNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpExcludeNumbers :: Lens.Lens' GetRandomPassword (Lude.Maybe Lude.Bool)
grpExcludeNumbers = Lens.lens (excludeNumbers :: GetRandomPassword -> Lude.Maybe Lude.Bool) (\s a -> s {excludeNumbers = a} :: GetRandomPassword)
{-# DEPRECATED grpExcludeNumbers "Use generic-lens or generic-optics with 'excludeNumbers' instead." #-}

-- | Specifies that the generated password should not include lowercase letters. The default if you do not include this switch parameter is that lowercase letters can be included.
--
-- /Note:/ Consider using 'excludeLowercase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpExcludeLowercase :: Lens.Lens' GetRandomPassword (Lude.Maybe Lude.Bool)
grpExcludeLowercase = Lens.lens (excludeLowercase :: GetRandomPassword -> Lude.Maybe Lude.Bool) (\s a -> s {excludeLowercase = a} :: GetRandomPassword)
{-# DEPRECATED grpExcludeLowercase "Use generic-lens or generic-optics with 'excludeLowercase' instead." #-}

-- | A string that includes characters that should not be included in the generated password. The default is that all characters from the included sets can be used.
--
-- /Note:/ Consider using 'excludeCharacters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpExcludeCharacters :: Lens.Lens' GetRandomPassword (Lude.Maybe Lude.Text)
grpExcludeCharacters = Lens.lens (excludeCharacters :: GetRandomPassword -> Lude.Maybe Lude.Text) (\s a -> s {excludeCharacters = a} :: GetRandomPassword)
{-# DEPRECATED grpExcludeCharacters "Use generic-lens or generic-optics with 'excludeCharacters' instead." #-}

-- | Specifies that the generated password should not include punctuation characters. The default if you do not include this switch parameter is that punctuation characters can be included.
--
-- The following are the punctuation characters that /can/ be included in the generated password if you don't explicitly exclude them with @ExcludeCharacters@ or @ExcludePunctuation@ :
-- @! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~@
--
-- /Note:/ Consider using 'excludePunctuation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpExcludePunctuation :: Lens.Lens' GetRandomPassword (Lude.Maybe Lude.Bool)
grpExcludePunctuation = Lens.lens (excludePunctuation :: GetRandomPassword -> Lude.Maybe Lude.Bool) (\s a -> s {excludePunctuation = a} :: GetRandomPassword)
{-# DEPRECATED grpExcludePunctuation "Use generic-lens or generic-optics with 'excludePunctuation' instead." #-}

-- | A boolean value that specifies whether the generated password must include at least one of every allowed character type. The default value is @True@ and the operation requires at least one of every character type.
--
-- /Note:/ Consider using 'requireEachIncludedType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpRequireEachIncludedType :: Lens.Lens' GetRandomPassword (Lude.Maybe Lude.Bool)
grpRequireEachIncludedType = Lens.lens (requireEachIncludedType :: GetRandomPassword -> Lude.Maybe Lude.Bool) (\s a -> s {requireEachIncludedType = a} :: GetRandomPassword)
{-# DEPRECATED grpRequireEachIncludedType "Use generic-lens or generic-optics with 'requireEachIncludedType' instead." #-}

-- | Specifies that the generated password should not include uppercase letters. The default if you do not include this switch parameter is that uppercase letters can be included.
--
-- /Note:/ Consider using 'excludeUppercase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpExcludeUppercase :: Lens.Lens' GetRandomPassword (Lude.Maybe Lude.Bool)
grpExcludeUppercase = Lens.lens (excludeUppercase :: GetRandomPassword -> Lude.Maybe Lude.Bool) (\s a -> s {excludeUppercase = a} :: GetRandomPassword)
{-# DEPRECATED grpExcludeUppercase "Use generic-lens or generic-optics with 'excludeUppercase' instead." #-}

-- | The desired length of the generated password. The default value if you do not include this parameter is 32 characters.
--
-- /Note:/ Consider using 'passwordLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpPasswordLength :: Lens.Lens' GetRandomPassword (Lude.Maybe Lude.Natural)
grpPasswordLength = Lens.lens (passwordLength :: GetRandomPassword -> Lude.Maybe Lude.Natural) (\s a -> s {passwordLength = a} :: GetRandomPassword)
{-# DEPRECATED grpPasswordLength "Use generic-lens or generic-optics with 'passwordLength' instead." #-}

instance Lude.AWSRequest GetRandomPassword where
  type Rs GetRandomPassword = GetRandomPasswordResponse
  request = Req.postJSON secretsManagerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetRandomPasswordResponse'
            Lude.<$> (x Lude..?> "RandomPassword")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetRandomPassword where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("secretsmanager.GetRandomPassword" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetRandomPassword where
  toJSON GetRandomPassword' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("IncludeSpace" Lude..=) Lude.<$> includeSpace,
            ("ExcludeNumbers" Lude..=) Lude.<$> excludeNumbers,
            ("ExcludeLowercase" Lude..=) Lude.<$> excludeLowercase,
            ("ExcludeCharacters" Lude..=) Lude.<$> excludeCharacters,
            ("ExcludePunctuation" Lude..=) Lude.<$> excludePunctuation,
            ("RequireEachIncludedType" Lude..=)
              Lude.<$> requireEachIncludedType,
            ("ExcludeUppercase" Lude..=) Lude.<$> excludeUppercase,
            ("PasswordLength" Lude..=) Lude.<$> passwordLength
          ]
      )

instance Lude.ToPath GetRandomPassword where
  toPath = Lude.const "/"

instance Lude.ToQuery GetRandomPassword where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetRandomPasswordResponse' smart constructor.
data GetRandomPasswordResponse = GetRandomPasswordResponse'
  { randomPassword ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetRandomPasswordResponse' with the minimum fields required to make a request.
--
-- * 'randomPassword' - A string with the generated password.
-- * 'responseStatus' - The response status code.
mkGetRandomPasswordResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetRandomPasswordResponse
mkGetRandomPasswordResponse pResponseStatus_ =
  GetRandomPasswordResponse'
    { randomPassword = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A string with the generated password.
--
-- /Note:/ Consider using 'randomPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsRandomPassword :: Lens.Lens' GetRandomPasswordResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
grsRandomPassword = Lens.lens (randomPassword :: GetRandomPasswordResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {randomPassword = a} :: GetRandomPasswordResponse)
{-# DEPRECATED grsRandomPassword "Use generic-lens or generic-optics with 'randomPassword' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetRandomPasswordResponse Lude.Int
grsResponseStatus = Lens.lens (responseStatus :: GetRandomPasswordResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetRandomPasswordResponse)
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
