{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    grpExcludeCharacters,
    grpExcludeLowercase,
    grpExcludeNumbers,
    grpExcludePunctuation,
    grpExcludeUppercase,
    grpIncludeSpace,
    grpPasswordLength,
    grpRequireEachIncludedType,

    -- * Destructuring the response
    GetRandomPasswordResponse (..),
    mkGetRandomPasswordResponse,

    -- ** Response lenses
    grsRandomPassword,
    grsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SecretsManager.Types as Types

-- | /See:/ 'mkGetRandomPassword' smart constructor.
data GetRandomPassword = GetRandomPassword'
  { -- | A string that includes characters that should not be included in the generated password. The default is that all characters from the included sets can be used.
    excludeCharacters :: Core.Maybe Types.ExcludeCharactersType,
    -- | Specifies that the generated password should not include lowercase letters. The default if you do not include this switch parameter is that lowercase letters can be included.
    excludeLowercase :: Core.Maybe Core.Bool,
    -- | Specifies that the generated password should not include digits. The default if you do not include this switch parameter is that digits can be included.
    excludeNumbers :: Core.Maybe Core.Bool,
    -- | Specifies that the generated password should not include punctuation characters. The default if you do not include this switch parameter is that punctuation characters can be included.
    --
    -- The following are the punctuation characters that /can/ be included in the generated password if you don't explicitly exclude them with @ExcludeCharacters@ or @ExcludePunctuation@ :
    -- @! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~@
    excludePunctuation :: Core.Maybe Core.Bool,
    -- | Specifies that the generated password should not include uppercase letters. The default if you do not include this switch parameter is that uppercase letters can be included.
    excludeUppercase :: Core.Maybe Core.Bool,
    -- | Specifies that the generated password can include the space character. The default if you do not include this switch parameter is that the space character is not included.
    includeSpace :: Core.Maybe Core.Bool,
    -- | The desired length of the generated password. The default value if you do not include this parameter is 32 characters.
    passwordLength :: Core.Maybe Core.Natural,
    -- | A boolean value that specifies whether the generated password must include at least one of every allowed character type. The default value is @True@ and the operation requires at least one of every character type.
    requireEachIncludedType :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRandomPassword' value with any optional fields omitted.
mkGetRandomPassword ::
  GetRandomPassword
mkGetRandomPassword =
  GetRandomPassword'
    { excludeCharacters = Core.Nothing,
      excludeLowercase = Core.Nothing,
      excludeNumbers = Core.Nothing,
      excludePunctuation = Core.Nothing,
      excludeUppercase = Core.Nothing,
      includeSpace = Core.Nothing,
      passwordLength = Core.Nothing,
      requireEachIncludedType = Core.Nothing
    }

-- | A string that includes characters that should not be included in the generated password. The default is that all characters from the included sets can be used.
--
-- /Note:/ Consider using 'excludeCharacters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpExcludeCharacters :: Lens.Lens' GetRandomPassword (Core.Maybe Types.ExcludeCharactersType)
grpExcludeCharacters = Lens.field @"excludeCharacters"
{-# DEPRECATED grpExcludeCharacters "Use generic-lens or generic-optics with 'excludeCharacters' instead." #-}

-- | Specifies that the generated password should not include lowercase letters. The default if you do not include this switch parameter is that lowercase letters can be included.
--
-- /Note:/ Consider using 'excludeLowercase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpExcludeLowercase :: Lens.Lens' GetRandomPassword (Core.Maybe Core.Bool)
grpExcludeLowercase = Lens.field @"excludeLowercase"
{-# DEPRECATED grpExcludeLowercase "Use generic-lens or generic-optics with 'excludeLowercase' instead." #-}

-- | Specifies that the generated password should not include digits. The default if you do not include this switch parameter is that digits can be included.
--
-- /Note:/ Consider using 'excludeNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpExcludeNumbers :: Lens.Lens' GetRandomPassword (Core.Maybe Core.Bool)
grpExcludeNumbers = Lens.field @"excludeNumbers"
{-# DEPRECATED grpExcludeNumbers "Use generic-lens or generic-optics with 'excludeNumbers' instead." #-}

-- | Specifies that the generated password should not include punctuation characters. The default if you do not include this switch parameter is that punctuation characters can be included.
--
-- The following are the punctuation characters that /can/ be included in the generated password if you don't explicitly exclude them with @ExcludeCharacters@ or @ExcludePunctuation@ :
-- @! " # $ % & ' ( ) * + , - . / : ; < = > ? @ [ \ ] ^ _ ` { | } ~@
--
-- /Note:/ Consider using 'excludePunctuation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpExcludePunctuation :: Lens.Lens' GetRandomPassword (Core.Maybe Core.Bool)
grpExcludePunctuation = Lens.field @"excludePunctuation"
{-# DEPRECATED grpExcludePunctuation "Use generic-lens or generic-optics with 'excludePunctuation' instead." #-}

-- | Specifies that the generated password should not include uppercase letters. The default if you do not include this switch parameter is that uppercase letters can be included.
--
-- /Note:/ Consider using 'excludeUppercase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpExcludeUppercase :: Lens.Lens' GetRandomPassword (Core.Maybe Core.Bool)
grpExcludeUppercase = Lens.field @"excludeUppercase"
{-# DEPRECATED grpExcludeUppercase "Use generic-lens or generic-optics with 'excludeUppercase' instead." #-}

-- | Specifies that the generated password can include the space character. The default if you do not include this switch parameter is that the space character is not included.
--
-- /Note:/ Consider using 'includeSpace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpIncludeSpace :: Lens.Lens' GetRandomPassword (Core.Maybe Core.Bool)
grpIncludeSpace = Lens.field @"includeSpace"
{-# DEPRECATED grpIncludeSpace "Use generic-lens or generic-optics with 'includeSpace' instead." #-}

-- | The desired length of the generated password. The default value if you do not include this parameter is 32 characters.
--
-- /Note:/ Consider using 'passwordLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpPasswordLength :: Lens.Lens' GetRandomPassword (Core.Maybe Core.Natural)
grpPasswordLength = Lens.field @"passwordLength"
{-# DEPRECATED grpPasswordLength "Use generic-lens or generic-optics with 'passwordLength' instead." #-}

-- | A boolean value that specifies whether the generated password must include at least one of every allowed character type. The default value is @True@ and the operation requires at least one of every character type.
--
-- /Note:/ Consider using 'requireEachIncludedType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grpRequireEachIncludedType :: Lens.Lens' GetRandomPassword (Core.Maybe Core.Bool)
grpRequireEachIncludedType = Lens.field @"requireEachIncludedType"
{-# DEPRECATED grpRequireEachIncludedType "Use generic-lens or generic-optics with 'requireEachIncludedType' instead." #-}

instance Core.FromJSON GetRandomPassword where
  toJSON GetRandomPassword {..} =
    Core.object
      ( Core.catMaybes
          [ ("ExcludeCharacters" Core..=) Core.<$> excludeCharacters,
            ("ExcludeLowercase" Core..=) Core.<$> excludeLowercase,
            ("ExcludeNumbers" Core..=) Core.<$> excludeNumbers,
            ("ExcludePunctuation" Core..=) Core.<$> excludePunctuation,
            ("ExcludeUppercase" Core..=) Core.<$> excludeUppercase,
            ("IncludeSpace" Core..=) Core.<$> includeSpace,
            ("PasswordLength" Core..=) Core.<$> passwordLength,
            ("RequireEachIncludedType" Core..=)
              Core.<$> requireEachIncludedType
          ]
      )

instance Core.AWSRequest GetRandomPassword where
  type Rs GetRandomPassword = GetRandomPasswordResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "secretsmanager.GetRandomPassword")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetRandomPasswordResponse'
            Core.<$> (x Core..:? "RandomPassword")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetRandomPasswordResponse' smart constructor.
data GetRandomPasswordResponse = GetRandomPasswordResponse'
  { -- | A string with the generated password.
    randomPassword :: Core.Maybe Types.RandomPasswordType,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRandomPasswordResponse' value with any optional fields omitted.
mkGetRandomPasswordResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetRandomPasswordResponse
mkGetRandomPasswordResponse responseStatus =
  GetRandomPasswordResponse'
    { randomPassword = Core.Nothing,
      responseStatus
    }

-- | A string with the generated password.
--
-- /Note:/ Consider using 'randomPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsRandomPassword :: Lens.Lens' GetRandomPasswordResponse (Core.Maybe Types.RandomPasswordType)
grsRandomPassword = Lens.field @"randomPassword"
{-# DEPRECATED grsRandomPassword "Use generic-lens or generic-optics with 'randomPassword' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grsResponseStatus :: Lens.Lens' GetRandomPasswordResponse Core.Int
grsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED grsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
