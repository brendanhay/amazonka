{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UsernameConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UsernameConfigurationType
  ( UsernameConfigurationType (..),

    -- * Smart constructor
    mkUsernameConfigurationType,

    -- * Lenses
    uctCaseSensitive,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The username configuration type.
--
-- /See:/ 'mkUsernameConfigurationType' smart constructor.
newtype UsernameConfigurationType = UsernameConfigurationType'
  { caseSensitive ::
      Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UsernameConfigurationType' with the minimum fields required to make a request.
--
-- * 'caseSensitive' - Specifies whether username case sensitivity will be applied for all users in the user pool through Cognito APIs.
--
-- Valid values include:
--
--     * __@True@ __ : Enables case sensitivity for all username input. When this option is set to @True@ , users must sign in using the exact capitalization of their given username. For example, “UserName”. This is the default value.
--
--
--     * __@False@ __ : Enables case insensitivity for all username input. For example, when this option is set to @False@ , users will be able to sign in using either "username" or "Username". This option also enables both @preferred_username@ and @email@ alias to be case insensitive, in addition to the @username@ attribute.
mkUsernameConfigurationType ::
  -- | 'caseSensitive'
  Lude.Bool ->
  UsernameConfigurationType
mkUsernameConfigurationType pCaseSensitive_ =
  UsernameConfigurationType' {caseSensitive = pCaseSensitive_}

-- | Specifies whether username case sensitivity will be applied for all users in the user pool through Cognito APIs.
--
-- Valid values include:
--
--     * __@True@ __ : Enables case sensitivity for all username input. When this option is set to @True@ , users must sign in using the exact capitalization of their given username. For example, “UserName”. This is the default value.
--
--
--     * __@False@ __ : Enables case insensitivity for all username input. For example, when this option is set to @False@ , users will be able to sign in using either "username" or "Username". This option also enables both @preferred_username@ and @email@ alias to be case insensitive, in addition to the @username@ attribute.
--
--
--
-- /Note:/ Consider using 'caseSensitive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uctCaseSensitive :: Lens.Lens' UsernameConfigurationType Lude.Bool
uctCaseSensitive = Lens.lens (caseSensitive :: UsernameConfigurationType -> Lude.Bool) (\s a -> s {caseSensitive = a} :: UsernameConfigurationType)
{-# DEPRECATED uctCaseSensitive "Use generic-lens or generic-optics with 'caseSensitive' instead." #-}

instance Lude.FromJSON UsernameConfigurationType where
  parseJSON =
    Lude.withObject
      "UsernameConfigurationType"
      ( \x ->
          UsernameConfigurationType' Lude.<$> (x Lude..: "CaseSensitive")
      )

instance Lude.ToJSON UsernameConfigurationType where
  toJSON UsernameConfigurationType' {..} =
    Lude.object
      ( Lude.catMaybes
          [Lude.Just ("CaseSensitive" Lude..= caseSensitive)]
      )
