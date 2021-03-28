{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UsernameConfigurationType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.CognitoIdentityProvider.Types.UsernameConfigurationType
  ( UsernameConfigurationType (..)
  -- * Smart constructor
  , mkUsernameConfigurationType
  -- * Lenses
  , uctCaseSensitive
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The username configuration type. 
--
-- /See:/ 'mkUsernameConfigurationType' smart constructor.
newtype UsernameConfigurationType = UsernameConfigurationType'
  { caseSensitive :: Core.Bool
    -- ^ Specifies whether username case sensitivity will be applied for all users in the user pool through Cognito APIs.
--
-- Valid values include:
--
--     * __@True@ __ : Enables case sensitivity for all username input. When this option is set to @True@ , users must sign in using the exact capitalization of their given username. For example, “UserName”. This is the default value.
--
--
--     * __@False@ __ : Enables case insensitivity for all username input. For example, when this option is set to @False@ , users will be able to sign in using either "username" or "Username". This option also enables both @preferred_username@ and @email@ alias to be case insensitive, in addition to the @username@ attribute.
--
--
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UsernameConfigurationType' value with any optional fields omitted.
mkUsernameConfigurationType
    :: Core.Bool -- ^ 'caseSensitive'
    -> UsernameConfigurationType
mkUsernameConfigurationType caseSensitive
  = UsernameConfigurationType'{caseSensitive}

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
uctCaseSensitive :: Lens.Lens' UsernameConfigurationType Core.Bool
uctCaseSensitive = Lens.field @"caseSensitive"
{-# INLINEABLE uctCaseSensitive #-}
{-# DEPRECATED caseSensitive "Use generic-lens or generic-optics with 'caseSensitive' instead"  #-}

instance Core.FromJSON UsernameConfigurationType where
        toJSON UsernameConfigurationType{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CaseSensitive" Core..= caseSensitive)])

instance Core.FromJSON UsernameConfigurationType where
        parseJSON
          = Core.withObject "UsernameConfigurationType" Core.$
              \ x ->
                UsernameConfigurationType' Core.<$> (x Core..: "CaseSensitive")
