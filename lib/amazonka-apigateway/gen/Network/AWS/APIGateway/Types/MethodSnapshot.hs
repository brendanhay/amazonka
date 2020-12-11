-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.APIGateway.Types.MethodSnapshot
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.APIGateway.Types.MethodSnapshot
  ( MethodSnapshot (..),

    -- * Smart constructor
    mkMethodSnapshot,

    -- * Lenses
    msAuthorizationType,
    msApiKeyRequired,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a summary of a 'Method' resource, given a particular date and time.
--
-- /See:/ 'mkMethodSnapshot' smart constructor.
data MethodSnapshot = MethodSnapshot'
  { authorizationType ::
      Lude.Maybe Lude.Text,
    apiKeyRequired :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'MethodSnapshot' with the minimum fields required to make a request.
--
-- * 'apiKeyRequired' - Specifies whether the method requires a valid 'ApiKey' .
-- * 'authorizationType' - The method's authorization type. Valid values are @NONE@ for open access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user pool.
mkMethodSnapshot ::
  MethodSnapshot
mkMethodSnapshot =
  MethodSnapshot'
    { authorizationType = Lude.Nothing,
      apiKeyRequired = Lude.Nothing
    }

-- | The method's authorization type. Valid values are @NONE@ for open access, @AWS_IAM@ for using AWS IAM permissions, @CUSTOM@ for using a custom authorizer, or @COGNITO_USER_POOLS@ for using a Cognito user pool.
--
-- /Note:/ Consider using 'authorizationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msAuthorizationType :: Lens.Lens' MethodSnapshot (Lude.Maybe Lude.Text)
msAuthorizationType = Lens.lens (authorizationType :: MethodSnapshot -> Lude.Maybe Lude.Text) (\s a -> s {authorizationType = a} :: MethodSnapshot)
{-# DEPRECATED msAuthorizationType "Use generic-lens or generic-optics with 'authorizationType' instead." #-}

-- | Specifies whether the method requires a valid 'ApiKey' .
--
-- /Note:/ Consider using 'apiKeyRequired' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
msApiKeyRequired :: Lens.Lens' MethodSnapshot (Lude.Maybe Lude.Bool)
msApiKeyRequired = Lens.lens (apiKeyRequired :: MethodSnapshot -> Lude.Maybe Lude.Bool) (\s a -> s {apiKeyRequired = a} :: MethodSnapshot)
{-# DEPRECATED msApiKeyRequired "Use generic-lens or generic-optics with 'apiKeyRequired' instead." #-}

instance Lude.FromJSON MethodSnapshot where
  parseJSON =
    Lude.withObject
      "MethodSnapshot"
      ( \x ->
          MethodSnapshot'
            Lude.<$> (x Lude..:? "authorizationType")
            Lude.<*> (x Lude..:? "apiKeyRequired")
      )
