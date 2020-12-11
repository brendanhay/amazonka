-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.AuthorizerSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.AuthorizerSummary
  ( AuthorizerSummary (..),

    -- * Smart constructor
    mkAuthorizerSummary,

    -- * Lenses
    asAuthorizerName,
    asAuthorizerARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The authorizer summary.
--
-- /See:/ 'mkAuthorizerSummary' smart constructor.
data AuthorizerSummary = AuthorizerSummary'
  { authorizerName ::
      Lude.Maybe Lude.Text,
    authorizerARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AuthorizerSummary' with the minimum fields required to make a request.
--
-- * 'authorizerARN' - The authorizer ARN.
-- * 'authorizerName' - The authorizer name.
mkAuthorizerSummary ::
  AuthorizerSummary
mkAuthorizerSummary =
  AuthorizerSummary'
    { authorizerName = Lude.Nothing,
      authorizerARN = Lude.Nothing
    }

-- | The authorizer name.
--
-- /Note:/ Consider using 'authorizerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAuthorizerName :: Lens.Lens' AuthorizerSummary (Lude.Maybe Lude.Text)
asAuthorizerName = Lens.lens (authorizerName :: AuthorizerSummary -> Lude.Maybe Lude.Text) (\s a -> s {authorizerName = a} :: AuthorizerSummary)
{-# DEPRECATED asAuthorizerName "Use generic-lens or generic-optics with 'authorizerName' instead." #-}

-- | The authorizer ARN.
--
-- /Note:/ Consider using 'authorizerARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAuthorizerARN :: Lens.Lens' AuthorizerSummary (Lude.Maybe Lude.Text)
asAuthorizerARN = Lens.lens (authorizerARN :: AuthorizerSummary -> Lude.Maybe Lude.Text) (\s a -> s {authorizerARN = a} :: AuthorizerSummary)
{-# DEPRECATED asAuthorizerARN "Use generic-lens or generic-optics with 'authorizerARN' instead." #-}

instance Lude.FromJSON AuthorizerSummary where
  parseJSON =
    Lude.withObject
      "AuthorizerSummary"
      ( \x ->
          AuthorizerSummary'
            Lude.<$> (x Lude..:? "authorizerName")
            Lude.<*> (x Lude..:? "authorizerArn")
      )
