-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Authentication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Authentication
  ( Authentication (..),

    -- * Smart constructor
    mkAuthentication,

    -- * Lenses
    aPasswordCount,
    aType,
  )
where

import Network.AWS.ElastiCache.Types.AuthenticationType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Indicates whether the user requires a password to authenticate.
--
-- /See:/ 'mkAuthentication' smart constructor.
data Authentication = Authentication'
  { passwordCount ::
      Lude.Maybe Lude.Int,
    type' :: Lude.Maybe AuthenticationType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Authentication' with the minimum fields required to make a request.
--
-- * 'passwordCount' - The number of passwords belonging to the user. The maximum is two.
-- * 'type'' - Indicates whether the user requires a password to authenticate.
mkAuthentication ::
  Authentication
mkAuthentication =
  Authentication'
    { passwordCount = Lude.Nothing,
      type' = Lude.Nothing
    }

-- | The number of passwords belonging to the user. The maximum is two.
--
-- /Note:/ Consider using 'passwordCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPasswordCount :: Lens.Lens' Authentication (Lude.Maybe Lude.Int)
aPasswordCount = Lens.lens (passwordCount :: Authentication -> Lude.Maybe Lude.Int) (\s a -> s {passwordCount = a} :: Authentication)
{-# DEPRECATED aPasswordCount "Use generic-lens or generic-optics with 'passwordCount' instead." #-}

-- | Indicates whether the user requires a password to authenticate.
--
-- /Note:/ Consider using 'type'' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aType :: Lens.Lens' Authentication (Lude.Maybe AuthenticationType)
aType = Lens.lens (type' :: Authentication -> Lude.Maybe AuthenticationType) (\s a -> s {type' = a} :: Authentication)
{-# DEPRECATED aType "Use generic-lens or generic-optics with 'type'' instead." #-}

instance Lude.FromXML Authentication where
  parseXML x =
    Authentication'
      Lude.<$> (x Lude..@? "PasswordCount") Lude.<*> (x Lude..@? "Type")
