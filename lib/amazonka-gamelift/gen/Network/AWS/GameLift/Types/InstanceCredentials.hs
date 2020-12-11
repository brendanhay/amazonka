-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.InstanceCredentials
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.InstanceCredentials
  ( InstanceCredentials (..),

    -- * Smart constructor
    mkInstanceCredentials,

    -- * Lenses
    icUserName,
    icSecret,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Set of credentials required to remotely access a fleet instance. Access credentials are requested by calling 'GetInstanceAccess' and returned in an 'InstanceAccess' object.
--
-- /See:/ 'mkInstanceCredentials' smart constructor.
data InstanceCredentials = InstanceCredentials'
  { userName ::
      Lude.Maybe Lude.Text,
    secret :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'InstanceCredentials' with the minimum fields required to make a request.
--
-- * 'secret' - Secret string. For Windows instances, the secret is a password for use with Windows Remote Desktop. For Linux instances, it is a private key (which must be saved as a @.pem@ file) for use with SSH.
-- * 'userName' - User login string.
mkInstanceCredentials ::
  InstanceCredentials
mkInstanceCredentials =
  InstanceCredentials'
    { userName = Lude.Nothing,
      secret = Lude.Nothing
    }

-- | User login string.
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icUserName :: Lens.Lens' InstanceCredentials (Lude.Maybe Lude.Text)
icUserName = Lens.lens (userName :: InstanceCredentials -> Lude.Maybe Lude.Text) (\s a -> s {userName = a} :: InstanceCredentials)
{-# DEPRECATED icUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | Secret string. For Windows instances, the secret is a password for use with Windows Remote Desktop. For Linux instances, it is a private key (which must be saved as a @.pem@ file) for use with SSH.
--
-- /Note:/ Consider using 'secret' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
icSecret :: Lens.Lens' InstanceCredentials (Lude.Maybe Lude.Text)
icSecret = Lens.lens (secret :: InstanceCredentials -> Lude.Maybe Lude.Text) (\s a -> s {secret = a} :: InstanceCredentials)
{-# DEPRECATED icSecret "Use generic-lens or generic-optics with 'secret' instead." #-}

instance Lude.FromJSON InstanceCredentials where
  parseJSON =
    Lude.withObject
      "InstanceCredentials"
      ( \x ->
          InstanceCredentials'
            Lude.<$> (x Lude..:? "UserName") Lude.<*> (x Lude..:? "Secret")
      )
