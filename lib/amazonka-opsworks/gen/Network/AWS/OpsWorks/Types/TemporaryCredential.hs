{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.TemporaryCredential
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.TemporaryCredential
  ( TemporaryCredential (..),

    -- * Smart constructor
    mkTemporaryCredential,

    -- * Lenses
    tcInstanceId,
    tcUsername,
    tcPassword,
    tcValidForInMinutes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains the data needed by RDP clients such as the Microsoft Remote Desktop Connection to log in to the instance.
--
-- /See:/ 'mkTemporaryCredential' smart constructor.
data TemporaryCredential = TemporaryCredential'
  { instanceId ::
      Lude.Maybe Lude.Text,
    username :: Lude.Maybe Lude.Text,
    password :: Lude.Maybe Lude.Text,
    validForInMinutes :: Lude.Maybe Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TemporaryCredential' with the minimum fields required to make a request.
--
-- * 'instanceId' - The instance's AWS OpsWorks Stacks ID.
-- * 'password' - The password.
-- * 'username' - The user name.
-- * 'validForInMinutes' - The length of time (in minutes) that the grant is valid. When the grant expires, at the end of this period, the user will no longer be able to use the credentials to log in. If they are logged in at the time, they will be automatically logged out.
mkTemporaryCredential ::
  TemporaryCredential
mkTemporaryCredential =
  TemporaryCredential'
    { instanceId = Lude.Nothing,
      username = Lude.Nothing,
      password = Lude.Nothing,
      validForInMinutes = Lude.Nothing
    }

-- | The instance's AWS OpsWorks Stacks ID.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcInstanceId :: Lens.Lens' TemporaryCredential (Lude.Maybe Lude.Text)
tcInstanceId = Lens.lens (instanceId :: TemporaryCredential -> Lude.Maybe Lude.Text) (\s a -> s {instanceId = a} :: TemporaryCredential)
{-# DEPRECATED tcInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The user name.
--
-- /Note:/ Consider using 'username' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcUsername :: Lens.Lens' TemporaryCredential (Lude.Maybe Lude.Text)
tcUsername = Lens.lens (username :: TemporaryCredential -> Lude.Maybe Lude.Text) (\s a -> s {username = a} :: TemporaryCredential)
{-# DEPRECATED tcUsername "Use generic-lens or generic-optics with 'username' instead." #-}

-- | The password.
--
-- /Note:/ Consider using 'password' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcPassword :: Lens.Lens' TemporaryCredential (Lude.Maybe Lude.Text)
tcPassword = Lens.lens (password :: TemporaryCredential -> Lude.Maybe Lude.Text) (\s a -> s {password = a} :: TemporaryCredential)
{-# DEPRECATED tcPassword "Use generic-lens or generic-optics with 'password' instead." #-}

-- | The length of time (in minutes) that the grant is valid. When the grant expires, at the end of this period, the user will no longer be able to use the credentials to log in. If they are logged in at the time, they will be automatically logged out.
--
-- /Note:/ Consider using 'validForInMinutes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tcValidForInMinutes :: Lens.Lens' TemporaryCredential (Lude.Maybe Lude.Int)
tcValidForInMinutes = Lens.lens (validForInMinutes :: TemporaryCredential -> Lude.Maybe Lude.Int) (\s a -> s {validForInMinutes = a} :: TemporaryCredential)
{-# DEPRECATED tcValidForInMinutes "Use generic-lens or generic-optics with 'validForInMinutes' instead." #-}

instance Lude.FromJSON TemporaryCredential where
  parseJSON =
    Lude.withObject
      "TemporaryCredential"
      ( \x ->
          TemporaryCredential'
            Lude.<$> (x Lude..:? "InstanceId")
            Lude.<*> (x Lude..:? "Username")
            Lude.<*> (x Lude..:? "Password")
            Lude.<*> (x Lude..:? "ValidForInMinutes")
      )
