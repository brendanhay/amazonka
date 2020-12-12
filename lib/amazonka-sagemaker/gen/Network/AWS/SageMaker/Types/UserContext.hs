{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SageMaker.Types.UserContext
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.UserContext
  ( UserContext (..),

    -- * Smart constructor
    mkUserContext,

    -- * Lenses
    ucUserProfileName,
    ucUserProfileARN,
    ucDomainId,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the user who created or modified an experiment, trial, or trial component.
--
-- /See:/ 'mkUserContext' smart constructor.
data UserContext = UserContext'
  { userProfileName ::
      Lude.Maybe Lude.Text,
    userProfileARN :: Lude.Maybe Lude.Text,
    domainId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserContext' with the minimum fields required to make a request.
--
-- * 'domainId' - The domain associated with the user.
-- * 'userProfileARN' - The Amazon Resource Name (ARN) of the user's profile.
-- * 'userProfileName' - The name of the user's profile.
mkUserContext ::
  UserContext
mkUserContext =
  UserContext'
    { userProfileName = Lude.Nothing,
      userProfileARN = Lude.Nothing,
      domainId = Lude.Nothing
    }

-- | The name of the user's profile.
--
-- /Note:/ Consider using 'userProfileName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucUserProfileName :: Lens.Lens' UserContext (Lude.Maybe Lude.Text)
ucUserProfileName = Lens.lens (userProfileName :: UserContext -> Lude.Maybe Lude.Text) (\s a -> s {userProfileName = a} :: UserContext)
{-# DEPRECATED ucUserProfileName "Use generic-lens or generic-optics with 'userProfileName' instead." #-}

-- | The Amazon Resource Name (ARN) of the user's profile.
--
-- /Note:/ Consider using 'userProfileARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucUserProfileARN :: Lens.Lens' UserContext (Lude.Maybe Lude.Text)
ucUserProfileARN = Lens.lens (userProfileARN :: UserContext -> Lude.Maybe Lude.Text) (\s a -> s {userProfileARN = a} :: UserContext)
{-# DEPRECATED ucUserProfileARN "Use generic-lens or generic-optics with 'userProfileARN' instead." #-}

-- | The domain associated with the user.
--
-- /Note:/ Consider using 'domainId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucDomainId :: Lens.Lens' UserContext (Lude.Maybe Lude.Text)
ucDomainId = Lens.lens (domainId :: UserContext -> Lude.Maybe Lude.Text) (\s a -> s {domainId = a} :: UserContext)
{-# DEPRECATED ucDomainId "Use generic-lens or generic-optics with 'domainId' instead." #-}

instance Lude.FromJSON UserContext where
  parseJSON =
    Lude.withObject
      "UserContext"
      ( \x ->
          UserContext'
            Lude.<$> (x Lude..:? "UserProfileName")
            Lude.<*> (x Lude..:? "UserProfileArn")
            Lude.<*> (x Lude..:? "DomainId")
      )
