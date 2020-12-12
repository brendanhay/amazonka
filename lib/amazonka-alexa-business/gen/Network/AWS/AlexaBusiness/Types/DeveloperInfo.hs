{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeveloperInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.AlexaBusiness.Types.DeveloperInfo
  ( DeveloperInfo (..),

    -- * Smart constructor
    mkDeveloperInfo,

    -- * Lenses
    diEmail,
    diURL,
    diPrivacyPolicy,
    diDeveloperName,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The details about the developer that published the skill.
--
-- /See:/ 'mkDeveloperInfo' smart constructor.
data DeveloperInfo = DeveloperInfo'
  { email :: Lude.Maybe Lude.Text,
    url :: Lude.Maybe Lude.Text,
    privacyPolicy :: Lude.Maybe Lude.Text,
    developerName :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeveloperInfo' with the minimum fields required to make a request.
--
-- * 'developerName' - The name of the developer.
-- * 'email' - The email of the developer.
-- * 'privacyPolicy' - The URL of the privacy policy.
-- * 'url' - The website of the developer.
mkDeveloperInfo ::
  DeveloperInfo
mkDeveloperInfo =
  DeveloperInfo'
    { email = Lude.Nothing,
      url = Lude.Nothing,
      privacyPolicy = Lude.Nothing,
      developerName = Lude.Nothing
    }

-- | The email of the developer.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diEmail :: Lens.Lens' DeveloperInfo (Lude.Maybe Lude.Text)
diEmail = Lens.lens (email :: DeveloperInfo -> Lude.Maybe Lude.Text) (\s a -> s {email = a} :: DeveloperInfo)
{-# DEPRECATED diEmail "Use generic-lens or generic-optics with 'email' instead." #-}

-- | The website of the developer.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diURL :: Lens.Lens' DeveloperInfo (Lude.Maybe Lude.Text)
diURL = Lens.lens (url :: DeveloperInfo -> Lude.Maybe Lude.Text) (\s a -> s {url = a} :: DeveloperInfo)
{-# DEPRECATED diURL "Use generic-lens or generic-optics with 'url' instead." #-}

-- | The URL of the privacy policy.
--
-- /Note:/ Consider using 'privacyPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diPrivacyPolicy :: Lens.Lens' DeveloperInfo (Lude.Maybe Lude.Text)
diPrivacyPolicy = Lens.lens (privacyPolicy :: DeveloperInfo -> Lude.Maybe Lude.Text) (\s a -> s {privacyPolicy = a} :: DeveloperInfo)
{-# DEPRECATED diPrivacyPolicy "Use generic-lens or generic-optics with 'privacyPolicy' instead." #-}

-- | The name of the developer.
--
-- /Note:/ Consider using 'developerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDeveloperName :: Lens.Lens' DeveloperInfo (Lude.Maybe Lude.Text)
diDeveloperName = Lens.lens (developerName :: DeveloperInfo -> Lude.Maybe Lude.Text) (\s a -> s {developerName = a} :: DeveloperInfo)
{-# DEPRECATED diDeveloperName "Use generic-lens or generic-optics with 'developerName' instead." #-}

instance Lude.FromJSON DeveloperInfo where
  parseJSON =
    Lude.withObject
      "DeveloperInfo"
      ( \x ->
          DeveloperInfo'
            Lude.<$> (x Lude..:? "Email")
            Lude.<*> (x Lude..:? "Url")
            Lude.<*> (x Lude..:? "PrivacyPolicy")
            Lude.<*> (x Lude..:? "DeveloperName")
      )
