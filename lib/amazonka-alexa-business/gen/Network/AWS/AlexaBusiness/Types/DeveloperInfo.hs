{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.Types.DeveloperInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.AlexaBusiness.Types.DeveloperInfo
  ( DeveloperInfo (..)
  -- * Smart constructor
  , mkDeveloperInfo
  -- * Lenses
  , diDeveloperName
  , diEmail
  , diPrivacyPolicy
  , diUrl
  ) where

import qualified Network.AWS.AlexaBusiness.Types.DeveloperName as Types
import qualified Network.AWS.AlexaBusiness.Types.Email as Types
import qualified Network.AWS.AlexaBusiness.Types.PrivacyPolicy as Types
import qualified Network.AWS.AlexaBusiness.Types.Url as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The details about the developer that published the skill.
--
-- /See:/ 'mkDeveloperInfo' smart constructor.
data DeveloperInfo = DeveloperInfo'
  { developerName :: Core.Maybe Types.DeveloperName
    -- ^ The name of the developer.
  , email :: Core.Maybe Types.Email
    -- ^ The email of the developer.
  , privacyPolicy :: Core.Maybe Types.PrivacyPolicy
    -- ^ The URL of the privacy policy.
  , url :: Core.Maybe Types.Url
    -- ^ The website of the developer.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeveloperInfo' value with any optional fields omitted.
mkDeveloperInfo
    :: DeveloperInfo
mkDeveloperInfo
  = DeveloperInfo'{developerName = Core.Nothing,
                   email = Core.Nothing, privacyPolicy = Core.Nothing,
                   url = Core.Nothing}

-- | The name of the developer.
--
-- /Note:/ Consider using 'developerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diDeveloperName :: Lens.Lens' DeveloperInfo (Core.Maybe Types.DeveloperName)
diDeveloperName = Lens.field @"developerName"
{-# INLINEABLE diDeveloperName #-}
{-# DEPRECATED developerName "Use generic-lens or generic-optics with 'developerName' instead"  #-}

-- | The email of the developer.
--
-- /Note:/ Consider using 'email' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diEmail :: Lens.Lens' DeveloperInfo (Core.Maybe Types.Email)
diEmail = Lens.field @"email"
{-# INLINEABLE diEmail #-}
{-# DEPRECATED email "Use generic-lens or generic-optics with 'email' instead"  #-}

-- | The URL of the privacy policy.
--
-- /Note:/ Consider using 'privacyPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diPrivacyPolicy :: Lens.Lens' DeveloperInfo (Core.Maybe Types.PrivacyPolicy)
diPrivacyPolicy = Lens.field @"privacyPolicy"
{-# INLINEABLE diPrivacyPolicy #-}
{-# DEPRECATED privacyPolicy "Use generic-lens or generic-optics with 'privacyPolicy' instead"  #-}

-- | The website of the developer.
--
-- /Note:/ Consider using 'url' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diUrl :: Lens.Lens' DeveloperInfo (Core.Maybe Types.Url)
diUrl = Lens.field @"url"
{-# INLINEABLE diUrl #-}
{-# DEPRECATED url "Use generic-lens or generic-optics with 'url' instead"  #-}

instance Core.FromJSON DeveloperInfo where
        parseJSON
          = Core.withObject "DeveloperInfo" Core.$
              \ x ->
                DeveloperInfo' Core.<$>
                  (x Core..:? "DeveloperName") Core.<*> x Core..:? "Email" Core.<*>
                    x Core..:? "PrivacyPolicy"
                    Core.<*> x Core..:? "Url"
