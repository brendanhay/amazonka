{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.Types.ApplicationSummary
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ServerlessApplicationRepository.Types.ApplicationSummary
  ( ApplicationSummary (..)
  -- * Smart constructor
  , mkApplicationSummary
  -- * Lenses
  , asDescription
  , asAuthor
  , asApplicationId
  , asName
  , asCreationTime
  , asHomePageUrl
  , asLabels
  , asSpdxLicenseId
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Summary of details about the application.
--
-- /See:/ 'mkApplicationSummary' smart constructor.
data ApplicationSummary = ApplicationSummary'
  { description :: Core.Text
    -- ^ The description of the application.
--
-- Minimum length=1. Maximum length=256
  , author :: Core.Text
    -- ^ The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
  , applicationId :: Core.Text
    -- ^ The application Amazon Resource Name (ARN).
  , name :: Core.Text
    -- ^ The name of the application.
--
-- Minimum length=1. Maximum length=140
-- Pattern: "[a-zA-Z0-9\\-]+";
  , creationTime :: Core.Maybe Core.Text
    -- ^ The date and time this resource was created.
  , homePageUrl :: Core.Maybe Core.Text
    -- ^ A URL with more information about the application, for example the location of your GitHub repository for the application.
  , labels :: Core.Maybe [Core.Text]
    -- ^ Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
  , spdxLicenseId :: Core.Maybe Core.Text
    -- ^ A valid identifier from <https://spdx.org/licenses/ https://spdx.org/licenses/> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ApplicationSummary' value with any optional fields omitted.
mkApplicationSummary
    :: Core.Text -- ^ 'description'
    -> Core.Text -- ^ 'author'
    -> Core.Text -- ^ 'applicationId'
    -> Core.Text -- ^ 'name'
    -> ApplicationSummary
mkApplicationSummary description author applicationId name
  = ApplicationSummary'{description, author, applicationId, name,
                        creationTime = Core.Nothing, homePageUrl = Core.Nothing,
                        labels = Core.Nothing, spdxLicenseId = Core.Nothing}

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asDescription :: Lens.Lens' ApplicationSummary Core.Text
asDescription = Lens.field @"description"
{-# INLINEABLE asDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- /Note:/ Consider using 'author' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asAuthor :: Lens.Lens' ApplicationSummary Core.Text
asAuthor = Lens.field @"author"
{-# INLINEABLE asAuthor #-}
{-# DEPRECATED author "Use generic-lens or generic-optics with 'author' instead"  #-}

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asApplicationId :: Lens.Lens' ApplicationSummary Core.Text
asApplicationId = Lens.field @"applicationId"
{-# INLINEABLE asApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The name of the application.
--
-- Minimum length=1. Maximum length=140
-- Pattern: "[a-zA-Z0-9\\-]+";
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asName :: Lens.Lens' ApplicationSummary Core.Text
asName = Lens.field @"name"
{-# INLINEABLE asName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The date and time this resource was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asCreationTime :: Lens.Lens' ApplicationSummary (Core.Maybe Core.Text)
asCreationTime = Lens.field @"creationTime"
{-# INLINEABLE asCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | A URL with more information about the application, for example the location of your GitHub repository for the application.
--
-- /Note:/ Consider using 'homePageUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asHomePageUrl :: Lens.Lens' ApplicationSummary (Core.Maybe Core.Text)
asHomePageUrl = Lens.field @"homePageUrl"
{-# INLINEABLE asHomePageUrl #-}
{-# DEPRECATED homePageUrl "Use generic-lens or generic-optics with 'homePageUrl' instead"  #-}

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asLabels :: Lens.Lens' ApplicationSummary (Core.Maybe [Core.Text])
asLabels = Lens.field @"labels"
{-# INLINEABLE asLabels #-}
{-# DEPRECATED labels "Use generic-lens or generic-optics with 'labels' instead"  #-}

-- | A valid identifier from <https://spdx.org/licenses/ https://spdx.org/licenses/> .
--
-- /Note:/ Consider using 'spdxLicenseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
asSpdxLicenseId :: Lens.Lens' ApplicationSummary (Core.Maybe Core.Text)
asSpdxLicenseId = Lens.field @"spdxLicenseId"
{-# INLINEABLE asSpdxLicenseId #-}
{-# DEPRECATED spdxLicenseId "Use generic-lens or generic-optics with 'spdxLicenseId' instead"  #-}

instance Core.FromJSON ApplicationSummary where
        parseJSON
          = Core.withObject "ApplicationSummary" Core.$
              \ x ->
                ApplicationSummary' Core.<$>
                  (x Core..: "description") Core.<*> x Core..: "author" Core.<*>
                    x Core..: "applicationId"
                    Core.<*> x Core..: "name"
                    Core.<*> x Core..:? "creationTime"
                    Core.<*> x Core..:? "homePageUrl"
                    Core.<*> x Core..:? "labels"
                    Core.<*> x Core..:? "spdxLicenseId"
