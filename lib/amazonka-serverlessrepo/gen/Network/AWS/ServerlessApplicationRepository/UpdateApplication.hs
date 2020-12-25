{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.UpdateApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified application.
module Network.AWS.ServerlessApplicationRepository.UpdateApplication
  ( -- * Creating a request
    UpdateApplication (..),
    mkUpdateApplication,

    -- ** Request lenses
    uApplicationId,
    uAuthor,
    uDescription,
    uHomePageUrl,
    uLabels,
    uReadmeBody,
    uReadmeUrl,

    -- * Destructuring the response
    UpdateApplicationResponse (..),
    mkUpdateApplicationResponse,

    -- ** Response lenses
    uarrsApplicationId,
    uarrsAuthor,
    uarrsCreationTime,
    uarrsDescription,
    uarrsHomePageUrl,
    uarrsIsVerifiedAuthor,
    uarrsLabels,
    uarrsLicenseUrl,
    uarrsName,
    uarrsReadmeUrl,
    uarrsSpdxLicenseId,
    uarrsVerifiedAuthorUrl,
    uarrsVersion,
    uarrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServerlessApplicationRepository.Types as Types

-- | /See:/ 'mkUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Core.Text,
    -- | The name of the author publishing the app.
    --
    -- Minimum length=1. Maximum length=127.
    -- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
    author :: Core.Maybe Core.Text,
    -- | The description of the application.
    --
    -- Minimum length=1. Maximum length=256
    description :: Core.Maybe Core.Text,
    -- | A URL with more information about the application, for example the location of your GitHub repository for the application.
    homePageUrl :: Core.Maybe Core.Text,
    -- | Labels to improve discovery of apps in search results.
    --
    -- Minimum length=1. Maximum length=127. Maximum number of labels: 10
    -- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
    labels :: Core.Maybe [Core.Text],
    -- | A text readme file in Markdown language that contains a more detailed description of the application and how it works.
    --
    -- Maximum size 5 MB
    readmeBody :: Core.Maybe Core.Text,
    -- | A link to the readme file in Markdown language that contains a more detailed description of the application and how it works.
    --
    -- Maximum size 5 MB
    readmeUrl :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplication' value with any optional fields omitted.
mkUpdateApplication ::
  -- | 'applicationId'
  Core.Text ->
  UpdateApplication
mkUpdateApplication applicationId =
  UpdateApplication'
    { applicationId,
      author = Core.Nothing,
      description = Core.Nothing,
      homePageUrl = Core.Nothing,
      labels = Core.Nothing,
      readmeBody = Core.Nothing,
      readmeUrl = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uApplicationId :: Lens.Lens' UpdateApplication Core.Text
uApplicationId = Lens.field @"applicationId"
{-# DEPRECATED uApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- /Note:/ Consider using 'author' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uAuthor :: Lens.Lens' UpdateApplication (Core.Maybe Core.Text)
uAuthor = Lens.field @"author"
{-# DEPRECATED uAuthor "Use generic-lens or generic-optics with 'author' instead." #-}

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uDescription :: Lens.Lens' UpdateApplication (Core.Maybe Core.Text)
uDescription = Lens.field @"description"
{-# DEPRECATED uDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A URL with more information about the application, for example the location of your GitHub repository for the application.
--
-- /Note:/ Consider using 'homePageUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uHomePageUrl :: Lens.Lens' UpdateApplication (Core.Maybe Core.Text)
uHomePageUrl = Lens.field @"homePageUrl"
{-# DEPRECATED uHomePageUrl "Use generic-lens or generic-optics with 'homePageUrl' instead." #-}

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uLabels :: Lens.Lens' UpdateApplication (Core.Maybe [Core.Text])
uLabels = Lens.field @"labels"
{-# DEPRECATED uLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | A text readme file in Markdown language that contains a more detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- /Note:/ Consider using 'readmeBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uReadmeBody :: Lens.Lens' UpdateApplication (Core.Maybe Core.Text)
uReadmeBody = Lens.field @"readmeBody"
{-# DEPRECATED uReadmeBody "Use generic-lens or generic-optics with 'readmeBody' instead." #-}

-- | A link to the readme file in Markdown language that contains a more detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- /Note:/ Consider using 'readmeUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uReadmeUrl :: Lens.Lens' UpdateApplication (Core.Maybe Core.Text)
uReadmeUrl = Lens.field @"readmeUrl"
{-# DEPRECATED uReadmeUrl "Use generic-lens or generic-optics with 'readmeUrl' instead." #-}

instance Core.FromJSON UpdateApplication where
  toJSON UpdateApplication {..} =
    Core.object
      ( Core.catMaybes
          [ ("author" Core..=) Core.<$> author,
            ("description" Core..=) Core.<$> description,
            ("homePageUrl" Core..=) Core.<$> homePageUrl,
            ("labels" Core..=) Core.<$> labels,
            ("readmeBody" Core..=) Core.<$> readmeBody,
            ("readmeUrl" Core..=) Core.<$> readmeUrl
          ]
      )

instance Core.AWSRequest UpdateApplication where
  type Rs UpdateApplication = UpdateApplicationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.PATCH,
        Core._rqPath =
          Core.rawPath
            ("/applications/" Core.<> (Core.toText applicationId)),
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApplicationResponse'
            Core.<$> (x Core..:? "applicationId")
            Core.<*> (x Core..:? "author")
            Core.<*> (x Core..:? "creationTime")
            Core.<*> (x Core..:? "description")
            Core.<*> (x Core..:? "homePageUrl")
            Core.<*> (x Core..:? "isVerifiedAuthor")
            Core.<*> (x Core..:? "labels")
            Core.<*> (x Core..:? "licenseUrl")
            Core.<*> (x Core..:? "name")
            Core.<*> (x Core..:? "readmeUrl")
            Core.<*> (x Core..:? "spdxLicenseId")
            Core.<*> (x Core..:? "verifiedAuthorUrl")
            Core.<*> (x Core..:? "version")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkUpdateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse'
  { -- | The application Amazon Resource Name (ARN).
    applicationId :: Core.Maybe Core.Text,
    -- | The name of the author publishing the app.
    --
    -- Minimum length=1. Maximum length=127.
    -- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
    author :: Core.Maybe Core.Text,
    -- | The date and time this resource was created.
    creationTime :: Core.Maybe Core.Text,
    -- | The description of the application.
    --
    -- Minimum length=1. Maximum length=256
    description :: Core.Maybe Core.Text,
    -- | A URL with more information about the application, for example the location of your GitHub repository for the application.
    homePageUrl :: Core.Maybe Core.Text,
    -- | Whether the author of this application has been verified. This means means that AWS has made a good faith review, as a reasonable and prudent service provider, of the information provided by the requester and has confirmed that the requester's identity is as claimed.
    isVerifiedAuthor :: Core.Maybe Core.Bool,
    -- | Labels to improve discovery of apps in search results.
    --
    -- Minimum length=1. Maximum length=127. Maximum number of labels: 10
    -- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
    labels :: Core.Maybe [Core.Text],
    -- | A link to a license file of the app that matches the spdxLicenseID value of your application.
    --
    -- Maximum size 5 MB
    licenseUrl :: Core.Maybe Core.Text,
    -- | The name of the application.
    --
    -- Minimum length=1. Maximum length=140
    -- Pattern: "[a-zA-Z0-9\\-]+";
    name :: Core.Maybe Core.Text,
    -- | A link to the readme file in Markdown language that contains a more detailed description of the application and how it works.
    --
    -- Maximum size 5 MB
    readmeUrl :: Core.Maybe Core.Text,
    -- | A valid identifier from https://spdx.org/licenses/.
    spdxLicenseId :: Core.Maybe Core.Text,
    -- | The URL to the public profile of a verified author. This URL is submitted by the author.
    verifiedAuthorUrl :: Core.Maybe Core.Text,
    -- | Version information about the application.
    version :: Core.Maybe Types.Version,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateApplicationResponse' value with any optional fields omitted.
mkUpdateApplicationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateApplicationResponse
mkUpdateApplicationResponse responseStatus =
  UpdateApplicationResponse'
    { applicationId = Core.Nothing,
      author = Core.Nothing,
      creationTime = Core.Nothing,
      description = Core.Nothing,
      homePageUrl = Core.Nothing,
      isVerifiedAuthor = Core.Nothing,
      labels = Core.Nothing,
      licenseUrl = Core.Nothing,
      name = Core.Nothing,
      readmeUrl = Core.Nothing,
      spdxLicenseId = Core.Nothing,
      verifiedAuthorUrl = Core.Nothing,
      version = Core.Nothing,
      responseStatus
    }

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsApplicationId :: Lens.Lens' UpdateApplicationResponse (Core.Maybe Core.Text)
uarrsApplicationId = Lens.field @"applicationId"
{-# DEPRECATED uarrsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- /Note:/ Consider using 'author' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsAuthor :: Lens.Lens' UpdateApplicationResponse (Core.Maybe Core.Text)
uarrsAuthor = Lens.field @"author"
{-# DEPRECATED uarrsAuthor "Use generic-lens or generic-optics with 'author' instead." #-}

-- | The date and time this resource was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsCreationTime :: Lens.Lens' UpdateApplicationResponse (Core.Maybe Core.Text)
uarrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED uarrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsDescription :: Lens.Lens' UpdateApplicationResponse (Core.Maybe Core.Text)
uarrsDescription = Lens.field @"description"
{-# DEPRECATED uarrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A URL with more information about the application, for example the location of your GitHub repository for the application.
--
-- /Note:/ Consider using 'homePageUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsHomePageUrl :: Lens.Lens' UpdateApplicationResponse (Core.Maybe Core.Text)
uarrsHomePageUrl = Lens.field @"homePageUrl"
{-# DEPRECATED uarrsHomePageUrl "Use generic-lens or generic-optics with 'homePageUrl' instead." #-}

-- | Whether the author of this application has been verified. This means means that AWS has made a good faith review, as a reasonable and prudent service provider, of the information provided by the requester and has confirmed that the requester's identity is as claimed.
--
-- /Note:/ Consider using 'isVerifiedAuthor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsIsVerifiedAuthor :: Lens.Lens' UpdateApplicationResponse (Core.Maybe Core.Bool)
uarrsIsVerifiedAuthor = Lens.field @"isVerifiedAuthor"
{-# DEPRECATED uarrsIsVerifiedAuthor "Use generic-lens or generic-optics with 'isVerifiedAuthor' instead." #-}

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsLabels :: Lens.Lens' UpdateApplicationResponse (Core.Maybe [Core.Text])
uarrsLabels = Lens.field @"labels"
{-# DEPRECATED uarrsLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | A link to a license file of the app that matches the spdxLicenseID value of your application.
--
-- Maximum size 5 MB
--
-- /Note:/ Consider using 'licenseUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsLicenseUrl :: Lens.Lens' UpdateApplicationResponse (Core.Maybe Core.Text)
uarrsLicenseUrl = Lens.field @"licenseUrl"
{-# DEPRECATED uarrsLicenseUrl "Use generic-lens or generic-optics with 'licenseUrl' instead." #-}

-- | The name of the application.
--
-- Minimum length=1. Maximum length=140
-- Pattern: "[a-zA-Z0-9\\-]+";
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsName :: Lens.Lens' UpdateApplicationResponse (Core.Maybe Core.Text)
uarrsName = Lens.field @"name"
{-# DEPRECATED uarrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A link to the readme file in Markdown language that contains a more detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- /Note:/ Consider using 'readmeUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsReadmeUrl :: Lens.Lens' UpdateApplicationResponse (Core.Maybe Core.Text)
uarrsReadmeUrl = Lens.field @"readmeUrl"
{-# DEPRECATED uarrsReadmeUrl "Use generic-lens or generic-optics with 'readmeUrl' instead." #-}

-- | A valid identifier from https://spdx.org/licenses/.
--
-- /Note:/ Consider using 'spdxLicenseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsSpdxLicenseId :: Lens.Lens' UpdateApplicationResponse (Core.Maybe Core.Text)
uarrsSpdxLicenseId = Lens.field @"spdxLicenseId"
{-# DEPRECATED uarrsSpdxLicenseId "Use generic-lens or generic-optics with 'spdxLicenseId' instead." #-}

-- | The URL to the public profile of a verified author. This URL is submitted by the author.
--
-- /Note:/ Consider using 'verifiedAuthorUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsVerifiedAuthorUrl :: Lens.Lens' UpdateApplicationResponse (Core.Maybe Core.Text)
uarrsVerifiedAuthorUrl = Lens.field @"verifiedAuthorUrl"
{-# DEPRECATED uarrsVerifiedAuthorUrl "Use generic-lens or generic-optics with 'verifiedAuthorUrl' instead." #-}

-- | Version information about the application.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsVersion :: Lens.Lens' UpdateApplicationResponse (Core.Maybe Types.Version)
uarrsVersion = Lens.field @"version"
{-# DEPRECATED uarrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarrsResponseStatus :: Lens.Lens' UpdateApplicationResponse Core.Int
uarrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED uarrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
