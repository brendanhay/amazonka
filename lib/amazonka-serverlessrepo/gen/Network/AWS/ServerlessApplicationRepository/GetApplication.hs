{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.GetApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified application.
module Network.AWS.ServerlessApplicationRepository.GetApplication
  ( -- * Creating a request
    GetApplication (..),
    mkGetApplication,

    -- ** Request lenses
    gaApplicationId,
    gaSemanticVersion,

    -- * Destructuring the response
    GetApplicationResponse (..),
    mkGetApplicationResponse,

    -- ** Response lenses
    garrsApplicationId,
    garrsAuthor,
    garrsCreationTime,
    garrsDescription,
    garrsHomePageUrl,
    garrsIsVerifiedAuthor,
    garrsLabels,
    garrsLicenseUrl,
    garrsName,
    garrsReadmeUrl,
    garrsSpdxLicenseId,
    garrsVerifiedAuthorUrl,
    garrsVersion,
    garrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServerlessApplicationRepository.Types as Types

-- | /See:/ 'mkGetApplication' smart constructor.
data GetApplication = GetApplication'
  { -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Core.Text,
    -- | The semantic version of the application to get.
    semanticVersion :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetApplication' value with any optional fields omitted.
mkGetApplication ::
  -- | 'applicationId'
  Core.Text ->
  GetApplication
mkGetApplication applicationId =
  GetApplication' {applicationId, semanticVersion = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaApplicationId :: Lens.Lens' GetApplication Core.Text
gaApplicationId = Lens.field @"applicationId"
{-# DEPRECATED gaApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The semantic version of the application to get.
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaSemanticVersion :: Lens.Lens' GetApplication (Core.Maybe Core.Text)
gaSemanticVersion = Lens.field @"semanticVersion"
{-# DEPRECATED gaSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

instance Core.AWSRequest GetApplication where
  type Rs GetApplication = GetApplicationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ("/applications/" Core.<> (Core.toText applicationId)),
        Core._rqQuery =
          Core.toQueryValue "semanticVersion" Core.<$> semanticVersion,
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationResponse'
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

-- | /See:/ 'mkGetApplicationResponse' smart constructor.
data GetApplicationResponse = GetApplicationResponse'
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

-- | Creates a 'GetApplicationResponse' value with any optional fields omitted.
mkGetApplicationResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetApplicationResponse
mkGetApplicationResponse responseStatus =
  GetApplicationResponse'
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
garrsApplicationId :: Lens.Lens' GetApplicationResponse (Core.Maybe Core.Text)
garrsApplicationId = Lens.field @"applicationId"
{-# DEPRECATED garrsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- /Note:/ Consider using 'author' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsAuthor :: Lens.Lens' GetApplicationResponse (Core.Maybe Core.Text)
garrsAuthor = Lens.field @"author"
{-# DEPRECATED garrsAuthor "Use generic-lens or generic-optics with 'author' instead." #-}

-- | The date and time this resource was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsCreationTime :: Lens.Lens' GetApplicationResponse (Core.Maybe Core.Text)
garrsCreationTime = Lens.field @"creationTime"
{-# DEPRECATED garrsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsDescription :: Lens.Lens' GetApplicationResponse (Core.Maybe Core.Text)
garrsDescription = Lens.field @"description"
{-# DEPRECATED garrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A URL with more information about the application, for example the location of your GitHub repository for the application.
--
-- /Note:/ Consider using 'homePageUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsHomePageUrl :: Lens.Lens' GetApplicationResponse (Core.Maybe Core.Text)
garrsHomePageUrl = Lens.field @"homePageUrl"
{-# DEPRECATED garrsHomePageUrl "Use generic-lens or generic-optics with 'homePageUrl' instead." #-}

-- | Whether the author of this application has been verified. This means means that AWS has made a good faith review, as a reasonable and prudent service provider, of the information provided by the requester and has confirmed that the requester's identity is as claimed.
--
-- /Note:/ Consider using 'isVerifiedAuthor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsIsVerifiedAuthor :: Lens.Lens' GetApplicationResponse (Core.Maybe Core.Bool)
garrsIsVerifiedAuthor = Lens.field @"isVerifiedAuthor"
{-# DEPRECATED garrsIsVerifiedAuthor "Use generic-lens or generic-optics with 'isVerifiedAuthor' instead." #-}

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsLabels :: Lens.Lens' GetApplicationResponse (Core.Maybe [Core.Text])
garrsLabels = Lens.field @"labels"
{-# DEPRECATED garrsLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | A link to a license file of the app that matches the spdxLicenseID value of your application.
--
-- Maximum size 5 MB
--
-- /Note:/ Consider using 'licenseUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsLicenseUrl :: Lens.Lens' GetApplicationResponse (Core.Maybe Core.Text)
garrsLicenseUrl = Lens.field @"licenseUrl"
{-# DEPRECATED garrsLicenseUrl "Use generic-lens or generic-optics with 'licenseUrl' instead." #-}

-- | The name of the application.
--
-- Minimum length=1. Maximum length=140
-- Pattern: "[a-zA-Z0-9\\-]+";
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsName :: Lens.Lens' GetApplicationResponse (Core.Maybe Core.Text)
garrsName = Lens.field @"name"
{-# DEPRECATED garrsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | A link to the readme file in Markdown language that contains a more detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- /Note:/ Consider using 'readmeUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsReadmeUrl :: Lens.Lens' GetApplicationResponse (Core.Maybe Core.Text)
garrsReadmeUrl = Lens.field @"readmeUrl"
{-# DEPRECATED garrsReadmeUrl "Use generic-lens or generic-optics with 'readmeUrl' instead." #-}

-- | A valid identifier from https://spdx.org/licenses/.
--
-- /Note:/ Consider using 'spdxLicenseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsSpdxLicenseId :: Lens.Lens' GetApplicationResponse (Core.Maybe Core.Text)
garrsSpdxLicenseId = Lens.field @"spdxLicenseId"
{-# DEPRECATED garrsSpdxLicenseId "Use generic-lens or generic-optics with 'spdxLicenseId' instead." #-}

-- | The URL to the public profile of a verified author. This URL is submitted by the author.
--
-- /Note:/ Consider using 'verifiedAuthorUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsVerifiedAuthorUrl :: Lens.Lens' GetApplicationResponse (Core.Maybe Core.Text)
garrsVerifiedAuthorUrl = Lens.field @"verifiedAuthorUrl"
{-# DEPRECATED garrsVerifiedAuthorUrl "Use generic-lens or generic-optics with 'verifiedAuthorUrl' instead." #-}

-- | Version information about the application.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsVersion :: Lens.Lens' GetApplicationResponse (Core.Maybe Types.Version)
garrsVersion = Lens.field @"version"
{-# DEPRECATED garrsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garrsResponseStatus :: Lens.Lens' GetApplicationResponse Core.Int
garrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED garrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
