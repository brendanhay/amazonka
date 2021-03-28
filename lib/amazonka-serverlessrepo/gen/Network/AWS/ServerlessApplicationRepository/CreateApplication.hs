{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.CreateApplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application, optionally including an AWS SAM file to create the first application version in the same call.
module Network.AWS.ServerlessApplicationRepository.CreateApplication
    (
    -- * Creating a request
      CreateApplication (..)
    , mkCreateApplication
    -- ** Request lenses
    , caDescription
    , caName
    , caAuthor
    , caHomePageUrl
    , caLabels
    , caLicenseBody
    , caLicenseUrl
    , caReadmeBody
    , caReadmeUrl
    , caSemanticVersion
    , caSourceCodeArchiveUrl
    , caSourceCodeUrl
    , caSpdxLicenseId
    , caTemplateBody
    , caTemplateUrl

    -- * Destructuring the response
    , CreateApplicationResponse (..)
    , mkCreateApplicationResponse
    -- ** Response lenses
    , carrsApplicationId
    , carrsAuthor
    , carrsCreationTime
    , carrsDescription
    , carrsHomePageUrl
    , carrsIsVerifiedAuthor
    , carrsLabels
    , carrsLicenseUrl
    , carrsName
    , carrsReadmeUrl
    , carrsSpdxLicenseId
    , carrsVerifiedAuthorUrl
    , carrsVersion
    , carrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.ServerlessApplicationRepository.Types as Types

-- | /See:/ 'mkCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { description :: Core.Text
    -- ^ The description of the application.
--
-- Minimum length=1. Maximum length=256
  , name :: Core.Text
    -- ^ The name of the application that you want to publish.
--
-- Minimum length=1. Maximum length=140
-- Pattern: "[a-zA-Z0-9\\-]+";
  , author :: Core.Text
    -- ^ The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
  , homePageUrl :: Core.Maybe Core.Text
    -- ^ A URL with more information about the application, for example the location of your GitHub repository for the application.
  , labels :: Core.Maybe [Core.Text]
    -- ^ Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
  , licenseBody :: Core.Maybe Core.Text
    -- ^ A local text file that contains the license of the app that matches the spdxLicenseID value of your application.
--
--  The file has the format file://<path>/<filename>.
-- Maximum size 5 MB
-- You can specify only one of licenseBody and licenseUrl; otherwise, an error results.
  , licenseUrl :: Core.Maybe Core.Text
    -- ^ A link to the S3 object that contains the license of the app that matches the spdxLicenseID value of your application.
--
-- Maximum size 5 MB
-- You can specify only one of licenseBody and licenseUrl; otherwise, an error results.
  , readmeBody :: Core.Maybe Core.Text
    -- ^ A local text readme file in Markdown language that contains a more detailed description of the application and how it works.
--
--  The file has the format file://<path>/<filename>.
-- Maximum size 5 MB
-- You can specify only one of readmeBody and readmeUrl; otherwise, an error results.
  , readmeUrl :: Core.Maybe Core.Text
    -- ^ A link to the S3 object in Markdown language that contains a more detailed description of the application and how it works.
--
-- Maximum size 5 MB
-- You can specify only one of readmeBody and readmeUrl; otherwise, an error results.
  , semanticVersion :: Core.Maybe Core.Text
    -- ^ The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/> 
  , sourceCodeArchiveUrl :: Core.Maybe Core.Text
    -- ^ A link to the S3 object that contains the ZIP archive of the source code for this version of your application.
--
-- Maximum size 50 MB
  , sourceCodeUrl :: Core.Maybe Core.Text
    -- ^ A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
  , spdxLicenseId :: Core.Maybe Core.Text
    -- ^ A valid identifier from <https://spdx.org/licenses/ https://spdx.org/licenses/> .
  , templateBody :: Core.Maybe Core.Text
    -- ^ The local raw packaged AWS SAM template file of your application.
--
--  The file has the format file://<path>/<filename>.
-- You can specify only one of templateBody and templateUrl; otherwise an error results.
  , templateUrl :: Core.Maybe Core.Text
    -- ^ A link to the S3 object containing the packaged AWS SAM template of your application.
--
-- You can specify only one of templateBody and templateUrl; otherwise an error results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApplication' value with any optional fields omitted.
mkCreateApplication
    :: Core.Text -- ^ 'description'
    -> Core.Text -- ^ 'name'
    -> Core.Text -- ^ 'author'
    -> CreateApplication
mkCreateApplication description name author
  = CreateApplication'{description, name, author,
                       homePageUrl = Core.Nothing, labels = Core.Nothing,
                       licenseBody = Core.Nothing, licenseUrl = Core.Nothing,
                       readmeBody = Core.Nothing, readmeUrl = Core.Nothing,
                       semanticVersion = Core.Nothing,
                       sourceCodeArchiveUrl = Core.Nothing, sourceCodeUrl = Core.Nothing,
                       spdxLicenseId = Core.Nothing, templateBody = Core.Nothing,
                       templateUrl = Core.Nothing}

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateApplication Core.Text
caDescription = Lens.field @"description"
{-# INLINEABLE caDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The name of the application that you want to publish.
--
-- Minimum length=1. Maximum length=140
-- Pattern: "[a-zA-Z0-9\\-]+";
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' CreateApplication Core.Text
caName = Lens.field @"name"
{-# INLINEABLE caName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- /Note:/ Consider using 'author' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAuthor :: Lens.Lens' CreateApplication Core.Text
caAuthor = Lens.field @"author"
{-# INLINEABLE caAuthor #-}
{-# DEPRECATED author "Use generic-lens or generic-optics with 'author' instead"  #-}

-- | A URL with more information about the application, for example the location of your GitHub repository for the application.
--
-- /Note:/ Consider using 'homePageUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caHomePageUrl :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
caHomePageUrl = Lens.field @"homePageUrl"
{-# INLINEABLE caHomePageUrl #-}
{-# DEPRECATED homePageUrl "Use generic-lens or generic-optics with 'homePageUrl' instead"  #-}

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caLabels :: Lens.Lens' CreateApplication (Core.Maybe [Core.Text])
caLabels = Lens.field @"labels"
{-# INLINEABLE caLabels #-}
{-# DEPRECATED labels "Use generic-lens or generic-optics with 'labels' instead"  #-}

-- | A local text file that contains the license of the app that matches the spdxLicenseID value of your application.
--
--  The file has the format file://<path>/<filename>.
-- Maximum size 5 MB
-- You can specify only one of licenseBody and licenseUrl; otherwise, an error results.
--
-- /Note:/ Consider using 'licenseBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caLicenseBody :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
caLicenseBody = Lens.field @"licenseBody"
{-# INLINEABLE caLicenseBody #-}
{-# DEPRECATED licenseBody "Use generic-lens or generic-optics with 'licenseBody' instead"  #-}

-- | A link to the S3 object that contains the license of the app that matches the spdxLicenseID value of your application.
--
-- Maximum size 5 MB
-- You can specify only one of licenseBody and licenseUrl; otherwise, an error results.
--
-- /Note:/ Consider using 'licenseUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caLicenseUrl :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
caLicenseUrl = Lens.field @"licenseUrl"
{-# INLINEABLE caLicenseUrl #-}
{-# DEPRECATED licenseUrl "Use generic-lens or generic-optics with 'licenseUrl' instead"  #-}

-- | A local text readme file in Markdown language that contains a more detailed description of the application and how it works.
--
--  The file has the format file://<path>/<filename>.
-- Maximum size 5 MB
-- You can specify only one of readmeBody and readmeUrl; otherwise, an error results.
--
-- /Note:/ Consider using 'readmeBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caReadmeBody :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
caReadmeBody = Lens.field @"readmeBody"
{-# INLINEABLE caReadmeBody #-}
{-# DEPRECATED readmeBody "Use generic-lens or generic-optics with 'readmeBody' instead"  #-}

-- | A link to the S3 object in Markdown language that contains a more detailed description of the application and how it works.
--
-- Maximum size 5 MB
-- You can specify only one of readmeBody and readmeUrl; otherwise, an error results.
--
-- /Note:/ Consider using 'readmeUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caReadmeUrl :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
caReadmeUrl = Lens.field @"readmeUrl"
{-# INLINEABLE caReadmeUrl #-}
{-# DEPRECATED readmeUrl "Use generic-lens or generic-optics with 'readmeUrl' instead"  #-}

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/> 
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caSemanticVersion :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
caSemanticVersion = Lens.field @"semanticVersion"
{-# INLINEABLE caSemanticVersion #-}
{-# DEPRECATED semanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead"  #-}

-- | A link to the S3 object that contains the ZIP archive of the source code for this version of your application.
--
-- Maximum size 50 MB
--
-- /Note:/ Consider using 'sourceCodeArchiveUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caSourceCodeArchiveUrl :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
caSourceCodeArchiveUrl = Lens.field @"sourceCodeArchiveUrl"
{-# INLINEABLE caSourceCodeArchiveUrl #-}
{-# DEPRECATED sourceCodeArchiveUrl "Use generic-lens or generic-optics with 'sourceCodeArchiveUrl' instead"  #-}

-- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
--
-- /Note:/ Consider using 'sourceCodeUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caSourceCodeUrl :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
caSourceCodeUrl = Lens.field @"sourceCodeUrl"
{-# INLINEABLE caSourceCodeUrl #-}
{-# DEPRECATED sourceCodeUrl "Use generic-lens or generic-optics with 'sourceCodeUrl' instead"  #-}

-- | A valid identifier from <https://spdx.org/licenses/ https://spdx.org/licenses/> .
--
-- /Note:/ Consider using 'spdxLicenseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caSpdxLicenseId :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
caSpdxLicenseId = Lens.field @"spdxLicenseId"
{-# INLINEABLE caSpdxLicenseId #-}
{-# DEPRECATED spdxLicenseId "Use generic-lens or generic-optics with 'spdxLicenseId' instead"  #-}

-- | The local raw packaged AWS SAM template file of your application.
--
--  The file has the format file://<path>/<filename>.
-- You can specify only one of templateBody and templateUrl; otherwise an error results.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTemplateBody :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
caTemplateBody = Lens.field @"templateBody"
{-# INLINEABLE caTemplateBody #-}
{-# DEPRECATED templateBody "Use generic-lens or generic-optics with 'templateBody' instead"  #-}

-- | A link to the S3 object containing the packaged AWS SAM template of your application.
--
-- You can specify only one of templateBody and templateUrl; otherwise an error results.
--
-- /Note:/ Consider using 'templateUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTemplateUrl :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
caTemplateUrl = Lens.field @"templateUrl"
{-# INLINEABLE caTemplateUrl #-}
{-# DEPRECATED templateUrl "Use generic-lens or generic-optics with 'templateUrl' instead"  #-}

instance Core.ToQuery CreateApplication where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateApplication where
        toHeaders CreateApplication{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateApplication where
        toJSON CreateApplication{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("description" Core..= description),
                  Core.Just ("name" Core..= name),
                  Core.Just ("author" Core..= author),
                  ("homePageUrl" Core..=) Core.<$> homePageUrl,
                  ("labels" Core..=) Core.<$> labels,
                  ("licenseBody" Core..=) Core.<$> licenseBody,
                  ("licenseUrl" Core..=) Core.<$> licenseUrl,
                  ("readmeBody" Core..=) Core.<$> readmeBody,
                  ("readmeUrl" Core..=) Core.<$> readmeUrl,
                  ("semanticVersion" Core..=) Core.<$> semanticVersion,
                  ("sourceCodeArchiveUrl" Core..=) Core.<$> sourceCodeArchiveUrl,
                  ("sourceCodeUrl" Core..=) Core.<$> sourceCodeUrl,
                  ("spdxLicenseId" Core..=) Core.<$> spdxLicenseId,
                  ("templateBody" Core..=) Core.<$> templateBody,
                  ("templateUrl" Core..=) Core.<$> templateUrl])

instance Core.AWSRequest CreateApplication where
        type Rs CreateApplication = CreateApplicationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/applications",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 CreateApplicationResponse' Core.<$>
                   (x Core..:? "applicationId") Core.<*> x Core..:? "author" Core.<*>
                     x Core..:? "creationTime"
                     Core.<*> x Core..:? "description"
                     Core.<*> x Core..:? "homePageUrl"
                     Core.<*> x Core..:? "isVerifiedAuthor"
                     Core.<*> x Core..:? "labels"
                     Core.<*> x Core..:? "licenseUrl"
                     Core.<*> x Core..:? "name"
                     Core.<*> x Core..:? "readmeUrl"
                     Core.<*> x Core..:? "spdxLicenseId"
                     Core.<*> x Core..:? "verifiedAuthorUrl"
                     Core.<*> x Core..:? "version"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { applicationId :: Core.Maybe Core.Text
    -- ^ The application Amazon Resource Name (ARN).
  , author :: Core.Maybe Core.Text
    -- ^ The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
  , creationTime :: Core.Maybe Core.Text
    -- ^ The date and time this resource was created.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the application.
--
-- Minimum length=1. Maximum length=256
  , homePageUrl :: Core.Maybe Core.Text
    -- ^ A URL with more information about the application, for example the location of your GitHub repository for the application.
  , isVerifiedAuthor :: Core.Maybe Core.Bool
    -- ^ Whether the author of this application has been verified. This means means that AWS has made a good faith review, as a reasonable and prudent service provider, of the information provided by the requester and has confirmed that the requester's identity is as claimed.
  , labels :: Core.Maybe [Core.Text]
    -- ^ Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
  , licenseUrl :: Core.Maybe Core.Text
    -- ^ A link to a license file of the app that matches the spdxLicenseID value of your application.
--
-- Maximum size 5 MB
  , name :: Core.Maybe Core.Text
    -- ^ The name of the application.
--
-- Minimum length=1. Maximum length=140
-- Pattern: "[a-zA-Z0-9\\-]+";
  , readmeUrl :: Core.Maybe Core.Text
    -- ^ A link to the readme file in Markdown language that contains a more detailed description of the application and how it works.
--
-- Maximum size 5 MB
  , spdxLicenseId :: Core.Maybe Core.Text
    -- ^ A valid identifier from https://spdx.org/licenses/.
  , verifiedAuthorUrl :: Core.Maybe Core.Text
    -- ^ The URL to the public profile of a verified author. This URL is submitted by the author.
  , version :: Core.Maybe Types.Version
    -- ^ Version information about the application.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateApplicationResponse' value with any optional fields omitted.
mkCreateApplicationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateApplicationResponse
mkCreateApplicationResponse responseStatus
  = CreateApplicationResponse'{applicationId = Core.Nothing,
                               author = Core.Nothing, creationTime = Core.Nothing,
                               description = Core.Nothing, homePageUrl = Core.Nothing,
                               isVerifiedAuthor = Core.Nothing, labels = Core.Nothing,
                               licenseUrl = Core.Nothing, name = Core.Nothing,
                               readmeUrl = Core.Nothing, spdxLicenseId = Core.Nothing,
                               verifiedAuthorUrl = Core.Nothing, version = Core.Nothing,
                               responseStatus}

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsApplicationId :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
carrsApplicationId = Lens.field @"applicationId"
{-# INLINEABLE carrsApplicationId #-}
{-# DEPRECATED applicationId "Use generic-lens or generic-optics with 'applicationId' instead"  #-}

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- /Note:/ Consider using 'author' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsAuthor :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
carrsAuthor = Lens.field @"author"
{-# INLINEABLE carrsAuthor #-}
{-# DEPRECATED author "Use generic-lens or generic-optics with 'author' instead"  #-}

-- | The date and time this resource was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsCreationTime :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
carrsCreationTime = Lens.field @"creationTime"
{-# INLINEABLE carrsCreationTime #-}
{-# DEPRECATED creationTime "Use generic-lens or generic-optics with 'creationTime' instead"  #-}

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsDescription :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
carrsDescription = Lens.field @"description"
{-# INLINEABLE carrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A URL with more information about the application, for example the location of your GitHub repository for the application.
--
-- /Note:/ Consider using 'homePageUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsHomePageUrl :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
carrsHomePageUrl = Lens.field @"homePageUrl"
{-# INLINEABLE carrsHomePageUrl #-}
{-# DEPRECATED homePageUrl "Use generic-lens or generic-optics with 'homePageUrl' instead"  #-}

-- | Whether the author of this application has been verified. This means means that AWS has made a good faith review, as a reasonable and prudent service provider, of the information provided by the requester and has confirmed that the requester's identity is as claimed.
--
-- /Note:/ Consider using 'isVerifiedAuthor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsIsVerifiedAuthor :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Bool)
carrsIsVerifiedAuthor = Lens.field @"isVerifiedAuthor"
{-# INLINEABLE carrsIsVerifiedAuthor #-}
{-# DEPRECATED isVerifiedAuthor "Use generic-lens or generic-optics with 'isVerifiedAuthor' instead"  #-}

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsLabels :: Lens.Lens' CreateApplicationResponse (Core.Maybe [Core.Text])
carrsLabels = Lens.field @"labels"
{-# INLINEABLE carrsLabels #-}
{-# DEPRECATED labels "Use generic-lens or generic-optics with 'labels' instead"  #-}

-- | A link to a license file of the app that matches the spdxLicenseID value of your application.
--
-- Maximum size 5 MB
--
-- /Note:/ Consider using 'licenseUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsLicenseUrl :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
carrsLicenseUrl = Lens.field @"licenseUrl"
{-# INLINEABLE carrsLicenseUrl #-}
{-# DEPRECATED licenseUrl "Use generic-lens or generic-optics with 'licenseUrl' instead"  #-}

-- | The name of the application.
--
-- Minimum length=1. Maximum length=140
-- Pattern: "[a-zA-Z0-9\\-]+";
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsName :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
carrsName = Lens.field @"name"
{-# INLINEABLE carrsName #-}
{-# DEPRECATED name "Use generic-lens or generic-optics with 'name' instead"  #-}

-- | A link to the readme file in Markdown language that contains a more detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- /Note:/ Consider using 'readmeUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsReadmeUrl :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
carrsReadmeUrl = Lens.field @"readmeUrl"
{-# INLINEABLE carrsReadmeUrl #-}
{-# DEPRECATED readmeUrl "Use generic-lens or generic-optics with 'readmeUrl' instead"  #-}

-- | A valid identifier from https://spdx.org/licenses/.
--
-- /Note:/ Consider using 'spdxLicenseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsSpdxLicenseId :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
carrsSpdxLicenseId = Lens.field @"spdxLicenseId"
{-# INLINEABLE carrsSpdxLicenseId #-}
{-# DEPRECATED spdxLicenseId "Use generic-lens or generic-optics with 'spdxLicenseId' instead"  #-}

-- | The URL to the public profile of a verified author. This URL is submitted by the author.
--
-- /Note:/ Consider using 'verifiedAuthorUrl' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsVerifiedAuthorUrl :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
carrsVerifiedAuthorUrl = Lens.field @"verifiedAuthorUrl"
{-# INLINEABLE carrsVerifiedAuthorUrl #-}
{-# DEPRECATED verifiedAuthorUrl "Use generic-lens or generic-optics with 'verifiedAuthorUrl' instead"  #-}

-- | Version information about the application.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsVersion :: Lens.Lens' CreateApplicationResponse (Core.Maybe Types.Version)
carrsVersion = Lens.field @"version"
{-# INLINEABLE carrsVersion #-}
{-# DEPRECATED version "Use generic-lens or generic-optics with 'version' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carrsResponseStatus :: Lens.Lens' CreateApplicationResponse Core.Int
carrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE carrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
