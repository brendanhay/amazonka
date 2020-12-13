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
    gaSemanticVersion,
    gaApplicationId,

    -- * Destructuring the response
    GetApplicationResponse (..),
    mkGetApplicationResponse,

    -- ** Response lenses
    garsCreationTime,
    garsHomePageURL,
    garsLicenseURL,
    garsReadmeURL,
    garsApplicationId,
    garsName,
    garsVersion,
    garsAuthor,
    garsLabels,
    garsVerifiedAuthorURL,
    garsDescription,
    garsSpdxLicenseId,
    garsIsVerifiedAuthor,
    garsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'mkGetApplication' smart constructor.
data GetApplication = GetApplication'
  { -- | The semantic version of the application to get.
    semanticVersion :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetApplication' with the minimum fields required to make a request.
--
-- * 'semanticVersion' - The semantic version of the application to get.
-- * 'applicationId' - The Amazon Resource Name (ARN) of the application.
mkGetApplication ::
  -- | 'applicationId'
  Lude.Text ->
  GetApplication
mkGetApplication pApplicationId_ =
  GetApplication'
    { semanticVersion = Lude.Nothing,
      applicationId = pApplicationId_
    }

-- | The semantic version of the application to get.
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaSemanticVersion :: Lens.Lens' GetApplication (Lude.Maybe Lude.Text)
gaSemanticVersion = Lens.lens (semanticVersion :: GetApplication -> Lude.Maybe Lude.Text) (\s a -> s {semanticVersion = a} :: GetApplication)
{-# DEPRECATED gaSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gaApplicationId :: Lens.Lens' GetApplication Lude.Text
gaApplicationId = Lens.lens (applicationId :: GetApplication -> Lude.Text) (\s a -> s {applicationId = a} :: GetApplication)
{-# DEPRECATED gaApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

instance Lude.AWSRequest GetApplication where
  type Rs GetApplication = GetApplicationResponse
  request = Req.get serverlessApplicationRepositoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetApplicationResponse'
            Lude.<$> (x Lude..?> "creationTime")
            Lude.<*> (x Lude..?> "homePageUrl")
            Lude.<*> (x Lude..?> "licenseUrl")
            Lude.<*> (x Lude..?> "readmeUrl")
            Lude.<*> (x Lude..?> "applicationId")
            Lude.<*> (x Lude..?> "name")
            Lude.<*> (x Lude..?> "version")
            Lude.<*> (x Lude..?> "author")
            Lude.<*> (x Lude..?> "labels" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "verifiedAuthorUrl")
            Lude.<*> (x Lude..?> "description")
            Lude.<*> (x Lude..?> "spdxLicenseId")
            Lude.<*> (x Lude..?> "isVerifiedAuthor")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetApplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToPath GetApplication where
  toPath GetApplication' {..} =
    Lude.mconcat ["/applications/", Lude.toBS applicationId]

instance Lude.ToQuery GetApplication where
  toQuery GetApplication' {..} =
    Lude.mconcat ["semanticVersion" Lude.=: semanticVersion]

-- | /See:/ 'mkGetApplicationResponse' smart constructor.
data GetApplicationResponse = GetApplicationResponse'
  { -- | The date and time this resource was created.
    creationTime :: Lude.Maybe Lude.Text,
    -- | A URL with more information about the application, for example the location of your GitHub repository for the application.
    homePageURL :: Lude.Maybe Lude.Text,
    -- | A link to a license file of the app that matches the spdxLicenseID value of your application.
    --
    -- Maximum size 5 MB
    licenseURL :: Lude.Maybe Lude.Text,
    -- | A link to the readme file in Markdown language that contains a more detailed description of the application and how it works.
    --
    -- Maximum size 5 MB
    readmeURL :: Lude.Maybe Lude.Text,
    -- | The application Amazon Resource Name (ARN).
    applicationId :: Lude.Maybe Lude.Text,
    -- | The name of the application.
    --
    -- Minimum length=1. Maximum length=140
    -- Pattern: "[a-zA-Z0-9\\-]+";
    name :: Lude.Maybe Lude.Text,
    -- | Version information about the application.
    version :: Lude.Maybe Version,
    -- | The name of the author publishing the app.
    --
    -- Minimum length=1. Maximum length=127.
    -- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
    author :: Lude.Maybe Lude.Text,
    -- | Labels to improve discovery of apps in search results.
    --
    -- Minimum length=1. Maximum length=127. Maximum number of labels: 10
    -- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
    labels :: Lude.Maybe [Lude.Text],
    -- | The URL to the public profile of a verified author. This URL is submitted by the author.
    verifiedAuthorURL :: Lude.Maybe Lude.Text,
    -- | The description of the application.
    --
    -- Minimum length=1. Maximum length=256
    description :: Lude.Maybe Lude.Text,
    -- | A valid identifier from https://spdx.org/licenses/.
    spdxLicenseId :: Lude.Maybe Lude.Text,
    -- | Whether the author of this application has been verified. This means means that AWS has made a good faith review, as a reasonable and prudent service provider, of the information provided by the requester and has confirmed that the requester's identity is as claimed.
    isVerifiedAuthor :: Lude.Maybe Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetApplicationResponse' with the minimum fields required to make a request.
--
-- * 'creationTime' - The date and time this resource was created.
-- * 'homePageURL' - A URL with more information about the application, for example the location of your GitHub repository for the application.
-- * 'licenseURL' - A link to a license file of the app that matches the spdxLicenseID value of your application.
--
-- Maximum size 5 MB
-- * 'readmeURL' - A link to the readme file in Markdown language that contains a more detailed description of the application and how it works.
--
-- Maximum size 5 MB
-- * 'applicationId' - The application Amazon Resource Name (ARN).
-- * 'name' - The name of the application.
--
-- Minimum length=1. Maximum length=140
-- Pattern: "[a-zA-Z0-9\\-]+";
-- * 'version' - Version information about the application.
-- * 'author' - The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
-- * 'labels' - Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
-- * 'verifiedAuthorURL' - The URL to the public profile of a verified author. This URL is submitted by the author.
-- * 'description' - The description of the application.
--
-- Minimum length=1. Maximum length=256
-- * 'spdxLicenseId' - A valid identifier from https://spdx.org/licenses/.
-- * 'isVerifiedAuthor' - Whether the author of this application has been verified. This means means that AWS has made a good faith review, as a reasonable and prudent service provider, of the information provided by the requester and has confirmed that the requester's identity is as claimed.
-- * 'responseStatus' - The response status code.
mkGetApplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetApplicationResponse
mkGetApplicationResponse pResponseStatus_ =
  GetApplicationResponse'
    { creationTime = Lude.Nothing,
      homePageURL = Lude.Nothing,
      licenseURL = Lude.Nothing,
      readmeURL = Lude.Nothing,
      applicationId = Lude.Nothing,
      name = Lude.Nothing,
      version = Lude.Nothing,
      author = Lude.Nothing,
      labels = Lude.Nothing,
      verifiedAuthorURL = Lude.Nothing,
      description = Lude.Nothing,
      spdxLicenseId = Lude.Nothing,
      isVerifiedAuthor = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The date and time this resource was created.
--
-- /Note:/ Consider using 'creationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsCreationTime :: Lens.Lens' GetApplicationResponse (Lude.Maybe Lude.Text)
garsCreationTime = Lens.lens (creationTime :: GetApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTime = a} :: GetApplicationResponse)
{-# DEPRECATED garsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A URL with more information about the application, for example the location of your GitHub repository for the application.
--
-- /Note:/ Consider using 'homePageURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsHomePageURL :: Lens.Lens' GetApplicationResponse (Lude.Maybe Lude.Text)
garsHomePageURL = Lens.lens (homePageURL :: GetApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {homePageURL = a} :: GetApplicationResponse)
{-# DEPRECATED garsHomePageURL "Use generic-lens or generic-optics with 'homePageURL' instead." #-}

-- | A link to a license file of the app that matches the spdxLicenseID value of your application.
--
-- Maximum size 5 MB
--
-- /Note:/ Consider using 'licenseURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsLicenseURL :: Lens.Lens' GetApplicationResponse (Lude.Maybe Lude.Text)
garsLicenseURL = Lens.lens (licenseURL :: GetApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {licenseURL = a} :: GetApplicationResponse)
{-# DEPRECATED garsLicenseURL "Use generic-lens or generic-optics with 'licenseURL' instead." #-}

-- | A link to the readme file in Markdown language that contains a more detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- /Note:/ Consider using 'readmeURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsReadmeURL :: Lens.Lens' GetApplicationResponse (Lude.Maybe Lude.Text)
garsReadmeURL = Lens.lens (readmeURL :: GetApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {readmeURL = a} :: GetApplicationResponse)
{-# DEPRECATED garsReadmeURL "Use generic-lens or generic-optics with 'readmeURL' instead." #-}

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsApplicationId :: Lens.Lens' GetApplicationResponse (Lude.Maybe Lude.Text)
garsApplicationId = Lens.lens (applicationId :: GetApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: GetApplicationResponse)
{-# DEPRECATED garsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The name of the application.
--
-- Minimum length=1. Maximum length=140
-- Pattern: "[a-zA-Z0-9\\-]+";
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsName :: Lens.Lens' GetApplicationResponse (Lude.Maybe Lude.Text)
garsName = Lens.lens (name :: GetApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: GetApplicationResponse)
{-# DEPRECATED garsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Version information about the application.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsVersion :: Lens.Lens' GetApplicationResponse (Lude.Maybe Version)
garsVersion = Lens.lens (version :: GetApplicationResponse -> Lude.Maybe Version) (\s a -> s {version = a} :: GetApplicationResponse)
{-# DEPRECATED garsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- /Note:/ Consider using 'author' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsAuthor :: Lens.Lens' GetApplicationResponse (Lude.Maybe Lude.Text)
garsAuthor = Lens.lens (author :: GetApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {author = a} :: GetApplicationResponse)
{-# DEPRECATED garsAuthor "Use generic-lens or generic-optics with 'author' instead." #-}

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsLabels :: Lens.Lens' GetApplicationResponse (Lude.Maybe [Lude.Text])
garsLabels = Lens.lens (labels :: GetApplicationResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {labels = a} :: GetApplicationResponse)
{-# DEPRECATED garsLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | The URL to the public profile of a verified author. This URL is submitted by the author.
--
-- /Note:/ Consider using 'verifiedAuthorURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsVerifiedAuthorURL :: Lens.Lens' GetApplicationResponse (Lude.Maybe Lude.Text)
garsVerifiedAuthorURL = Lens.lens (verifiedAuthorURL :: GetApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {verifiedAuthorURL = a} :: GetApplicationResponse)
{-# DEPRECATED garsVerifiedAuthorURL "Use generic-lens or generic-optics with 'verifiedAuthorURL' instead." #-}

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsDescription :: Lens.Lens' GetApplicationResponse (Lude.Maybe Lude.Text)
garsDescription = Lens.lens (description :: GetApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: GetApplicationResponse)
{-# DEPRECATED garsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A valid identifier from https://spdx.org/licenses/.
--
-- /Note:/ Consider using 'spdxLicenseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsSpdxLicenseId :: Lens.Lens' GetApplicationResponse (Lude.Maybe Lude.Text)
garsSpdxLicenseId = Lens.lens (spdxLicenseId :: GetApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {spdxLicenseId = a} :: GetApplicationResponse)
{-# DEPRECATED garsSpdxLicenseId "Use generic-lens or generic-optics with 'spdxLicenseId' instead." #-}

-- | Whether the author of this application has been verified. This means means that AWS has made a good faith review, as a reasonable and prudent service provider, of the information provided by the requester and has confirmed that the requester's identity is as claimed.
--
-- /Note:/ Consider using 'isVerifiedAuthor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsIsVerifiedAuthor :: Lens.Lens' GetApplicationResponse (Lude.Maybe Lude.Bool)
garsIsVerifiedAuthor = Lens.lens (isVerifiedAuthor :: GetApplicationResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isVerifiedAuthor = a} :: GetApplicationResponse)
{-# DEPRECATED garsIsVerifiedAuthor "Use generic-lens or generic-optics with 'isVerifiedAuthor' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
garsResponseStatus :: Lens.Lens' GetApplicationResponse Lude.Int
garsResponseStatus = Lens.lens (responseStatus :: GetApplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetApplicationResponse)
{-# DEPRECATED garsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
