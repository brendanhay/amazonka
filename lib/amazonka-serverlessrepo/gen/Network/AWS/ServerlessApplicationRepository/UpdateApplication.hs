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
    uaHomePageURL,
    uaReadmeBody,
    uaReadmeURL,
    uaApplicationId,
    uaAuthor,
    uaLabels,
    uaDescription,

    -- * Destructuring the response
    UpdateApplicationResponse (..),
    mkUpdateApplicationResponse,

    -- ** Response lenses
    uarsCreationTime,
    uarsHomePageURL,
    uarsLicenseURL,
    uarsReadmeURL,
    uarsApplicationId,
    uarsName,
    uarsVersion,
    uarsAuthor,
    uarsLabels,
    uarsVerifiedAuthorURL,
    uarsDescription,
    uarsSpdxLicenseId,
    uarsIsVerifiedAuthor,
    uarsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'mkUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | A URL with more information about the application, for example the location of your GitHub repository for the application.
    homePageURL :: Lude.Maybe Lude.Text,
    -- | A text readme file in Markdown language that contains a more detailed description of the application and how it works.
    --
    -- Maximum size 5 MB
    readmeBody :: Lude.Maybe Lude.Text,
    -- | A link to the readme file in Markdown language that contains a more detailed description of the application and how it works.
    --
    -- Maximum size 5 MB
    readmeURL :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Lude.Text,
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
    -- | The description of the application.
    --
    -- Minimum length=1. Maximum length=256
    description :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateApplication' with the minimum fields required to make a request.
--
-- * 'homePageURL' - A URL with more information about the application, for example the location of your GitHub repository for the application.
-- * 'readmeBody' - A text readme file in Markdown language that contains a more detailed description of the application and how it works.
--
-- Maximum size 5 MB
-- * 'readmeURL' - A link to the readme file in Markdown language that contains a more detailed description of the application and how it works.
--
-- Maximum size 5 MB
-- * 'applicationId' - The Amazon Resource Name (ARN) of the application.
-- * 'author' - The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
-- * 'labels' - Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
-- * 'description' - The description of the application.
--
-- Minimum length=1. Maximum length=256
mkUpdateApplication ::
  -- | 'applicationId'
  Lude.Text ->
  UpdateApplication
mkUpdateApplication pApplicationId_ =
  UpdateApplication'
    { homePageURL = Lude.Nothing,
      readmeBody = Lude.Nothing,
      readmeURL = Lude.Nothing,
      applicationId = pApplicationId_,
      author = Lude.Nothing,
      labels = Lude.Nothing,
      description = Lude.Nothing
    }

-- | A URL with more information about the application, for example the location of your GitHub repository for the application.
--
-- /Note:/ Consider using 'homePageURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaHomePageURL :: Lens.Lens' UpdateApplication (Lude.Maybe Lude.Text)
uaHomePageURL = Lens.lens (homePageURL :: UpdateApplication -> Lude.Maybe Lude.Text) (\s a -> s {homePageURL = a} :: UpdateApplication)
{-# DEPRECATED uaHomePageURL "Use generic-lens or generic-optics with 'homePageURL' instead." #-}

-- | A text readme file in Markdown language that contains a more detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- /Note:/ Consider using 'readmeBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaReadmeBody :: Lens.Lens' UpdateApplication (Lude.Maybe Lude.Text)
uaReadmeBody = Lens.lens (readmeBody :: UpdateApplication -> Lude.Maybe Lude.Text) (\s a -> s {readmeBody = a} :: UpdateApplication)
{-# DEPRECATED uaReadmeBody "Use generic-lens or generic-optics with 'readmeBody' instead." #-}

-- | A link to the readme file in Markdown language that contains a more detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- /Note:/ Consider using 'readmeURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaReadmeURL :: Lens.Lens' UpdateApplication (Lude.Maybe Lude.Text)
uaReadmeURL = Lens.lens (readmeURL :: UpdateApplication -> Lude.Maybe Lude.Text) (\s a -> s {readmeURL = a} :: UpdateApplication)
{-# DEPRECATED uaReadmeURL "Use generic-lens or generic-optics with 'readmeURL' instead." #-}

-- | The Amazon Resource Name (ARN) of the application.
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaApplicationId :: Lens.Lens' UpdateApplication Lude.Text
uaApplicationId = Lens.lens (applicationId :: UpdateApplication -> Lude.Text) (\s a -> s {applicationId = a} :: UpdateApplication)
{-# DEPRECATED uaApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- /Note:/ Consider using 'author' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaAuthor :: Lens.Lens' UpdateApplication (Lude.Maybe Lude.Text)
uaAuthor = Lens.lens (author :: UpdateApplication -> Lude.Maybe Lude.Text) (\s a -> s {author = a} :: UpdateApplication)
{-# DEPRECATED uaAuthor "Use generic-lens or generic-optics with 'author' instead." #-}

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaLabels :: Lens.Lens' UpdateApplication (Lude.Maybe [Lude.Text])
uaLabels = Lens.lens (labels :: UpdateApplication -> Lude.Maybe [Lude.Text]) (\s a -> s {labels = a} :: UpdateApplication)
{-# DEPRECATED uaLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uaDescription :: Lens.Lens' UpdateApplication (Lude.Maybe Lude.Text)
uaDescription = Lens.lens (description :: UpdateApplication -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateApplication)
{-# DEPRECATED uaDescription "Use generic-lens or generic-optics with 'description' instead." #-}

instance Lude.AWSRequest UpdateApplication where
  type Rs UpdateApplication = UpdateApplicationResponse
  request = Req.patchJSON serverlessApplicationRepositoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateApplicationResponse'
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

instance Lude.ToHeaders UpdateApplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateApplication where
  toJSON UpdateApplication' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("homePageUrl" Lude..=) Lude.<$> homePageURL,
            ("readmeBody" Lude..=) Lude.<$> readmeBody,
            ("readmeUrl" Lude..=) Lude.<$> readmeURL,
            ("author" Lude..=) Lude.<$> author,
            ("labels" Lude..=) Lude.<$> labels,
            ("description" Lude..=) Lude.<$> description
          ]
      )

instance Lude.ToPath UpdateApplication where
  toPath UpdateApplication' {..} =
    Lude.mconcat ["/applications/", Lude.toBS applicationId]

instance Lude.ToQuery UpdateApplication where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse'
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

-- | Creates a value of 'UpdateApplicationResponse' with the minimum fields required to make a request.
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
mkUpdateApplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateApplicationResponse
mkUpdateApplicationResponse pResponseStatus_ =
  UpdateApplicationResponse'
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
uarsCreationTime :: Lens.Lens' UpdateApplicationResponse (Lude.Maybe Lude.Text)
uarsCreationTime = Lens.lens (creationTime :: UpdateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTime = a} :: UpdateApplicationResponse)
{-# DEPRECATED uarsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A URL with more information about the application, for example the location of your GitHub repository for the application.
--
-- /Note:/ Consider using 'homePageURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsHomePageURL :: Lens.Lens' UpdateApplicationResponse (Lude.Maybe Lude.Text)
uarsHomePageURL = Lens.lens (homePageURL :: UpdateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {homePageURL = a} :: UpdateApplicationResponse)
{-# DEPRECATED uarsHomePageURL "Use generic-lens or generic-optics with 'homePageURL' instead." #-}

-- | A link to a license file of the app that matches the spdxLicenseID value of your application.
--
-- Maximum size 5 MB
--
-- /Note:/ Consider using 'licenseURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsLicenseURL :: Lens.Lens' UpdateApplicationResponse (Lude.Maybe Lude.Text)
uarsLicenseURL = Lens.lens (licenseURL :: UpdateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {licenseURL = a} :: UpdateApplicationResponse)
{-# DEPRECATED uarsLicenseURL "Use generic-lens or generic-optics with 'licenseURL' instead." #-}

-- | A link to the readme file in Markdown language that contains a more detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- /Note:/ Consider using 'readmeURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsReadmeURL :: Lens.Lens' UpdateApplicationResponse (Lude.Maybe Lude.Text)
uarsReadmeURL = Lens.lens (readmeURL :: UpdateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {readmeURL = a} :: UpdateApplicationResponse)
{-# DEPRECATED uarsReadmeURL "Use generic-lens or generic-optics with 'readmeURL' instead." #-}

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsApplicationId :: Lens.Lens' UpdateApplicationResponse (Lude.Maybe Lude.Text)
uarsApplicationId = Lens.lens (applicationId :: UpdateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: UpdateApplicationResponse)
{-# DEPRECATED uarsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The name of the application.
--
-- Minimum length=1. Maximum length=140
-- Pattern: "[a-zA-Z0-9\\-]+";
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsName :: Lens.Lens' UpdateApplicationResponse (Lude.Maybe Lude.Text)
uarsName = Lens.lens (name :: UpdateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: UpdateApplicationResponse)
{-# DEPRECATED uarsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Version information about the application.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsVersion :: Lens.Lens' UpdateApplicationResponse (Lude.Maybe Version)
uarsVersion = Lens.lens (version :: UpdateApplicationResponse -> Lude.Maybe Version) (\s a -> s {version = a} :: UpdateApplicationResponse)
{-# DEPRECATED uarsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- /Note:/ Consider using 'author' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsAuthor :: Lens.Lens' UpdateApplicationResponse (Lude.Maybe Lude.Text)
uarsAuthor = Lens.lens (author :: UpdateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {author = a} :: UpdateApplicationResponse)
{-# DEPRECATED uarsAuthor "Use generic-lens or generic-optics with 'author' instead." #-}

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsLabels :: Lens.Lens' UpdateApplicationResponse (Lude.Maybe [Lude.Text])
uarsLabels = Lens.lens (labels :: UpdateApplicationResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {labels = a} :: UpdateApplicationResponse)
{-# DEPRECATED uarsLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | The URL to the public profile of a verified author. This URL is submitted by the author.
--
-- /Note:/ Consider using 'verifiedAuthorURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsVerifiedAuthorURL :: Lens.Lens' UpdateApplicationResponse (Lude.Maybe Lude.Text)
uarsVerifiedAuthorURL = Lens.lens (verifiedAuthorURL :: UpdateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {verifiedAuthorURL = a} :: UpdateApplicationResponse)
{-# DEPRECATED uarsVerifiedAuthorURL "Use generic-lens or generic-optics with 'verifiedAuthorURL' instead." #-}

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsDescription :: Lens.Lens' UpdateApplicationResponse (Lude.Maybe Lude.Text)
uarsDescription = Lens.lens (description :: UpdateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateApplicationResponse)
{-# DEPRECATED uarsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A valid identifier from https://spdx.org/licenses/.
--
-- /Note:/ Consider using 'spdxLicenseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsSpdxLicenseId :: Lens.Lens' UpdateApplicationResponse (Lude.Maybe Lude.Text)
uarsSpdxLicenseId = Lens.lens (spdxLicenseId :: UpdateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {spdxLicenseId = a} :: UpdateApplicationResponse)
{-# DEPRECATED uarsSpdxLicenseId "Use generic-lens or generic-optics with 'spdxLicenseId' instead." #-}

-- | Whether the author of this application has been verified. This means means that AWS has made a good faith review, as a reasonable and prudent service provider, of the information provided by the requester and has confirmed that the requester's identity is as claimed.
--
-- /Note:/ Consider using 'isVerifiedAuthor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsIsVerifiedAuthor :: Lens.Lens' UpdateApplicationResponse (Lude.Maybe Lude.Bool)
uarsIsVerifiedAuthor = Lens.lens (isVerifiedAuthor :: UpdateApplicationResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isVerifiedAuthor = a} :: UpdateApplicationResponse)
{-# DEPRECATED uarsIsVerifiedAuthor "Use generic-lens or generic-optics with 'isVerifiedAuthor' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uarsResponseStatus :: Lens.Lens' UpdateApplicationResponse Lude.Int
uarsResponseStatus = Lens.lens (responseStatus :: UpdateApplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateApplicationResponse)
{-# DEPRECATED uarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
