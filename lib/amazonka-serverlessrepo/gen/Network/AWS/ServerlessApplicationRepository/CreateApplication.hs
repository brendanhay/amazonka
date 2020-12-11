{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    CreateApplication (..),
    mkCreateApplication,

    -- ** Request lenses
    caHomePageURL,
    caReadmeBody,
    caLicenseURL,
    caSemanticVersion,
    caSourceCodeURL,
    caReadmeURL,
    caLabels,
    caTemplateBody,
    caTemplateURL,
    caLicenseBody,
    caSpdxLicenseId,
    caSourceCodeArchiveURL,
    caDescription,
    caName,
    caAuthor,

    -- * Destructuring the response
    CreateApplicationResponse (..),
    mkCreateApplicationResponse,

    -- ** Response lenses
    carsCreationTime,
    carsHomePageURL,
    carsLicenseURL,
    carsReadmeURL,
    carsApplicationId,
    carsName,
    carsVersion,
    carsAuthor,
    carsLabels,
    carsVerifiedAuthorURL,
    carsDescription,
    carsSpdxLicenseId,
    carsIsVerifiedAuthor,
    carsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'mkCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { homePageURL ::
      Lude.Maybe Lude.Text,
    readmeBody :: Lude.Maybe Lude.Text,
    licenseURL :: Lude.Maybe Lude.Text,
    semanticVersion :: Lude.Maybe Lude.Text,
    sourceCodeURL :: Lude.Maybe Lude.Text,
    readmeURL :: Lude.Maybe Lude.Text,
    labels :: Lude.Maybe [Lude.Text],
    templateBody :: Lude.Maybe Lude.Text,
    templateURL :: Lude.Maybe Lude.Text,
    licenseBody :: Lude.Maybe Lude.Text,
    spdxLicenseId :: Lude.Maybe Lude.Text,
    sourceCodeArchiveURL :: Lude.Maybe Lude.Text,
    description :: Lude.Text,
    name :: Lude.Text,
    author :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateApplication' with the minimum fields required to make a request.
--
-- * 'author' - The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
-- * 'description' - The description of the application.
--
-- Minimum length=1. Maximum length=256
-- * 'homePageURL' - A URL with more information about the application, for example the location of your GitHub repository for the application.
-- * 'labels' - Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
-- * 'licenseBody' - A local text file that contains the license of the app that matches the spdxLicenseID value of your application.
--
--  The file has the format file://<path>/<filename>.
-- Maximum size 5 MB
-- You can specify only one of licenseBody and licenseUrl; otherwise, an error results.
-- * 'licenseURL' - A link to the S3 object that contains the license of the app that matches the spdxLicenseID value of your application.
--
-- Maximum size 5 MB
-- You can specify only one of licenseBody and licenseUrl; otherwise, an error results.
-- * 'name' - The name of the application that you want to publish.
--
-- Minimum length=1. Maximum length=140
-- Pattern: "[a-zA-Z0-9\\-]+";
-- * 'readmeBody' - A local text readme file in Markdown language that contains a more detailed description of the application and how it works.
--
--  The file has the format file://<path>/<filename>.
-- Maximum size 5 MB
-- You can specify only one of readmeBody and readmeUrl; otherwise, an error results.
-- * 'readmeURL' - A link to the S3 object in Markdown language that contains a more detailed description of the application and how it works.
--
-- Maximum size 5 MB
-- You can specify only one of readmeBody and readmeUrl; otherwise, an error results.
-- * 'semanticVersion' - The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
-- * 'sourceCodeArchiveURL' - A link to the S3 object that contains the ZIP archive of the source code for this version of your application.
--
-- Maximum size 50 MB
-- * 'sourceCodeURL' - A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
-- * 'spdxLicenseId' - A valid identifier from <https://spdx.org/licenses/ https://spdx.org/licenses/> .
-- * 'templateBody' - The local raw packaged AWS SAM template file of your application.
--
--  The file has the format file://<path>/<filename>.
-- You can specify only one of templateBody and templateUrl; otherwise an error results.
-- * 'templateURL' - A link to the S3 object containing the packaged AWS SAM template of your application.
--
-- You can specify only one of templateBody and templateUrl; otherwise an error results.
mkCreateApplication ::
  -- | 'description'
  Lude.Text ->
  -- | 'name'
  Lude.Text ->
  -- | 'author'
  Lude.Text ->
  CreateApplication
mkCreateApplication pDescription_ pName_ pAuthor_ =
  CreateApplication'
    { homePageURL = Lude.Nothing,
      readmeBody = Lude.Nothing,
      licenseURL = Lude.Nothing,
      semanticVersion = Lude.Nothing,
      sourceCodeURL = Lude.Nothing,
      readmeURL = Lude.Nothing,
      labels = Lude.Nothing,
      templateBody = Lude.Nothing,
      templateURL = Lude.Nothing,
      licenseBody = Lude.Nothing,
      spdxLicenseId = Lude.Nothing,
      sourceCodeArchiveURL = Lude.Nothing,
      description = pDescription_,
      name = pName_,
      author = pAuthor_
    }

-- | A URL with more information about the application, for example the location of your GitHub repository for the application.
--
-- /Note:/ Consider using 'homePageURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caHomePageURL :: Lens.Lens' CreateApplication (Lude.Maybe Lude.Text)
caHomePageURL = Lens.lens (homePageURL :: CreateApplication -> Lude.Maybe Lude.Text) (\s a -> s {homePageURL = a} :: CreateApplication)
{-# DEPRECATED caHomePageURL "Use generic-lens or generic-optics with 'homePageURL' instead." #-}

-- | A local text readme file in Markdown language that contains a more detailed description of the application and how it works.
--
--  The file has the format file://<path>/<filename>.
-- Maximum size 5 MB
-- You can specify only one of readmeBody and readmeUrl; otherwise, an error results.
--
-- /Note:/ Consider using 'readmeBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caReadmeBody :: Lens.Lens' CreateApplication (Lude.Maybe Lude.Text)
caReadmeBody = Lens.lens (readmeBody :: CreateApplication -> Lude.Maybe Lude.Text) (\s a -> s {readmeBody = a} :: CreateApplication)
{-# DEPRECATED caReadmeBody "Use generic-lens or generic-optics with 'readmeBody' instead." #-}

-- | A link to the S3 object that contains the license of the app that matches the spdxLicenseID value of your application.
--
-- Maximum size 5 MB
-- You can specify only one of licenseBody and licenseUrl; otherwise, an error results.
--
-- /Note:/ Consider using 'licenseURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caLicenseURL :: Lens.Lens' CreateApplication (Lude.Maybe Lude.Text)
caLicenseURL = Lens.lens (licenseURL :: CreateApplication -> Lude.Maybe Lude.Text) (\s a -> s {licenseURL = a} :: CreateApplication)
{-# DEPRECATED caLicenseURL "Use generic-lens or generic-optics with 'licenseURL' instead." #-}

-- | The semantic version of the application:
--
-- <https://semver.org/ https://semver.org/>
--
-- /Note:/ Consider using 'semanticVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caSemanticVersion :: Lens.Lens' CreateApplication (Lude.Maybe Lude.Text)
caSemanticVersion = Lens.lens (semanticVersion :: CreateApplication -> Lude.Maybe Lude.Text) (\s a -> s {semanticVersion = a} :: CreateApplication)
{-# DEPRECATED caSemanticVersion "Use generic-lens or generic-optics with 'semanticVersion' instead." #-}

-- | A link to a public repository for the source code of your application, for example the URL of a specific GitHub commit.
--
-- /Note:/ Consider using 'sourceCodeURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caSourceCodeURL :: Lens.Lens' CreateApplication (Lude.Maybe Lude.Text)
caSourceCodeURL = Lens.lens (sourceCodeURL :: CreateApplication -> Lude.Maybe Lude.Text) (\s a -> s {sourceCodeURL = a} :: CreateApplication)
{-# DEPRECATED caSourceCodeURL "Use generic-lens or generic-optics with 'sourceCodeURL' instead." #-}

-- | A link to the S3 object in Markdown language that contains a more detailed description of the application and how it works.
--
-- Maximum size 5 MB
-- You can specify only one of readmeBody and readmeUrl; otherwise, an error results.
--
-- /Note:/ Consider using 'readmeURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caReadmeURL :: Lens.Lens' CreateApplication (Lude.Maybe Lude.Text)
caReadmeURL = Lens.lens (readmeURL :: CreateApplication -> Lude.Maybe Lude.Text) (\s a -> s {readmeURL = a} :: CreateApplication)
{-# DEPRECATED caReadmeURL "Use generic-lens or generic-optics with 'readmeURL' instead." #-}

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caLabels :: Lens.Lens' CreateApplication (Lude.Maybe [Lude.Text])
caLabels = Lens.lens (labels :: CreateApplication -> Lude.Maybe [Lude.Text]) (\s a -> s {labels = a} :: CreateApplication)
{-# DEPRECATED caLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | The local raw packaged AWS SAM template file of your application.
--
--  The file has the format file://<path>/<filename>.
-- You can specify only one of templateBody and templateUrl; otherwise an error results.
--
-- /Note:/ Consider using 'templateBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTemplateBody :: Lens.Lens' CreateApplication (Lude.Maybe Lude.Text)
caTemplateBody = Lens.lens (templateBody :: CreateApplication -> Lude.Maybe Lude.Text) (\s a -> s {templateBody = a} :: CreateApplication)
{-# DEPRECATED caTemplateBody "Use generic-lens or generic-optics with 'templateBody' instead." #-}

-- | A link to the S3 object containing the packaged AWS SAM template of your application.
--
-- You can specify only one of templateBody and templateUrl; otherwise an error results.
--
-- /Note:/ Consider using 'templateURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caTemplateURL :: Lens.Lens' CreateApplication (Lude.Maybe Lude.Text)
caTemplateURL = Lens.lens (templateURL :: CreateApplication -> Lude.Maybe Lude.Text) (\s a -> s {templateURL = a} :: CreateApplication)
{-# DEPRECATED caTemplateURL "Use generic-lens or generic-optics with 'templateURL' instead." #-}

-- | A local text file that contains the license of the app that matches the spdxLicenseID value of your application.
--
--  The file has the format file://<path>/<filename>.
-- Maximum size 5 MB
-- You can specify only one of licenseBody and licenseUrl; otherwise, an error results.
--
-- /Note:/ Consider using 'licenseBody' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caLicenseBody :: Lens.Lens' CreateApplication (Lude.Maybe Lude.Text)
caLicenseBody = Lens.lens (licenseBody :: CreateApplication -> Lude.Maybe Lude.Text) (\s a -> s {licenseBody = a} :: CreateApplication)
{-# DEPRECATED caLicenseBody "Use generic-lens or generic-optics with 'licenseBody' instead." #-}

-- | A valid identifier from <https://spdx.org/licenses/ https://spdx.org/licenses/> .
--
-- /Note:/ Consider using 'spdxLicenseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caSpdxLicenseId :: Lens.Lens' CreateApplication (Lude.Maybe Lude.Text)
caSpdxLicenseId = Lens.lens (spdxLicenseId :: CreateApplication -> Lude.Maybe Lude.Text) (\s a -> s {spdxLicenseId = a} :: CreateApplication)
{-# DEPRECATED caSpdxLicenseId "Use generic-lens or generic-optics with 'spdxLicenseId' instead." #-}

-- | A link to the S3 object that contains the ZIP archive of the source code for this version of your application.
--
-- Maximum size 50 MB
--
-- /Note:/ Consider using 'sourceCodeArchiveURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caSourceCodeArchiveURL :: Lens.Lens' CreateApplication (Lude.Maybe Lude.Text)
caSourceCodeArchiveURL = Lens.lens (sourceCodeArchiveURL :: CreateApplication -> Lude.Maybe Lude.Text) (\s a -> s {sourceCodeArchiveURL = a} :: CreateApplication)
{-# DEPRECATED caSourceCodeArchiveURL "Use generic-lens or generic-optics with 'sourceCodeArchiveURL' instead." #-}

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caDescription :: Lens.Lens' CreateApplication Lude.Text
caDescription = Lens.lens (description :: CreateApplication -> Lude.Text) (\s a -> s {description = a} :: CreateApplication)
{-# DEPRECATED caDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The name of the application that you want to publish.
--
-- Minimum length=1. Maximum length=140
-- Pattern: "[a-zA-Z0-9\\-]+";
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caName :: Lens.Lens' CreateApplication Lude.Text
caName = Lens.lens (name :: CreateApplication -> Lude.Text) (\s a -> s {name = a} :: CreateApplication)
{-# DEPRECATED caName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- /Note:/ Consider using 'author' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
caAuthor :: Lens.Lens' CreateApplication Lude.Text
caAuthor = Lens.lens (author :: CreateApplication -> Lude.Text) (\s a -> s {author = a} :: CreateApplication)
{-# DEPRECATED caAuthor "Use generic-lens or generic-optics with 'author' instead." #-}

instance Lude.AWSRequest CreateApplication where
  type Rs CreateApplication = CreateApplicationResponse
  request = Req.postJSON serverlessApplicationRepositoryService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateApplicationResponse'
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

instance Lude.ToHeaders CreateApplication where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateApplication where
  toJSON CreateApplication' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("homePageUrl" Lude..=) Lude.<$> homePageURL,
            ("readmeBody" Lude..=) Lude.<$> readmeBody,
            ("licenseUrl" Lude..=) Lude.<$> licenseURL,
            ("semanticVersion" Lude..=) Lude.<$> semanticVersion,
            ("sourceCodeUrl" Lude..=) Lude.<$> sourceCodeURL,
            ("readmeUrl" Lude..=) Lude.<$> readmeURL,
            ("labels" Lude..=) Lude.<$> labels,
            ("templateBody" Lude..=) Lude.<$> templateBody,
            ("templateUrl" Lude..=) Lude.<$> templateURL,
            ("licenseBody" Lude..=) Lude.<$> licenseBody,
            ("spdxLicenseId" Lude..=) Lude.<$> spdxLicenseId,
            ("sourceCodeArchiveUrl" Lude..=) Lude.<$> sourceCodeArchiveURL,
            Lude.Just ("description" Lude..= description),
            Lude.Just ("name" Lude..= name),
            Lude.Just ("author" Lude..= author)
          ]
      )

instance Lude.ToPath CreateApplication where
  toPath = Lude.const "/applications"

instance Lude.ToQuery CreateApplication where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { creationTime ::
      Lude.Maybe Lude.Text,
    homePageURL :: Lude.Maybe Lude.Text,
    licenseURL :: Lude.Maybe Lude.Text,
    readmeURL :: Lude.Maybe Lude.Text,
    applicationId :: Lude.Maybe Lude.Text,
    name :: Lude.Maybe Lude.Text,
    version :: Lude.Maybe Version,
    author :: Lude.Maybe Lude.Text,
    labels :: Lude.Maybe [Lude.Text],
    verifiedAuthorURL ::
      Lude.Maybe Lude.Text,
    description :: Lude.Maybe Lude.Text,
    spdxLicenseId :: Lude.Maybe Lude.Text,
    isVerifiedAuthor ::
      Lude.Maybe Lude.Bool,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateApplicationResponse' with the minimum fields required to make a request.
--
-- * 'applicationId' - The application Amazon Resource Name (ARN).
-- * 'author' - The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
-- * 'creationTime' - The date and time this resource was created.
-- * 'description' - The description of the application.
--
-- Minimum length=1. Maximum length=256
-- * 'homePageURL' - A URL with more information about the application, for example the location of your GitHub repository for the application.
-- * 'isVerifiedAuthor' - Whether the author of this application has been verified. This means means that AWS has made a good faith review, as a reasonable and prudent service provider, of the information provided by the requester and has confirmed that the requester's identity is as claimed.
-- * 'labels' - Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
-- * 'licenseURL' - A link to a license file of the app that matches the spdxLicenseID value of your application.
--
-- Maximum size 5 MB
-- * 'name' - The name of the application.
--
-- Minimum length=1. Maximum length=140
-- Pattern: "[a-zA-Z0-9\\-]+";
-- * 'readmeURL' - A link to the readme file in Markdown language that contains a more detailed description of the application and how it works.
--
-- Maximum size 5 MB
-- * 'responseStatus' - The response status code.
-- * 'spdxLicenseId' - A valid identifier from https://spdx.org/licenses/.
-- * 'verifiedAuthorURL' - The URL to the public profile of a verified author. This URL is submitted by the author.
-- * 'version' - Version information about the application.
mkCreateApplicationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateApplicationResponse
mkCreateApplicationResponse pResponseStatus_ =
  CreateApplicationResponse'
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
carsCreationTime :: Lens.Lens' CreateApplicationResponse (Lude.Maybe Lude.Text)
carsCreationTime = Lens.lens (creationTime :: CreateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {creationTime = a} :: CreateApplicationResponse)
{-# DEPRECATED carsCreationTime "Use generic-lens or generic-optics with 'creationTime' instead." #-}

-- | A URL with more information about the application, for example the location of your GitHub repository for the application.
--
-- /Note:/ Consider using 'homePageURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsHomePageURL :: Lens.Lens' CreateApplicationResponse (Lude.Maybe Lude.Text)
carsHomePageURL = Lens.lens (homePageURL :: CreateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {homePageURL = a} :: CreateApplicationResponse)
{-# DEPRECATED carsHomePageURL "Use generic-lens or generic-optics with 'homePageURL' instead." #-}

-- | A link to a license file of the app that matches the spdxLicenseID value of your application.
--
-- Maximum size 5 MB
--
-- /Note:/ Consider using 'licenseURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsLicenseURL :: Lens.Lens' CreateApplicationResponse (Lude.Maybe Lude.Text)
carsLicenseURL = Lens.lens (licenseURL :: CreateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {licenseURL = a} :: CreateApplicationResponse)
{-# DEPRECATED carsLicenseURL "Use generic-lens or generic-optics with 'licenseURL' instead." #-}

-- | A link to the readme file in Markdown language that contains a more detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- /Note:/ Consider using 'readmeURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsReadmeURL :: Lens.Lens' CreateApplicationResponse (Lude.Maybe Lude.Text)
carsReadmeURL = Lens.lens (readmeURL :: CreateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {readmeURL = a} :: CreateApplicationResponse)
{-# DEPRECATED carsReadmeURL "Use generic-lens or generic-optics with 'readmeURL' instead." #-}

-- | The application Amazon Resource Name (ARN).
--
-- /Note:/ Consider using 'applicationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsApplicationId :: Lens.Lens' CreateApplicationResponse (Lude.Maybe Lude.Text)
carsApplicationId = Lens.lens (applicationId :: CreateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {applicationId = a} :: CreateApplicationResponse)
{-# DEPRECATED carsApplicationId "Use generic-lens or generic-optics with 'applicationId' instead." #-}

-- | The name of the application.
--
-- Minimum length=1. Maximum length=140
-- Pattern: "[a-zA-Z0-9\\-]+";
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsName :: Lens.Lens' CreateApplicationResponse (Lude.Maybe Lude.Text)
carsName = Lens.lens (name :: CreateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: CreateApplicationResponse)
{-# DEPRECATED carsName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | Version information about the application.
--
-- /Note:/ Consider using 'version' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsVersion :: Lens.Lens' CreateApplicationResponse (Lude.Maybe Version)
carsVersion = Lens.lens (version :: CreateApplicationResponse -> Lude.Maybe Version) (\s a -> s {version = a} :: CreateApplicationResponse)
{-# DEPRECATED carsVersion "Use generic-lens or generic-optics with 'version' instead." #-}

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
-- Pattern "^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$";
--
-- /Note:/ Consider using 'author' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsAuthor :: Lens.Lens' CreateApplicationResponse (Lude.Maybe Lude.Text)
carsAuthor = Lens.lens (author :: CreateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {author = a} :: CreateApplicationResponse)
{-# DEPRECATED carsAuthor "Use generic-lens or generic-optics with 'author' instead." #-}

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
-- Pattern: "^[a-zA-Z0-9+\\-_:\\/@]+$";
--
-- /Note:/ Consider using 'labels' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsLabels :: Lens.Lens' CreateApplicationResponse (Lude.Maybe [Lude.Text])
carsLabels = Lens.lens (labels :: CreateApplicationResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {labels = a} :: CreateApplicationResponse)
{-# DEPRECATED carsLabels "Use generic-lens or generic-optics with 'labels' instead." #-}

-- | The URL to the public profile of a verified author. This URL is submitted by the author.
--
-- /Note:/ Consider using 'verifiedAuthorURL' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsVerifiedAuthorURL :: Lens.Lens' CreateApplicationResponse (Lude.Maybe Lude.Text)
carsVerifiedAuthorURL = Lens.lens (verifiedAuthorURL :: CreateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {verifiedAuthorURL = a} :: CreateApplicationResponse)
{-# DEPRECATED carsVerifiedAuthorURL "Use generic-lens or generic-optics with 'verifiedAuthorURL' instead." #-}

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsDescription :: Lens.Lens' CreateApplicationResponse (Lude.Maybe Lude.Text)
carsDescription = Lens.lens (description :: CreateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: CreateApplicationResponse)
{-# DEPRECATED carsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A valid identifier from https://spdx.org/licenses/.
--
-- /Note:/ Consider using 'spdxLicenseId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsSpdxLicenseId :: Lens.Lens' CreateApplicationResponse (Lude.Maybe Lude.Text)
carsSpdxLicenseId = Lens.lens (spdxLicenseId :: CreateApplicationResponse -> Lude.Maybe Lude.Text) (\s a -> s {spdxLicenseId = a} :: CreateApplicationResponse)
{-# DEPRECATED carsSpdxLicenseId "Use generic-lens or generic-optics with 'spdxLicenseId' instead." #-}

-- | Whether the author of this application has been verified. This means means that AWS has made a good faith review, as a reasonable and prudent service provider, of the information provided by the requester and has confirmed that the requester's identity is as claimed.
--
-- /Note:/ Consider using 'isVerifiedAuthor' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsIsVerifiedAuthor :: Lens.Lens' CreateApplicationResponse (Lude.Maybe Lude.Bool)
carsIsVerifiedAuthor = Lens.lens (isVerifiedAuthor :: CreateApplicationResponse -> Lude.Maybe Lude.Bool) (\s a -> s {isVerifiedAuthor = a} :: CreateApplicationResponse)
{-# DEPRECATED carsIsVerifiedAuthor "Use generic-lens or generic-optics with 'isVerifiedAuthor' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
carsResponseStatus :: Lens.Lens' CreateApplicationResponse Lude.Int
carsResponseStatus = Lens.lens (responseStatus :: CreateApplicationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateApplicationResponse)
{-# DEPRECATED carsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
