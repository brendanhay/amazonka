{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ServerlessApplicationRepository.CreateApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application, optionally including an AWS SAM file to create
-- the first application version in the same call.
module Network.AWS.ServerlessApplicationRepository.CreateApplication
  ( -- * Creating a Request
    CreateApplication (..),
    newCreateApplication,

    -- * Request Lenses
    createApplication_spdxLicenseId,
    createApplication_templateUrl,
    createApplication_licenseBody,
    createApplication_licenseUrl,
    createApplication_labels,
    createApplication_readmeBody,
    createApplication_homePageUrl,
    createApplication_sourceCodeArchiveUrl,
    createApplication_readmeUrl,
    createApplication_sourceCodeUrl,
    createApplication_semanticVersion,
    createApplication_templateBody,
    createApplication_description,
    createApplication_name,
    createApplication_author,

    -- * Destructuring the Response
    CreateApplicationResponse (..),
    newCreateApplicationResponse,

    -- * Response Lenses
    createApplicationResponse_applicationId,
    createApplicationResponse_creationTime,
    createApplicationResponse_spdxLicenseId,
    createApplicationResponse_licenseUrl,
    createApplicationResponse_verifiedAuthorUrl,
    createApplicationResponse_labels,
    createApplicationResponse_author,
    createApplicationResponse_version,
    createApplicationResponse_homePageUrl,
    createApplicationResponse_name,
    createApplicationResponse_isVerifiedAuthor,
    createApplicationResponse_readmeUrl,
    createApplicationResponse_description,
    createApplicationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'newCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | A valid identifier from <https://spdx.org/licenses/>.
    spdxLicenseId :: Core.Maybe Core.Text,
    -- | A link to the S3 object containing the packaged AWS SAM template of your
    -- application.
    --
    -- You can specify only one of templateBody and templateUrl; otherwise an
    -- error results.
    templateUrl :: Core.Maybe Core.Text,
    -- | A local text file that contains the license of the app that matches the
    -- spdxLicenseID value of your application. The file has the format
    -- file:\/\/\<path>\/\<filename>.
    --
    -- Maximum size 5 MB
    --
    -- You can specify only one of licenseBody and licenseUrl; otherwise, an
    -- error results.
    licenseBody :: Core.Maybe Core.Text,
    -- | A link to the S3 object that contains the license of the app that
    -- matches the spdxLicenseID value of your application.
    --
    -- Maximum size 5 MB
    --
    -- You can specify only one of licenseBody and licenseUrl; otherwise, an
    -- error results.
    licenseUrl :: Core.Maybe Core.Text,
    -- | Labels to improve discovery of apps in search results.
    --
    -- Minimum length=1. Maximum length=127. Maximum number of labels: 10
    --
    -- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
    labels :: Core.Maybe [Core.Text],
    -- | A local text readme file in Markdown language that contains a more
    -- detailed description of the application and how it works. The file has
    -- the format file:\/\/\<path>\/\<filename>.
    --
    -- Maximum size 5 MB
    --
    -- You can specify only one of readmeBody and readmeUrl; otherwise, an
    -- error results.
    readmeBody :: Core.Maybe Core.Text,
    -- | A URL with more information about the application, for example the
    -- location of your GitHub repository for the application.
    homePageUrl :: Core.Maybe Core.Text,
    -- | A link to the S3 object that contains the ZIP archive of the source code
    -- for this version of your application.
    --
    -- Maximum size 50 MB
    sourceCodeArchiveUrl :: Core.Maybe Core.Text,
    -- | A link to the S3 object in Markdown language that contains a more
    -- detailed description of the application and how it works.
    --
    -- Maximum size 5 MB
    --
    -- You can specify only one of readmeBody and readmeUrl; otherwise, an
    -- error results.
    readmeUrl :: Core.Maybe Core.Text,
    -- | A link to a public repository for the source code of your application,
    -- for example the URL of a specific GitHub commit.
    sourceCodeUrl :: Core.Maybe Core.Text,
    -- | The semantic version of the application:
    --
    -- <https://semver.org/>
    semanticVersion :: Core.Maybe Core.Text,
    -- | The local raw packaged AWS SAM template file of your application. The
    -- file has the format file:\/\/\<path>\/\<filename>.
    --
    -- You can specify only one of templateBody and templateUrl; otherwise an
    -- error results.
    templateBody :: Core.Maybe Core.Text,
    -- | The description of the application.
    --
    -- Minimum length=1. Maximum length=256
    description :: Core.Text,
    -- | The name of the application that you want to publish.
    --
    -- Minimum length=1. Maximum length=140
    --
    -- Pattern: \"[a-zA-Z0-9\\\\-]+\";
    name :: Core.Text,
    -- | The name of the author publishing the app.
    --
    -- Minimum length=1. Maximum length=127.
    --
    -- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
    author :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'spdxLicenseId', 'createApplication_spdxLicenseId' - A valid identifier from <https://spdx.org/licenses/>.
--
-- 'templateUrl', 'createApplication_templateUrl' - A link to the S3 object containing the packaged AWS SAM template of your
-- application.
--
-- You can specify only one of templateBody and templateUrl; otherwise an
-- error results.
--
-- 'licenseBody', 'createApplication_licenseBody' - A local text file that contains the license of the app that matches the
-- spdxLicenseID value of your application. The file has the format
-- file:\/\/\<path>\/\<filename>.
--
-- Maximum size 5 MB
--
-- You can specify only one of licenseBody and licenseUrl; otherwise, an
-- error results.
--
-- 'licenseUrl', 'createApplication_licenseUrl' - A link to the S3 object that contains the license of the app that
-- matches the spdxLicenseID value of your application.
--
-- Maximum size 5 MB
--
-- You can specify only one of licenseBody and licenseUrl; otherwise, an
-- error results.
--
-- 'labels', 'createApplication_labels' - Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
--
-- 'readmeBody', 'createApplication_readmeBody' - A local text readme file in Markdown language that contains a more
-- detailed description of the application and how it works. The file has
-- the format file:\/\/\<path>\/\<filename>.
--
-- Maximum size 5 MB
--
-- You can specify only one of readmeBody and readmeUrl; otherwise, an
-- error results.
--
-- 'homePageUrl', 'createApplication_homePageUrl' - A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
--
-- 'sourceCodeArchiveUrl', 'createApplication_sourceCodeArchiveUrl' - A link to the S3 object that contains the ZIP archive of the source code
-- for this version of your application.
--
-- Maximum size 50 MB
--
-- 'readmeUrl', 'createApplication_readmeUrl' - A link to the S3 object in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- You can specify only one of readmeBody and readmeUrl; otherwise, an
-- error results.
--
-- 'sourceCodeUrl', 'createApplication_sourceCodeUrl' - A link to a public repository for the source code of your application,
-- for example the URL of a specific GitHub commit.
--
-- 'semanticVersion', 'createApplication_semanticVersion' - The semantic version of the application:
--
-- <https://semver.org/>
--
-- 'templateBody', 'createApplication_templateBody' - The local raw packaged AWS SAM template file of your application. The
-- file has the format file:\/\/\<path>\/\<filename>.
--
-- You can specify only one of templateBody and templateUrl; otherwise an
-- error results.
--
-- 'description', 'createApplication_description' - The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- 'name', 'createApplication_name' - The name of the application that you want to publish.
--
-- Minimum length=1. Maximum length=140
--
-- Pattern: \"[a-zA-Z0-9\\\\-]+\";
--
-- 'author', 'createApplication_author' - The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
newCreateApplication ::
  -- | 'description'
  Core.Text ->
  -- | 'name'
  Core.Text ->
  -- | 'author'
  Core.Text ->
  CreateApplication
newCreateApplication pDescription_ pName_ pAuthor_ =
  CreateApplication'
    { spdxLicenseId = Core.Nothing,
      templateUrl = Core.Nothing,
      licenseBody = Core.Nothing,
      licenseUrl = Core.Nothing,
      labels = Core.Nothing,
      readmeBody = Core.Nothing,
      homePageUrl = Core.Nothing,
      sourceCodeArchiveUrl = Core.Nothing,
      readmeUrl = Core.Nothing,
      sourceCodeUrl = Core.Nothing,
      semanticVersion = Core.Nothing,
      templateBody = Core.Nothing,
      description = pDescription_,
      name = pName_,
      author = pAuthor_
    }

-- | A valid identifier from <https://spdx.org/licenses/>.
createApplication_spdxLicenseId :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
createApplication_spdxLicenseId = Lens.lens (\CreateApplication' {spdxLicenseId} -> spdxLicenseId) (\s@CreateApplication' {} a -> s {spdxLicenseId = a} :: CreateApplication)

-- | A link to the S3 object containing the packaged AWS SAM template of your
-- application.
--
-- You can specify only one of templateBody and templateUrl; otherwise an
-- error results.
createApplication_templateUrl :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
createApplication_templateUrl = Lens.lens (\CreateApplication' {templateUrl} -> templateUrl) (\s@CreateApplication' {} a -> s {templateUrl = a} :: CreateApplication)

-- | A local text file that contains the license of the app that matches the
-- spdxLicenseID value of your application. The file has the format
-- file:\/\/\<path>\/\<filename>.
--
-- Maximum size 5 MB
--
-- You can specify only one of licenseBody and licenseUrl; otherwise, an
-- error results.
createApplication_licenseBody :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
createApplication_licenseBody = Lens.lens (\CreateApplication' {licenseBody} -> licenseBody) (\s@CreateApplication' {} a -> s {licenseBody = a} :: CreateApplication)

-- | A link to the S3 object that contains the license of the app that
-- matches the spdxLicenseID value of your application.
--
-- Maximum size 5 MB
--
-- You can specify only one of licenseBody and licenseUrl; otherwise, an
-- error results.
createApplication_licenseUrl :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
createApplication_licenseUrl = Lens.lens (\CreateApplication' {licenseUrl} -> licenseUrl) (\s@CreateApplication' {} a -> s {licenseUrl = a} :: CreateApplication)

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
createApplication_labels :: Lens.Lens' CreateApplication (Core.Maybe [Core.Text])
createApplication_labels = Lens.lens (\CreateApplication' {labels} -> labels) (\s@CreateApplication' {} a -> s {labels = a} :: CreateApplication) Core.. Lens.mapping Lens._Coerce

-- | A local text readme file in Markdown language that contains a more
-- detailed description of the application and how it works. The file has
-- the format file:\/\/\<path>\/\<filename>.
--
-- Maximum size 5 MB
--
-- You can specify only one of readmeBody and readmeUrl; otherwise, an
-- error results.
createApplication_readmeBody :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
createApplication_readmeBody = Lens.lens (\CreateApplication' {readmeBody} -> readmeBody) (\s@CreateApplication' {} a -> s {readmeBody = a} :: CreateApplication)

-- | A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
createApplication_homePageUrl :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
createApplication_homePageUrl = Lens.lens (\CreateApplication' {homePageUrl} -> homePageUrl) (\s@CreateApplication' {} a -> s {homePageUrl = a} :: CreateApplication)

-- | A link to the S3 object that contains the ZIP archive of the source code
-- for this version of your application.
--
-- Maximum size 50 MB
createApplication_sourceCodeArchiveUrl :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
createApplication_sourceCodeArchiveUrl = Lens.lens (\CreateApplication' {sourceCodeArchiveUrl} -> sourceCodeArchiveUrl) (\s@CreateApplication' {} a -> s {sourceCodeArchiveUrl = a} :: CreateApplication)

-- | A link to the S3 object in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- You can specify only one of readmeBody and readmeUrl; otherwise, an
-- error results.
createApplication_readmeUrl :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
createApplication_readmeUrl = Lens.lens (\CreateApplication' {readmeUrl} -> readmeUrl) (\s@CreateApplication' {} a -> s {readmeUrl = a} :: CreateApplication)

-- | A link to a public repository for the source code of your application,
-- for example the URL of a specific GitHub commit.
createApplication_sourceCodeUrl :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
createApplication_sourceCodeUrl = Lens.lens (\CreateApplication' {sourceCodeUrl} -> sourceCodeUrl) (\s@CreateApplication' {} a -> s {sourceCodeUrl = a} :: CreateApplication)

-- | The semantic version of the application:
--
-- <https://semver.org/>
createApplication_semanticVersion :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
createApplication_semanticVersion = Lens.lens (\CreateApplication' {semanticVersion} -> semanticVersion) (\s@CreateApplication' {} a -> s {semanticVersion = a} :: CreateApplication)

-- | The local raw packaged AWS SAM template file of your application. The
-- file has the format file:\/\/\<path>\/\<filename>.
--
-- You can specify only one of templateBody and templateUrl; otherwise an
-- error results.
createApplication_templateBody :: Lens.Lens' CreateApplication (Core.Maybe Core.Text)
createApplication_templateBody = Lens.lens (\CreateApplication' {templateBody} -> templateBody) (\s@CreateApplication' {} a -> s {templateBody = a} :: CreateApplication)

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
createApplication_description :: Lens.Lens' CreateApplication Core.Text
createApplication_description = Lens.lens (\CreateApplication' {description} -> description) (\s@CreateApplication' {} a -> s {description = a} :: CreateApplication)

-- | The name of the application that you want to publish.
--
-- Minimum length=1. Maximum length=140
--
-- Pattern: \"[a-zA-Z0-9\\\\-]+\";
createApplication_name :: Lens.Lens' CreateApplication Core.Text
createApplication_name = Lens.lens (\CreateApplication' {name} -> name) (\s@CreateApplication' {} a -> s {name = a} :: CreateApplication)

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
createApplication_author :: Lens.Lens' CreateApplication Core.Text
createApplication_author = Lens.lens (\CreateApplication' {author} -> author) (\s@CreateApplication' {} a -> s {author = a} :: CreateApplication)

instance Core.AWSRequest CreateApplication where
  type
    AWSResponse CreateApplication =
      CreateApplicationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApplicationResponse'
            Core.<$> (x Core..?> "applicationId")
            Core.<*> (x Core..?> "creationTime")
            Core.<*> (x Core..?> "spdxLicenseId")
            Core.<*> (x Core..?> "licenseUrl")
            Core.<*> (x Core..?> "verifiedAuthorUrl")
            Core.<*> (x Core..?> "labels" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "author")
            Core.<*> (x Core..?> "version")
            Core.<*> (x Core..?> "homePageUrl")
            Core.<*> (x Core..?> "name")
            Core.<*> (x Core..?> "isVerifiedAuthor")
            Core.<*> (x Core..?> "readmeUrl")
            Core.<*> (x Core..?> "description")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable CreateApplication

instance Core.NFData CreateApplication

instance Core.ToHeaders CreateApplication where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON CreateApplication where
  toJSON CreateApplication' {..} =
    Core.object
      ( Core.catMaybes
          [ ("spdxLicenseId" Core..=) Core.<$> spdxLicenseId,
            ("templateUrl" Core..=) Core.<$> templateUrl,
            ("licenseBody" Core..=) Core.<$> licenseBody,
            ("licenseUrl" Core..=) Core.<$> licenseUrl,
            ("labels" Core..=) Core.<$> labels,
            ("readmeBody" Core..=) Core.<$> readmeBody,
            ("homePageUrl" Core..=) Core.<$> homePageUrl,
            ("sourceCodeArchiveUrl" Core..=)
              Core.<$> sourceCodeArchiveUrl,
            ("readmeUrl" Core..=) Core.<$> readmeUrl,
            ("sourceCodeUrl" Core..=) Core.<$> sourceCodeUrl,
            ("semanticVersion" Core..=) Core.<$> semanticVersion,
            ("templateBody" Core..=) Core.<$> templateBody,
            Core.Just ("description" Core..= description),
            Core.Just ("name" Core..= name),
            Core.Just ("author" Core..= author)
          ]
      )

instance Core.ToPath CreateApplication where
  toPath = Core.const "/applications"

instance Core.ToQuery CreateApplication where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { -- | The application Amazon Resource Name (ARN).
    applicationId :: Core.Maybe Core.Text,
    -- | The date and time this resource was created.
    creationTime :: Core.Maybe Core.Text,
    -- | A valid identifier from https:\/\/spdx.org\/licenses\/.
    spdxLicenseId :: Core.Maybe Core.Text,
    -- | A link to a license file of the app that matches the spdxLicenseID value
    -- of your application.
    --
    -- Maximum size 5 MB
    licenseUrl :: Core.Maybe Core.Text,
    -- | The URL to the public profile of a verified author. This URL is
    -- submitted by the author.
    verifiedAuthorUrl :: Core.Maybe Core.Text,
    -- | Labels to improve discovery of apps in search results.
    --
    -- Minimum length=1. Maximum length=127. Maximum number of labels: 10
    --
    -- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
    labels :: Core.Maybe [Core.Text],
    -- | The name of the author publishing the app.
    --
    -- Minimum length=1. Maximum length=127.
    --
    -- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
    author :: Core.Maybe Core.Text,
    -- | Version information about the application.
    version :: Core.Maybe Version,
    -- | A URL with more information about the application, for example the
    -- location of your GitHub repository for the application.
    homePageUrl :: Core.Maybe Core.Text,
    -- | The name of the application.
    --
    -- Minimum length=1. Maximum length=140
    --
    -- Pattern: \"[a-zA-Z0-9\\\\-]+\";
    name :: Core.Maybe Core.Text,
    -- | Whether the author of this application has been verified. This means
    -- means that AWS has made a good faith review, as a reasonable and prudent
    -- service provider, of the information provided by the requester and has
    -- confirmed that the requester\'s identity is as claimed.
    isVerifiedAuthor :: Core.Maybe Core.Bool,
    -- | A link to the readme file in Markdown language that contains a more
    -- detailed description of the application and how it works.
    --
    -- Maximum size 5 MB
    readmeUrl :: Core.Maybe Core.Text,
    -- | The description of the application.
    --
    -- Minimum length=1. Maximum length=256
    description :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CreateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'createApplicationResponse_applicationId' - The application Amazon Resource Name (ARN).
--
-- 'creationTime', 'createApplicationResponse_creationTime' - The date and time this resource was created.
--
-- 'spdxLicenseId', 'createApplicationResponse_spdxLicenseId' - A valid identifier from https:\/\/spdx.org\/licenses\/.
--
-- 'licenseUrl', 'createApplicationResponse_licenseUrl' - A link to a license file of the app that matches the spdxLicenseID value
-- of your application.
--
-- Maximum size 5 MB
--
-- 'verifiedAuthorUrl', 'createApplicationResponse_verifiedAuthorUrl' - The URL to the public profile of a verified author. This URL is
-- submitted by the author.
--
-- 'labels', 'createApplicationResponse_labels' - Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
--
-- 'author', 'createApplicationResponse_author' - The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
--
-- 'version', 'createApplicationResponse_version' - Version information about the application.
--
-- 'homePageUrl', 'createApplicationResponse_homePageUrl' - A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
--
-- 'name', 'createApplicationResponse_name' - The name of the application.
--
-- Minimum length=1. Maximum length=140
--
-- Pattern: \"[a-zA-Z0-9\\\\-]+\";
--
-- 'isVerifiedAuthor', 'createApplicationResponse_isVerifiedAuthor' - Whether the author of this application has been verified. This means
-- means that AWS has made a good faith review, as a reasonable and prudent
-- service provider, of the information provided by the requester and has
-- confirmed that the requester\'s identity is as claimed.
--
-- 'readmeUrl', 'createApplicationResponse_readmeUrl' - A link to the readme file in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- 'description', 'createApplicationResponse_description' - The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- 'httpStatus', 'createApplicationResponse_httpStatus' - The response's http status code.
newCreateApplicationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  CreateApplicationResponse
newCreateApplicationResponse pHttpStatus_ =
  CreateApplicationResponse'
    { applicationId =
        Core.Nothing,
      creationTime = Core.Nothing,
      spdxLicenseId = Core.Nothing,
      licenseUrl = Core.Nothing,
      verifiedAuthorUrl = Core.Nothing,
      labels = Core.Nothing,
      author = Core.Nothing,
      version = Core.Nothing,
      homePageUrl = Core.Nothing,
      name = Core.Nothing,
      isVerifiedAuthor = Core.Nothing,
      readmeUrl = Core.Nothing,
      description = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The application Amazon Resource Name (ARN).
createApplicationResponse_applicationId :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
createApplicationResponse_applicationId = Lens.lens (\CreateApplicationResponse' {applicationId} -> applicationId) (\s@CreateApplicationResponse' {} a -> s {applicationId = a} :: CreateApplicationResponse)

-- | The date and time this resource was created.
createApplicationResponse_creationTime :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
createApplicationResponse_creationTime = Lens.lens (\CreateApplicationResponse' {creationTime} -> creationTime) (\s@CreateApplicationResponse' {} a -> s {creationTime = a} :: CreateApplicationResponse)

-- | A valid identifier from https:\/\/spdx.org\/licenses\/.
createApplicationResponse_spdxLicenseId :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
createApplicationResponse_spdxLicenseId = Lens.lens (\CreateApplicationResponse' {spdxLicenseId} -> spdxLicenseId) (\s@CreateApplicationResponse' {} a -> s {spdxLicenseId = a} :: CreateApplicationResponse)

-- | A link to a license file of the app that matches the spdxLicenseID value
-- of your application.
--
-- Maximum size 5 MB
createApplicationResponse_licenseUrl :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
createApplicationResponse_licenseUrl = Lens.lens (\CreateApplicationResponse' {licenseUrl} -> licenseUrl) (\s@CreateApplicationResponse' {} a -> s {licenseUrl = a} :: CreateApplicationResponse)

-- | The URL to the public profile of a verified author. This URL is
-- submitted by the author.
createApplicationResponse_verifiedAuthorUrl :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
createApplicationResponse_verifiedAuthorUrl = Lens.lens (\CreateApplicationResponse' {verifiedAuthorUrl} -> verifiedAuthorUrl) (\s@CreateApplicationResponse' {} a -> s {verifiedAuthorUrl = a} :: CreateApplicationResponse)

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
createApplicationResponse_labels :: Lens.Lens' CreateApplicationResponse (Core.Maybe [Core.Text])
createApplicationResponse_labels = Lens.lens (\CreateApplicationResponse' {labels} -> labels) (\s@CreateApplicationResponse' {} a -> s {labels = a} :: CreateApplicationResponse) Core.. Lens.mapping Lens._Coerce

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
createApplicationResponse_author :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
createApplicationResponse_author = Lens.lens (\CreateApplicationResponse' {author} -> author) (\s@CreateApplicationResponse' {} a -> s {author = a} :: CreateApplicationResponse)

-- | Version information about the application.
createApplicationResponse_version :: Lens.Lens' CreateApplicationResponse (Core.Maybe Version)
createApplicationResponse_version = Lens.lens (\CreateApplicationResponse' {version} -> version) (\s@CreateApplicationResponse' {} a -> s {version = a} :: CreateApplicationResponse)

-- | A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
createApplicationResponse_homePageUrl :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
createApplicationResponse_homePageUrl = Lens.lens (\CreateApplicationResponse' {homePageUrl} -> homePageUrl) (\s@CreateApplicationResponse' {} a -> s {homePageUrl = a} :: CreateApplicationResponse)

-- | The name of the application.
--
-- Minimum length=1. Maximum length=140
--
-- Pattern: \"[a-zA-Z0-9\\\\-]+\";
createApplicationResponse_name :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
createApplicationResponse_name = Lens.lens (\CreateApplicationResponse' {name} -> name) (\s@CreateApplicationResponse' {} a -> s {name = a} :: CreateApplicationResponse)

-- | Whether the author of this application has been verified. This means
-- means that AWS has made a good faith review, as a reasonable and prudent
-- service provider, of the information provided by the requester and has
-- confirmed that the requester\'s identity is as claimed.
createApplicationResponse_isVerifiedAuthor :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Bool)
createApplicationResponse_isVerifiedAuthor = Lens.lens (\CreateApplicationResponse' {isVerifiedAuthor} -> isVerifiedAuthor) (\s@CreateApplicationResponse' {} a -> s {isVerifiedAuthor = a} :: CreateApplicationResponse)

-- | A link to the readme file in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
createApplicationResponse_readmeUrl :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
createApplicationResponse_readmeUrl = Lens.lens (\CreateApplicationResponse' {readmeUrl} -> readmeUrl) (\s@CreateApplicationResponse' {} a -> s {readmeUrl = a} :: CreateApplicationResponse)

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
createApplicationResponse_description :: Lens.Lens' CreateApplicationResponse (Core.Maybe Core.Text)
createApplicationResponse_description = Lens.lens (\CreateApplicationResponse' {description} -> description) (\s@CreateApplicationResponse' {} a -> s {description = a} :: CreateApplicationResponse)

-- | The response's http status code.
createApplicationResponse_httpStatus :: Lens.Lens' CreateApplicationResponse Core.Int
createApplicationResponse_httpStatus = Lens.lens (\CreateApplicationResponse' {httpStatus} -> httpStatus) (\s@CreateApplicationResponse' {} a -> s {httpStatus = a} :: CreateApplicationResponse)

instance Core.NFData CreateApplicationResponse
