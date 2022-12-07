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
-- Module      : Amazonka.ServerlessApplicationRepository.CreateApplication
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an application, optionally including an AWS SAM file to create
-- the first application version in the same call.
module Amazonka.ServerlessApplicationRepository.CreateApplication
  ( -- * Creating a Request
    CreateApplication (..),
    newCreateApplication,

    -- * Request Lenses
    createApplication_semanticVersion,
    createApplication_sourceCodeArchiveUrl,
    createApplication_licenseUrl,
    createApplication_sourceCodeUrl,
    createApplication_readmeUrl,
    createApplication_templateBody,
    createApplication_spdxLicenseId,
    createApplication_licenseBody,
    createApplication_homePageUrl,
    createApplication_labels,
    createApplication_readmeBody,
    createApplication_templateUrl,
    createApplication_description,
    createApplication_name,
    createApplication_author,

    -- * Destructuring the Response
    CreateApplicationResponse (..),
    newCreateApplicationResponse,

    -- * Response Lenses
    createApplicationResponse_author,
    createApplicationResponse_name,
    createApplicationResponse_isVerifiedAuthor,
    createApplicationResponse_licenseUrl,
    createApplicationResponse_verifiedAuthorUrl,
    createApplicationResponse_readmeUrl,
    createApplicationResponse_spdxLicenseId,
    createApplicationResponse_description,
    createApplicationResponse_homePageUrl,
    createApplicationResponse_labels,
    createApplicationResponse_creationTime,
    createApplicationResponse_applicationId,
    createApplicationResponse_version,
    createApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServerlessApplicationRepository.Types

-- | /See:/ 'newCreateApplication' smart constructor.
data CreateApplication = CreateApplication'
  { -- | The semantic version of the application:
    --
    -- <https://semver.org/>
    semanticVersion :: Prelude.Maybe Prelude.Text,
    -- | A link to the S3 object that contains the ZIP archive of the source code
    -- for this version of your application.
    --
    -- Maximum size 50 MB
    sourceCodeArchiveUrl :: Prelude.Maybe Prelude.Text,
    -- | A link to the S3 object that contains the license of the app that
    -- matches the spdxLicenseID value of your application.
    --
    -- Maximum size 5 MB
    --
    -- You can specify only one of licenseBody and licenseUrl; otherwise, an
    -- error results.
    licenseUrl :: Prelude.Maybe Prelude.Text,
    -- | A link to a public repository for the source code of your application,
    -- for example the URL of a specific GitHub commit.
    sourceCodeUrl :: Prelude.Maybe Prelude.Text,
    -- | A link to the S3 object in Markdown language that contains a more
    -- detailed description of the application and how it works.
    --
    -- Maximum size 5 MB
    --
    -- You can specify only one of readmeBody and readmeUrl; otherwise, an
    -- error results.
    readmeUrl :: Prelude.Maybe Prelude.Text,
    -- | The local raw packaged AWS SAM template file of your application. The
    -- file has the format file:\/\/\<path>\/\<filename>.
    --
    -- You can specify only one of templateBody and templateUrl; otherwise an
    -- error results.
    templateBody :: Prelude.Maybe Prelude.Text,
    -- | A valid identifier from <https://spdx.org/licenses/>.
    spdxLicenseId :: Prelude.Maybe Prelude.Text,
    -- | A local text file that contains the license of the app that matches the
    -- spdxLicenseID value of your application. The file has the format
    -- file:\/\/\<path>\/\<filename>.
    --
    -- Maximum size 5 MB
    --
    -- You can specify only one of licenseBody and licenseUrl; otherwise, an
    -- error results.
    licenseBody :: Prelude.Maybe Prelude.Text,
    -- | A URL with more information about the application, for example the
    -- location of your GitHub repository for the application.
    homePageUrl :: Prelude.Maybe Prelude.Text,
    -- | Labels to improve discovery of apps in search results.
    --
    -- Minimum length=1. Maximum length=127. Maximum number of labels: 10
    --
    -- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
    labels :: Prelude.Maybe [Prelude.Text],
    -- | A local text readme file in Markdown language that contains a more
    -- detailed description of the application and how it works. The file has
    -- the format file:\/\/\<path>\/\<filename>.
    --
    -- Maximum size 5 MB
    --
    -- You can specify only one of readmeBody and readmeUrl; otherwise, an
    -- error results.
    readmeBody :: Prelude.Maybe Prelude.Text,
    -- | A link to the S3 object containing the packaged AWS SAM template of your
    -- application.
    --
    -- You can specify only one of templateBody and templateUrl; otherwise an
    -- error results.
    templateUrl :: Prelude.Maybe Prelude.Text,
    -- | The description of the application.
    --
    -- Minimum length=1. Maximum length=256
    description :: Prelude.Text,
    -- | The name of the application that you want to publish.
    --
    -- Minimum length=1. Maximum length=140
    --
    -- Pattern: \"[a-zA-Z0-9\\\\-]+\";
    name :: Prelude.Text,
    -- | The name of the author publishing the app.
    --
    -- Minimum length=1. Maximum length=127.
    --
    -- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
    author :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'semanticVersion', 'createApplication_semanticVersion' - The semantic version of the application:
--
-- <https://semver.org/>
--
-- 'sourceCodeArchiveUrl', 'createApplication_sourceCodeArchiveUrl' - A link to the S3 object that contains the ZIP archive of the source code
-- for this version of your application.
--
-- Maximum size 50 MB
--
-- 'licenseUrl', 'createApplication_licenseUrl' - A link to the S3 object that contains the license of the app that
-- matches the spdxLicenseID value of your application.
--
-- Maximum size 5 MB
--
-- You can specify only one of licenseBody and licenseUrl; otherwise, an
-- error results.
--
-- 'sourceCodeUrl', 'createApplication_sourceCodeUrl' - A link to a public repository for the source code of your application,
-- for example the URL of a specific GitHub commit.
--
-- 'readmeUrl', 'createApplication_readmeUrl' - A link to the S3 object in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- You can specify only one of readmeBody and readmeUrl; otherwise, an
-- error results.
--
-- 'templateBody', 'createApplication_templateBody' - The local raw packaged AWS SAM template file of your application. The
-- file has the format file:\/\/\<path>\/\<filename>.
--
-- You can specify only one of templateBody and templateUrl; otherwise an
-- error results.
--
-- 'spdxLicenseId', 'createApplication_spdxLicenseId' - A valid identifier from <https://spdx.org/licenses/>.
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
-- 'homePageUrl', 'createApplication_homePageUrl' - A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
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
-- 'templateUrl', 'createApplication_templateUrl' - A link to the S3 object containing the packaged AWS SAM template of your
-- application.
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
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'author'
  Prelude.Text ->
  CreateApplication
newCreateApplication pDescription_ pName_ pAuthor_ =
  CreateApplication'
    { semanticVersion =
        Prelude.Nothing,
      sourceCodeArchiveUrl = Prelude.Nothing,
      licenseUrl = Prelude.Nothing,
      sourceCodeUrl = Prelude.Nothing,
      readmeUrl = Prelude.Nothing,
      templateBody = Prelude.Nothing,
      spdxLicenseId = Prelude.Nothing,
      licenseBody = Prelude.Nothing,
      homePageUrl = Prelude.Nothing,
      labels = Prelude.Nothing,
      readmeBody = Prelude.Nothing,
      templateUrl = Prelude.Nothing,
      description = pDescription_,
      name = pName_,
      author = pAuthor_
    }

-- | The semantic version of the application:
--
-- <https://semver.org/>
createApplication_semanticVersion :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_semanticVersion = Lens.lens (\CreateApplication' {semanticVersion} -> semanticVersion) (\s@CreateApplication' {} a -> s {semanticVersion = a} :: CreateApplication)

-- | A link to the S3 object that contains the ZIP archive of the source code
-- for this version of your application.
--
-- Maximum size 50 MB
createApplication_sourceCodeArchiveUrl :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_sourceCodeArchiveUrl = Lens.lens (\CreateApplication' {sourceCodeArchiveUrl} -> sourceCodeArchiveUrl) (\s@CreateApplication' {} a -> s {sourceCodeArchiveUrl = a} :: CreateApplication)

-- | A link to the S3 object that contains the license of the app that
-- matches the spdxLicenseID value of your application.
--
-- Maximum size 5 MB
--
-- You can specify only one of licenseBody and licenseUrl; otherwise, an
-- error results.
createApplication_licenseUrl :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_licenseUrl = Lens.lens (\CreateApplication' {licenseUrl} -> licenseUrl) (\s@CreateApplication' {} a -> s {licenseUrl = a} :: CreateApplication)

-- | A link to a public repository for the source code of your application,
-- for example the URL of a specific GitHub commit.
createApplication_sourceCodeUrl :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_sourceCodeUrl = Lens.lens (\CreateApplication' {sourceCodeUrl} -> sourceCodeUrl) (\s@CreateApplication' {} a -> s {sourceCodeUrl = a} :: CreateApplication)

-- | A link to the S3 object in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- You can specify only one of readmeBody and readmeUrl; otherwise, an
-- error results.
createApplication_readmeUrl :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_readmeUrl = Lens.lens (\CreateApplication' {readmeUrl} -> readmeUrl) (\s@CreateApplication' {} a -> s {readmeUrl = a} :: CreateApplication)

-- | The local raw packaged AWS SAM template file of your application. The
-- file has the format file:\/\/\<path>\/\<filename>.
--
-- You can specify only one of templateBody and templateUrl; otherwise an
-- error results.
createApplication_templateBody :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_templateBody = Lens.lens (\CreateApplication' {templateBody} -> templateBody) (\s@CreateApplication' {} a -> s {templateBody = a} :: CreateApplication)

-- | A valid identifier from <https://spdx.org/licenses/>.
createApplication_spdxLicenseId :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_spdxLicenseId = Lens.lens (\CreateApplication' {spdxLicenseId} -> spdxLicenseId) (\s@CreateApplication' {} a -> s {spdxLicenseId = a} :: CreateApplication)

-- | A local text file that contains the license of the app that matches the
-- spdxLicenseID value of your application. The file has the format
-- file:\/\/\<path>\/\<filename>.
--
-- Maximum size 5 MB
--
-- You can specify only one of licenseBody and licenseUrl; otherwise, an
-- error results.
createApplication_licenseBody :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_licenseBody = Lens.lens (\CreateApplication' {licenseBody} -> licenseBody) (\s@CreateApplication' {} a -> s {licenseBody = a} :: CreateApplication)

-- | A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
createApplication_homePageUrl :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_homePageUrl = Lens.lens (\CreateApplication' {homePageUrl} -> homePageUrl) (\s@CreateApplication' {} a -> s {homePageUrl = a} :: CreateApplication)

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
createApplication_labels :: Lens.Lens' CreateApplication (Prelude.Maybe [Prelude.Text])
createApplication_labels = Lens.lens (\CreateApplication' {labels} -> labels) (\s@CreateApplication' {} a -> s {labels = a} :: CreateApplication) Prelude.. Lens.mapping Lens.coerced

-- | A local text readme file in Markdown language that contains a more
-- detailed description of the application and how it works. The file has
-- the format file:\/\/\<path>\/\<filename>.
--
-- Maximum size 5 MB
--
-- You can specify only one of readmeBody and readmeUrl; otherwise, an
-- error results.
createApplication_readmeBody :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_readmeBody = Lens.lens (\CreateApplication' {readmeBody} -> readmeBody) (\s@CreateApplication' {} a -> s {readmeBody = a} :: CreateApplication)

-- | A link to the S3 object containing the packaged AWS SAM template of your
-- application.
--
-- You can specify only one of templateBody and templateUrl; otherwise an
-- error results.
createApplication_templateUrl :: Lens.Lens' CreateApplication (Prelude.Maybe Prelude.Text)
createApplication_templateUrl = Lens.lens (\CreateApplication' {templateUrl} -> templateUrl) (\s@CreateApplication' {} a -> s {templateUrl = a} :: CreateApplication)

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
createApplication_description :: Lens.Lens' CreateApplication Prelude.Text
createApplication_description = Lens.lens (\CreateApplication' {description} -> description) (\s@CreateApplication' {} a -> s {description = a} :: CreateApplication)

-- | The name of the application that you want to publish.
--
-- Minimum length=1. Maximum length=140
--
-- Pattern: \"[a-zA-Z0-9\\\\-]+\";
createApplication_name :: Lens.Lens' CreateApplication Prelude.Text
createApplication_name = Lens.lens (\CreateApplication' {name} -> name) (\s@CreateApplication' {} a -> s {name = a} :: CreateApplication)

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
createApplication_author :: Lens.Lens' CreateApplication Prelude.Text
createApplication_author = Lens.lens (\CreateApplication' {author} -> author) (\s@CreateApplication' {} a -> s {author = a} :: CreateApplication)

instance Core.AWSRequest CreateApplication where
  type
    AWSResponse CreateApplication =
      CreateApplicationResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateApplicationResponse'
            Prelude.<$> (x Data..?> "author")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "isVerifiedAuthor")
            Prelude.<*> (x Data..?> "licenseUrl")
            Prelude.<*> (x Data..?> "verifiedAuthorUrl")
            Prelude.<*> (x Data..?> "readmeUrl")
            Prelude.<*> (x Data..?> "spdxLicenseId")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "homePageUrl")
            Prelude.<*> (x Data..?> "labels" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "applicationId")
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateApplication where
  hashWithSalt _salt CreateApplication' {..} =
    _salt `Prelude.hashWithSalt` semanticVersion
      `Prelude.hashWithSalt` sourceCodeArchiveUrl
      `Prelude.hashWithSalt` licenseUrl
      `Prelude.hashWithSalt` sourceCodeUrl
      `Prelude.hashWithSalt` readmeUrl
      `Prelude.hashWithSalt` templateBody
      `Prelude.hashWithSalt` spdxLicenseId
      `Prelude.hashWithSalt` licenseBody
      `Prelude.hashWithSalt` homePageUrl
      `Prelude.hashWithSalt` labels
      `Prelude.hashWithSalt` readmeBody
      `Prelude.hashWithSalt` templateUrl
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` author

instance Prelude.NFData CreateApplication where
  rnf CreateApplication' {..} =
    Prelude.rnf semanticVersion
      `Prelude.seq` Prelude.rnf sourceCodeArchiveUrl
      `Prelude.seq` Prelude.rnf licenseUrl
      `Prelude.seq` Prelude.rnf sourceCodeUrl
      `Prelude.seq` Prelude.rnf readmeUrl
      `Prelude.seq` Prelude.rnf templateBody
      `Prelude.seq` Prelude.rnf spdxLicenseId
      `Prelude.seq` Prelude.rnf licenseBody
      `Prelude.seq` Prelude.rnf homePageUrl
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf readmeBody
      `Prelude.seq` Prelude.rnf templateUrl
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf author

instance Data.ToHeaders CreateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateApplication where
  toJSON CreateApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("semanticVersion" Data..=)
              Prelude.<$> semanticVersion,
            ("sourceCodeArchiveUrl" Data..=)
              Prelude.<$> sourceCodeArchiveUrl,
            ("licenseUrl" Data..=) Prelude.<$> licenseUrl,
            ("sourceCodeUrl" Data..=) Prelude.<$> sourceCodeUrl,
            ("readmeUrl" Data..=) Prelude.<$> readmeUrl,
            ("templateBody" Data..=) Prelude.<$> templateBody,
            ("spdxLicenseId" Data..=) Prelude.<$> spdxLicenseId,
            ("licenseBody" Data..=) Prelude.<$> licenseBody,
            ("homePageUrl" Data..=) Prelude.<$> homePageUrl,
            ("labels" Data..=) Prelude.<$> labels,
            ("readmeBody" Data..=) Prelude.<$> readmeBody,
            ("templateUrl" Data..=) Prelude.<$> templateUrl,
            Prelude.Just ("description" Data..= description),
            Prelude.Just ("name" Data..= name),
            Prelude.Just ("author" Data..= author)
          ]
      )

instance Data.ToPath CreateApplication where
  toPath = Prelude.const "/applications"

instance Data.ToQuery CreateApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateApplicationResponse' smart constructor.
data CreateApplicationResponse = CreateApplicationResponse'
  { -- | The name of the author publishing the app.
    --
    -- Minimum length=1. Maximum length=127.
    --
    -- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
    author :: Prelude.Maybe Prelude.Text,
    -- | The name of the application.
    --
    -- Minimum length=1. Maximum length=140
    --
    -- Pattern: \"[a-zA-Z0-9\\\\-]+\";
    name :: Prelude.Maybe Prelude.Text,
    -- | Whether the author of this application has been verified. This means
    -- means that AWS has made a good faith review, as a reasonable and prudent
    -- service provider, of the information provided by the requester and has
    -- confirmed that the requester\'s identity is as claimed.
    isVerifiedAuthor :: Prelude.Maybe Prelude.Bool,
    -- | A link to a license file of the app that matches the spdxLicenseID value
    -- of your application.
    --
    -- Maximum size 5 MB
    licenseUrl :: Prelude.Maybe Prelude.Text,
    -- | The URL to the public profile of a verified author. This URL is
    -- submitted by the author.
    verifiedAuthorUrl :: Prelude.Maybe Prelude.Text,
    -- | A link to the readme file in Markdown language that contains a more
    -- detailed description of the application and how it works.
    --
    -- Maximum size 5 MB
    readmeUrl :: Prelude.Maybe Prelude.Text,
    -- | A valid identifier from https:\/\/spdx.org\/licenses\/.
    spdxLicenseId :: Prelude.Maybe Prelude.Text,
    -- | The description of the application.
    --
    -- Minimum length=1. Maximum length=256
    description :: Prelude.Maybe Prelude.Text,
    -- | A URL with more information about the application, for example the
    -- location of your GitHub repository for the application.
    homePageUrl :: Prelude.Maybe Prelude.Text,
    -- | Labels to improve discovery of apps in search results.
    --
    -- Minimum length=1. Maximum length=127. Maximum number of labels: 10
    --
    -- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
    labels :: Prelude.Maybe [Prelude.Text],
    -- | The date and time this resource was created.
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | The application Amazon Resource Name (ARN).
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | Version information about the application.
    version :: Prelude.Maybe Version,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'author', 'createApplicationResponse_author' - The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
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
-- 'licenseUrl', 'createApplicationResponse_licenseUrl' - A link to a license file of the app that matches the spdxLicenseID value
-- of your application.
--
-- Maximum size 5 MB
--
-- 'verifiedAuthorUrl', 'createApplicationResponse_verifiedAuthorUrl' - The URL to the public profile of a verified author. This URL is
-- submitted by the author.
--
-- 'readmeUrl', 'createApplicationResponse_readmeUrl' - A link to the readme file in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- 'spdxLicenseId', 'createApplicationResponse_spdxLicenseId' - A valid identifier from https:\/\/spdx.org\/licenses\/.
--
-- 'description', 'createApplicationResponse_description' - The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- 'homePageUrl', 'createApplicationResponse_homePageUrl' - A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
--
-- 'labels', 'createApplicationResponse_labels' - Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
--
-- 'creationTime', 'createApplicationResponse_creationTime' - The date and time this resource was created.
--
-- 'applicationId', 'createApplicationResponse_applicationId' - The application Amazon Resource Name (ARN).
--
-- 'version', 'createApplicationResponse_version' - Version information about the application.
--
-- 'httpStatus', 'createApplicationResponse_httpStatus' - The response's http status code.
newCreateApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateApplicationResponse
newCreateApplicationResponse pHttpStatus_ =
  CreateApplicationResponse'
    { author =
        Prelude.Nothing,
      name = Prelude.Nothing,
      isVerifiedAuthor = Prelude.Nothing,
      licenseUrl = Prelude.Nothing,
      verifiedAuthorUrl = Prelude.Nothing,
      readmeUrl = Prelude.Nothing,
      spdxLicenseId = Prelude.Nothing,
      description = Prelude.Nothing,
      homePageUrl = Prelude.Nothing,
      labels = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      applicationId = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
createApplicationResponse_author :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Text)
createApplicationResponse_author = Lens.lens (\CreateApplicationResponse' {author} -> author) (\s@CreateApplicationResponse' {} a -> s {author = a} :: CreateApplicationResponse)

-- | The name of the application.
--
-- Minimum length=1. Maximum length=140
--
-- Pattern: \"[a-zA-Z0-9\\\\-]+\";
createApplicationResponse_name :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Text)
createApplicationResponse_name = Lens.lens (\CreateApplicationResponse' {name} -> name) (\s@CreateApplicationResponse' {} a -> s {name = a} :: CreateApplicationResponse)

-- | Whether the author of this application has been verified. This means
-- means that AWS has made a good faith review, as a reasonable and prudent
-- service provider, of the information provided by the requester and has
-- confirmed that the requester\'s identity is as claimed.
createApplicationResponse_isVerifiedAuthor :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Bool)
createApplicationResponse_isVerifiedAuthor = Lens.lens (\CreateApplicationResponse' {isVerifiedAuthor} -> isVerifiedAuthor) (\s@CreateApplicationResponse' {} a -> s {isVerifiedAuthor = a} :: CreateApplicationResponse)

-- | A link to a license file of the app that matches the spdxLicenseID value
-- of your application.
--
-- Maximum size 5 MB
createApplicationResponse_licenseUrl :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Text)
createApplicationResponse_licenseUrl = Lens.lens (\CreateApplicationResponse' {licenseUrl} -> licenseUrl) (\s@CreateApplicationResponse' {} a -> s {licenseUrl = a} :: CreateApplicationResponse)

-- | The URL to the public profile of a verified author. This URL is
-- submitted by the author.
createApplicationResponse_verifiedAuthorUrl :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Text)
createApplicationResponse_verifiedAuthorUrl = Lens.lens (\CreateApplicationResponse' {verifiedAuthorUrl} -> verifiedAuthorUrl) (\s@CreateApplicationResponse' {} a -> s {verifiedAuthorUrl = a} :: CreateApplicationResponse)

-- | A link to the readme file in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
createApplicationResponse_readmeUrl :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Text)
createApplicationResponse_readmeUrl = Lens.lens (\CreateApplicationResponse' {readmeUrl} -> readmeUrl) (\s@CreateApplicationResponse' {} a -> s {readmeUrl = a} :: CreateApplicationResponse)

-- | A valid identifier from https:\/\/spdx.org\/licenses\/.
createApplicationResponse_spdxLicenseId :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Text)
createApplicationResponse_spdxLicenseId = Lens.lens (\CreateApplicationResponse' {spdxLicenseId} -> spdxLicenseId) (\s@CreateApplicationResponse' {} a -> s {spdxLicenseId = a} :: CreateApplicationResponse)

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
createApplicationResponse_description :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Text)
createApplicationResponse_description = Lens.lens (\CreateApplicationResponse' {description} -> description) (\s@CreateApplicationResponse' {} a -> s {description = a} :: CreateApplicationResponse)

-- | A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
createApplicationResponse_homePageUrl :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Text)
createApplicationResponse_homePageUrl = Lens.lens (\CreateApplicationResponse' {homePageUrl} -> homePageUrl) (\s@CreateApplicationResponse' {} a -> s {homePageUrl = a} :: CreateApplicationResponse)

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
createApplicationResponse_labels :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe [Prelude.Text])
createApplicationResponse_labels = Lens.lens (\CreateApplicationResponse' {labels} -> labels) (\s@CreateApplicationResponse' {} a -> s {labels = a} :: CreateApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | The date and time this resource was created.
createApplicationResponse_creationTime :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Text)
createApplicationResponse_creationTime = Lens.lens (\CreateApplicationResponse' {creationTime} -> creationTime) (\s@CreateApplicationResponse' {} a -> s {creationTime = a} :: CreateApplicationResponse)

-- | The application Amazon Resource Name (ARN).
createApplicationResponse_applicationId :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Prelude.Text)
createApplicationResponse_applicationId = Lens.lens (\CreateApplicationResponse' {applicationId} -> applicationId) (\s@CreateApplicationResponse' {} a -> s {applicationId = a} :: CreateApplicationResponse)

-- | Version information about the application.
createApplicationResponse_version :: Lens.Lens' CreateApplicationResponse (Prelude.Maybe Version)
createApplicationResponse_version = Lens.lens (\CreateApplicationResponse' {version} -> version) (\s@CreateApplicationResponse' {} a -> s {version = a} :: CreateApplicationResponse)

-- | The response's http status code.
createApplicationResponse_httpStatus :: Lens.Lens' CreateApplicationResponse Prelude.Int
createApplicationResponse_httpStatus = Lens.lens (\CreateApplicationResponse' {httpStatus} -> httpStatus) (\s@CreateApplicationResponse' {} a -> s {httpStatus = a} :: CreateApplicationResponse)

instance Prelude.NFData CreateApplicationResponse where
  rnf CreateApplicationResponse' {..} =
    Prelude.rnf author
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf isVerifiedAuthor
      `Prelude.seq` Prelude.rnf licenseUrl
      `Prelude.seq` Prelude.rnf verifiedAuthorUrl
      `Prelude.seq` Prelude.rnf readmeUrl
      `Prelude.seq` Prelude.rnf spdxLicenseId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf homePageUrl
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
