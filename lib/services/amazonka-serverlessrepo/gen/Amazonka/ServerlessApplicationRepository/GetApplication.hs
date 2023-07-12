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
-- Module      : Amazonka.ServerlessApplicationRepository.GetApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified application.
module Amazonka.ServerlessApplicationRepository.GetApplication
  ( -- * Creating a Request
    GetApplication (..),
    newGetApplication,

    -- * Request Lenses
    getApplication_semanticVersion,
    getApplication_applicationId,

    -- * Destructuring the Response
    GetApplicationResponse (..),
    newGetApplicationResponse,

    -- * Response Lenses
    getApplicationResponse_applicationId,
    getApplicationResponse_author,
    getApplicationResponse_creationTime,
    getApplicationResponse_description,
    getApplicationResponse_homePageUrl,
    getApplicationResponse_isVerifiedAuthor,
    getApplicationResponse_labels,
    getApplicationResponse_licenseUrl,
    getApplicationResponse_name,
    getApplicationResponse_readmeUrl,
    getApplicationResponse_spdxLicenseId,
    getApplicationResponse_verifiedAuthorUrl,
    getApplicationResponse_version,
    getApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServerlessApplicationRepository.Types

-- | /See:/ 'newGetApplication' smart constructor.
data GetApplication = GetApplication'
  { -- | The semantic version of the application to get.
    semanticVersion :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'semanticVersion', 'getApplication_semanticVersion' - The semantic version of the application to get.
--
-- 'applicationId', 'getApplication_applicationId' - The Amazon Resource Name (ARN) of the application.
newGetApplication ::
  -- | 'applicationId'
  Prelude.Text ->
  GetApplication
newGetApplication pApplicationId_ =
  GetApplication'
    { semanticVersion = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | The semantic version of the application to get.
getApplication_semanticVersion :: Lens.Lens' GetApplication (Prelude.Maybe Prelude.Text)
getApplication_semanticVersion = Lens.lens (\GetApplication' {semanticVersion} -> semanticVersion) (\s@GetApplication' {} a -> s {semanticVersion = a} :: GetApplication)

-- | The Amazon Resource Name (ARN) of the application.
getApplication_applicationId :: Lens.Lens' GetApplication Prelude.Text
getApplication_applicationId = Lens.lens (\GetApplication' {applicationId} -> applicationId) (\s@GetApplication' {} a -> s {applicationId = a} :: GetApplication)

instance Core.AWSRequest GetApplication where
  type
    AWSResponse GetApplication =
      GetApplicationResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationResponse'
            Prelude.<$> (x Data..?> "applicationId")
            Prelude.<*> (x Data..?> "author")
            Prelude.<*> (x Data..?> "creationTime")
            Prelude.<*> (x Data..?> "description")
            Prelude.<*> (x Data..?> "homePageUrl")
            Prelude.<*> (x Data..?> "isVerifiedAuthor")
            Prelude.<*> (x Data..?> "labels" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "licenseUrl")
            Prelude.<*> (x Data..?> "name")
            Prelude.<*> (x Data..?> "readmeUrl")
            Prelude.<*> (x Data..?> "spdxLicenseId")
            Prelude.<*> (x Data..?> "verifiedAuthorUrl")
            Prelude.<*> (x Data..?> "version")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetApplication where
  hashWithSalt _salt GetApplication' {..} =
    _salt
      `Prelude.hashWithSalt` semanticVersion
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData GetApplication where
  rnf GetApplication' {..} =
    Prelude.rnf semanticVersion
      `Prelude.seq` Prelude.rnf applicationId

instance Data.ToHeaders GetApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetApplication where
  toPath GetApplication' {..} =
    Prelude.mconcat
      ["/applications/", Data.toBS applicationId]

instance Data.ToQuery GetApplication where
  toQuery GetApplication' {..} =
    Prelude.mconcat
      ["semanticVersion" Data.=: semanticVersion]

-- | /See:/ 'newGetApplicationResponse' smart constructor.
data GetApplicationResponse = GetApplicationResponse'
  { -- | The application Amazon Resource Name (ARN).
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The name of the author publishing the app.
    --
    -- Minimum length=1. Maximum length=127.
    --
    -- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
    author :: Prelude.Maybe Prelude.Text,
    -- | The date and time this resource was created.
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | The description of the application.
    --
    -- Minimum length=1. Maximum length=256
    description :: Prelude.Maybe Prelude.Text,
    -- | A URL with more information about the application, for example the
    -- location of your GitHub repository for the application.
    homePageUrl :: Prelude.Maybe Prelude.Text,
    -- | Whether the author of this application has been verified. This means
    -- means that AWS has made a good faith review, as a reasonable and prudent
    -- service provider, of the information provided by the requester and has
    -- confirmed that the requester\'s identity is as claimed.
    isVerifiedAuthor :: Prelude.Maybe Prelude.Bool,
    -- | Labels to improve discovery of apps in search results.
    --
    -- Minimum length=1. Maximum length=127. Maximum number of labels: 10
    --
    -- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
    labels :: Prelude.Maybe [Prelude.Text],
    -- | A link to a license file of the app that matches the spdxLicenseID value
    -- of your application.
    --
    -- Maximum size 5 MB
    licenseUrl :: Prelude.Maybe Prelude.Text,
    -- | The name of the application.
    --
    -- Minimum length=1. Maximum length=140
    --
    -- Pattern: \"[a-zA-Z0-9\\\\-]+\";
    name :: Prelude.Maybe Prelude.Text,
    -- | A link to the readme file in Markdown language that contains a more
    -- detailed description of the application and how it works.
    --
    -- Maximum size 5 MB
    readmeUrl :: Prelude.Maybe Prelude.Text,
    -- | A valid identifier from https:\/\/spdx.org\/licenses\/.
    spdxLicenseId :: Prelude.Maybe Prelude.Text,
    -- | The URL to the public profile of a verified author. This URL is
    -- submitted by the author.
    verifiedAuthorUrl :: Prelude.Maybe Prelude.Text,
    -- | Version information about the application.
    version :: Prelude.Maybe Version,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'getApplicationResponse_applicationId' - The application Amazon Resource Name (ARN).
--
-- 'author', 'getApplicationResponse_author' - The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
--
-- 'creationTime', 'getApplicationResponse_creationTime' - The date and time this resource was created.
--
-- 'description', 'getApplicationResponse_description' - The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- 'homePageUrl', 'getApplicationResponse_homePageUrl' - A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
--
-- 'isVerifiedAuthor', 'getApplicationResponse_isVerifiedAuthor' - Whether the author of this application has been verified. This means
-- means that AWS has made a good faith review, as a reasonable and prudent
-- service provider, of the information provided by the requester and has
-- confirmed that the requester\'s identity is as claimed.
--
-- 'labels', 'getApplicationResponse_labels' - Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
--
-- 'licenseUrl', 'getApplicationResponse_licenseUrl' - A link to a license file of the app that matches the spdxLicenseID value
-- of your application.
--
-- Maximum size 5 MB
--
-- 'name', 'getApplicationResponse_name' - The name of the application.
--
-- Minimum length=1. Maximum length=140
--
-- Pattern: \"[a-zA-Z0-9\\\\-]+\";
--
-- 'readmeUrl', 'getApplicationResponse_readmeUrl' - A link to the readme file in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- 'spdxLicenseId', 'getApplicationResponse_spdxLicenseId' - A valid identifier from https:\/\/spdx.org\/licenses\/.
--
-- 'verifiedAuthorUrl', 'getApplicationResponse_verifiedAuthorUrl' - The URL to the public profile of a verified author. This URL is
-- submitted by the author.
--
-- 'version', 'getApplicationResponse_version' - Version information about the application.
--
-- 'httpStatus', 'getApplicationResponse_httpStatus' - The response's http status code.
newGetApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetApplicationResponse
newGetApplicationResponse pHttpStatus_ =
  GetApplicationResponse'
    { applicationId =
        Prelude.Nothing,
      author = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      homePageUrl = Prelude.Nothing,
      isVerifiedAuthor = Prelude.Nothing,
      labels = Prelude.Nothing,
      licenseUrl = Prelude.Nothing,
      name = Prelude.Nothing,
      readmeUrl = Prelude.Nothing,
      spdxLicenseId = Prelude.Nothing,
      verifiedAuthorUrl = Prelude.Nothing,
      version = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The application Amazon Resource Name (ARN).
getApplicationResponse_applicationId :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_applicationId = Lens.lens (\GetApplicationResponse' {applicationId} -> applicationId) (\s@GetApplicationResponse' {} a -> s {applicationId = a} :: GetApplicationResponse)

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
getApplicationResponse_author :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_author = Lens.lens (\GetApplicationResponse' {author} -> author) (\s@GetApplicationResponse' {} a -> s {author = a} :: GetApplicationResponse)

-- | The date and time this resource was created.
getApplicationResponse_creationTime :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_creationTime = Lens.lens (\GetApplicationResponse' {creationTime} -> creationTime) (\s@GetApplicationResponse' {} a -> s {creationTime = a} :: GetApplicationResponse)

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
getApplicationResponse_description :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_description = Lens.lens (\GetApplicationResponse' {description} -> description) (\s@GetApplicationResponse' {} a -> s {description = a} :: GetApplicationResponse)

-- | A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
getApplicationResponse_homePageUrl :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_homePageUrl = Lens.lens (\GetApplicationResponse' {homePageUrl} -> homePageUrl) (\s@GetApplicationResponse' {} a -> s {homePageUrl = a} :: GetApplicationResponse)

-- | Whether the author of this application has been verified. This means
-- means that AWS has made a good faith review, as a reasonable and prudent
-- service provider, of the information provided by the requester and has
-- confirmed that the requester\'s identity is as claimed.
getApplicationResponse_isVerifiedAuthor :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Bool)
getApplicationResponse_isVerifiedAuthor = Lens.lens (\GetApplicationResponse' {isVerifiedAuthor} -> isVerifiedAuthor) (\s@GetApplicationResponse' {} a -> s {isVerifiedAuthor = a} :: GetApplicationResponse)

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
getApplicationResponse_labels :: Lens.Lens' GetApplicationResponse (Prelude.Maybe [Prelude.Text])
getApplicationResponse_labels = Lens.lens (\GetApplicationResponse' {labels} -> labels) (\s@GetApplicationResponse' {} a -> s {labels = a} :: GetApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | A link to a license file of the app that matches the spdxLicenseID value
-- of your application.
--
-- Maximum size 5 MB
getApplicationResponse_licenseUrl :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_licenseUrl = Lens.lens (\GetApplicationResponse' {licenseUrl} -> licenseUrl) (\s@GetApplicationResponse' {} a -> s {licenseUrl = a} :: GetApplicationResponse)

-- | The name of the application.
--
-- Minimum length=1. Maximum length=140
--
-- Pattern: \"[a-zA-Z0-9\\\\-]+\";
getApplicationResponse_name :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_name = Lens.lens (\GetApplicationResponse' {name} -> name) (\s@GetApplicationResponse' {} a -> s {name = a} :: GetApplicationResponse)

-- | A link to the readme file in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
getApplicationResponse_readmeUrl :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_readmeUrl = Lens.lens (\GetApplicationResponse' {readmeUrl} -> readmeUrl) (\s@GetApplicationResponse' {} a -> s {readmeUrl = a} :: GetApplicationResponse)

-- | A valid identifier from https:\/\/spdx.org\/licenses\/.
getApplicationResponse_spdxLicenseId :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_spdxLicenseId = Lens.lens (\GetApplicationResponse' {spdxLicenseId} -> spdxLicenseId) (\s@GetApplicationResponse' {} a -> s {spdxLicenseId = a} :: GetApplicationResponse)

-- | The URL to the public profile of a verified author. This URL is
-- submitted by the author.
getApplicationResponse_verifiedAuthorUrl :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_verifiedAuthorUrl = Lens.lens (\GetApplicationResponse' {verifiedAuthorUrl} -> verifiedAuthorUrl) (\s@GetApplicationResponse' {} a -> s {verifiedAuthorUrl = a} :: GetApplicationResponse)

-- | Version information about the application.
getApplicationResponse_version :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Version)
getApplicationResponse_version = Lens.lens (\GetApplicationResponse' {version} -> version) (\s@GetApplicationResponse' {} a -> s {version = a} :: GetApplicationResponse)

-- | The response's http status code.
getApplicationResponse_httpStatus :: Lens.Lens' GetApplicationResponse Prelude.Int
getApplicationResponse_httpStatus = Lens.lens (\GetApplicationResponse' {httpStatus} -> httpStatus) (\s@GetApplicationResponse' {} a -> s {httpStatus = a} :: GetApplicationResponse)

instance Prelude.NFData GetApplicationResponse where
  rnf GetApplicationResponse' {..} =
    Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf author
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf homePageUrl
      `Prelude.seq` Prelude.rnf isVerifiedAuthor
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf licenseUrl
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf readmeUrl
      `Prelude.seq` Prelude.rnf spdxLicenseId
      `Prelude.seq` Prelude.rnf verifiedAuthorUrl
      `Prelude.seq` Prelude.rnf version
      `Prelude.seq` Prelude.rnf httpStatus
