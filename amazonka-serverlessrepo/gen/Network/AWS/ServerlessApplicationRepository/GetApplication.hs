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
-- Module      : Network.AWS.ServerlessApplicationRepository.GetApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets the specified application.
module Network.AWS.ServerlessApplicationRepository.GetApplication
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
    getApplicationResponse_creationTime,
    getApplicationResponse_spdxLicenseId,
    getApplicationResponse_licenseUrl,
    getApplicationResponse_verifiedAuthorUrl,
    getApplicationResponse_labels,
    getApplicationResponse_author,
    getApplicationResponse_version,
    getApplicationResponse_homePageUrl,
    getApplicationResponse_name,
    getApplicationResponse_isVerifiedAuthor,
    getApplicationResponse_readmeUrl,
    getApplicationResponse_description,
    getApplicationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServerlessApplicationRepository.Types

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
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetApplicationResponse'
            Prelude.<$> (x Core..?> "applicationId")
            Prelude.<*> (x Core..?> "creationTime")
            Prelude.<*> (x Core..?> "spdxLicenseId")
            Prelude.<*> (x Core..?> "licenseUrl")
            Prelude.<*> (x Core..?> "verifiedAuthorUrl")
            Prelude.<*> (x Core..?> "labels" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "author")
            Prelude.<*> (x Core..?> "version")
            Prelude.<*> (x Core..?> "homePageUrl")
            Prelude.<*> (x Core..?> "name")
            Prelude.<*> (x Core..?> "isVerifiedAuthor")
            Prelude.<*> (x Core..?> "readmeUrl")
            Prelude.<*> (x Core..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetApplication

instance Prelude.NFData GetApplication

instance Core.ToHeaders GetApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetApplication where
  toPath GetApplication' {..} =
    Prelude.mconcat
      ["/applications/", Core.toBS applicationId]

instance Core.ToQuery GetApplication where
  toQuery GetApplication' {..} =
    Prelude.mconcat
      ["semanticVersion" Core.=: semanticVersion]

-- | /See:/ 'newGetApplicationResponse' smart constructor.
data GetApplicationResponse = GetApplicationResponse'
  { -- | The application Amazon Resource Name (ARN).
    applicationId :: Prelude.Maybe Prelude.Text,
    -- | The date and time this resource was created.
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | A valid identifier from https:\/\/spdx.org\/licenses\/.
    spdxLicenseId :: Prelude.Maybe Prelude.Text,
    -- | A link to a license file of the app that matches the spdxLicenseID value
    -- of your application.
    --
    -- Maximum size 5 MB
    licenseUrl :: Prelude.Maybe Prelude.Text,
    -- | The URL to the public profile of a verified author. This URL is
    -- submitted by the author.
    verifiedAuthorUrl :: Prelude.Maybe Prelude.Text,
    -- | Labels to improve discovery of apps in search results.
    --
    -- Minimum length=1. Maximum length=127. Maximum number of labels: 10
    --
    -- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
    labels :: Prelude.Maybe [Prelude.Text],
    -- | The name of the author publishing the app.
    --
    -- Minimum length=1. Maximum length=127.
    --
    -- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
    author :: Prelude.Maybe Prelude.Text,
    -- | Version information about the application.
    version :: Prelude.Maybe Version,
    -- | A URL with more information about the application, for example the
    -- location of your GitHub repository for the application.
    homePageUrl :: Prelude.Maybe Prelude.Text,
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
    -- | A link to the readme file in Markdown language that contains a more
    -- detailed description of the application and how it works.
    --
    -- Maximum size 5 MB
    readmeUrl :: Prelude.Maybe Prelude.Text,
    -- | The description of the application.
    --
    -- Minimum length=1. Maximum length=256
    description :: Prelude.Maybe Prelude.Text,
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
-- 'creationTime', 'getApplicationResponse_creationTime' - The date and time this resource was created.
--
-- 'spdxLicenseId', 'getApplicationResponse_spdxLicenseId' - A valid identifier from https:\/\/spdx.org\/licenses\/.
--
-- 'licenseUrl', 'getApplicationResponse_licenseUrl' - A link to a license file of the app that matches the spdxLicenseID value
-- of your application.
--
-- Maximum size 5 MB
--
-- 'verifiedAuthorUrl', 'getApplicationResponse_verifiedAuthorUrl' - The URL to the public profile of a verified author. This URL is
-- submitted by the author.
--
-- 'labels', 'getApplicationResponse_labels' - Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
--
-- 'author', 'getApplicationResponse_author' - The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
--
-- 'version', 'getApplicationResponse_version' - Version information about the application.
--
-- 'homePageUrl', 'getApplicationResponse_homePageUrl' - A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
--
-- 'name', 'getApplicationResponse_name' - The name of the application.
--
-- Minimum length=1. Maximum length=140
--
-- Pattern: \"[a-zA-Z0-9\\\\-]+\";
--
-- 'isVerifiedAuthor', 'getApplicationResponse_isVerifiedAuthor' - Whether the author of this application has been verified. This means
-- means that AWS has made a good faith review, as a reasonable and prudent
-- service provider, of the information provided by the requester and has
-- confirmed that the requester\'s identity is as claimed.
--
-- 'readmeUrl', 'getApplicationResponse_readmeUrl' - A link to the readme file in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- 'description', 'getApplicationResponse_description' - The description of the application.
--
-- Minimum length=1. Maximum length=256
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
      creationTime = Prelude.Nothing,
      spdxLicenseId = Prelude.Nothing,
      licenseUrl = Prelude.Nothing,
      verifiedAuthorUrl = Prelude.Nothing,
      labels = Prelude.Nothing,
      author = Prelude.Nothing,
      version = Prelude.Nothing,
      homePageUrl = Prelude.Nothing,
      name = Prelude.Nothing,
      isVerifiedAuthor = Prelude.Nothing,
      readmeUrl = Prelude.Nothing,
      description = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The application Amazon Resource Name (ARN).
getApplicationResponse_applicationId :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_applicationId = Lens.lens (\GetApplicationResponse' {applicationId} -> applicationId) (\s@GetApplicationResponse' {} a -> s {applicationId = a} :: GetApplicationResponse)

-- | The date and time this resource was created.
getApplicationResponse_creationTime :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_creationTime = Lens.lens (\GetApplicationResponse' {creationTime} -> creationTime) (\s@GetApplicationResponse' {} a -> s {creationTime = a} :: GetApplicationResponse)

-- | A valid identifier from https:\/\/spdx.org\/licenses\/.
getApplicationResponse_spdxLicenseId :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_spdxLicenseId = Lens.lens (\GetApplicationResponse' {spdxLicenseId} -> spdxLicenseId) (\s@GetApplicationResponse' {} a -> s {spdxLicenseId = a} :: GetApplicationResponse)

-- | A link to a license file of the app that matches the spdxLicenseID value
-- of your application.
--
-- Maximum size 5 MB
getApplicationResponse_licenseUrl :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_licenseUrl = Lens.lens (\GetApplicationResponse' {licenseUrl} -> licenseUrl) (\s@GetApplicationResponse' {} a -> s {licenseUrl = a} :: GetApplicationResponse)

-- | The URL to the public profile of a verified author. This URL is
-- submitted by the author.
getApplicationResponse_verifiedAuthorUrl :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_verifiedAuthorUrl = Lens.lens (\GetApplicationResponse' {verifiedAuthorUrl} -> verifiedAuthorUrl) (\s@GetApplicationResponse' {} a -> s {verifiedAuthorUrl = a} :: GetApplicationResponse)

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
getApplicationResponse_labels :: Lens.Lens' GetApplicationResponse (Prelude.Maybe [Prelude.Text])
getApplicationResponse_labels = Lens.lens (\GetApplicationResponse' {labels} -> labels) (\s@GetApplicationResponse' {} a -> s {labels = a} :: GetApplicationResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
getApplicationResponse_author :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_author = Lens.lens (\GetApplicationResponse' {author} -> author) (\s@GetApplicationResponse' {} a -> s {author = a} :: GetApplicationResponse)

-- | Version information about the application.
getApplicationResponse_version :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Version)
getApplicationResponse_version = Lens.lens (\GetApplicationResponse' {version} -> version) (\s@GetApplicationResponse' {} a -> s {version = a} :: GetApplicationResponse)

-- | A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
getApplicationResponse_homePageUrl :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_homePageUrl = Lens.lens (\GetApplicationResponse' {homePageUrl} -> homePageUrl) (\s@GetApplicationResponse' {} a -> s {homePageUrl = a} :: GetApplicationResponse)

-- | The name of the application.
--
-- Minimum length=1. Maximum length=140
--
-- Pattern: \"[a-zA-Z0-9\\\\-]+\";
getApplicationResponse_name :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_name = Lens.lens (\GetApplicationResponse' {name} -> name) (\s@GetApplicationResponse' {} a -> s {name = a} :: GetApplicationResponse)

-- | Whether the author of this application has been verified. This means
-- means that AWS has made a good faith review, as a reasonable and prudent
-- service provider, of the information provided by the requester and has
-- confirmed that the requester\'s identity is as claimed.
getApplicationResponse_isVerifiedAuthor :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Bool)
getApplicationResponse_isVerifiedAuthor = Lens.lens (\GetApplicationResponse' {isVerifiedAuthor} -> isVerifiedAuthor) (\s@GetApplicationResponse' {} a -> s {isVerifiedAuthor = a} :: GetApplicationResponse)

-- | A link to the readme file in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
getApplicationResponse_readmeUrl :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_readmeUrl = Lens.lens (\GetApplicationResponse' {readmeUrl} -> readmeUrl) (\s@GetApplicationResponse' {} a -> s {readmeUrl = a} :: GetApplicationResponse)

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
getApplicationResponse_description :: Lens.Lens' GetApplicationResponse (Prelude.Maybe Prelude.Text)
getApplicationResponse_description = Lens.lens (\GetApplicationResponse' {description} -> description) (\s@GetApplicationResponse' {} a -> s {description = a} :: GetApplicationResponse)

-- | The response's http status code.
getApplicationResponse_httpStatus :: Lens.Lens' GetApplicationResponse Prelude.Int
getApplicationResponse_httpStatus = Lens.lens (\GetApplicationResponse' {httpStatus} -> httpStatus) (\s@GetApplicationResponse' {} a -> s {httpStatus = a} :: GetApplicationResponse)

instance Prelude.NFData GetApplicationResponse
