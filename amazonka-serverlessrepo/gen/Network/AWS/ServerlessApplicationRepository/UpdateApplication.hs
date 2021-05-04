{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ServerlessApplicationRepository.UpdateApplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified application.
module Network.AWS.ServerlessApplicationRepository.UpdateApplication
  ( -- * Creating a Request
    UpdateApplication (..),
    newUpdateApplication,

    -- * Request Lenses
    updateApplication_labels,
    updateApplication_author,
    updateApplication_readmeBody,
    updateApplication_homePageUrl,
    updateApplication_readmeUrl,
    updateApplication_description,
    updateApplication_applicationId,

    -- * Destructuring the Response
    UpdateApplicationResponse (..),
    newUpdateApplicationResponse,

    -- * Response Lenses
    updateApplicationResponse_applicationId,
    updateApplicationResponse_creationTime,
    updateApplicationResponse_spdxLicenseId,
    updateApplicationResponse_licenseUrl,
    updateApplicationResponse_verifiedAuthorUrl,
    updateApplicationResponse_labels,
    updateApplicationResponse_author,
    updateApplicationResponse_version,
    updateApplicationResponse_homePageUrl,
    updateApplicationResponse_name,
    updateApplicationResponse_isVerifiedAuthor,
    updateApplicationResponse_readmeUrl,
    updateApplicationResponse_description,
    updateApplicationResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServerlessApplicationRepository.Types

-- | /See:/ 'newUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | Labels to improve discovery of apps in search results.
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
    -- | A text readme file in Markdown language that contains a more detailed
    -- description of the application and how it works.
    --
    -- Maximum size 5 MB
    readmeBody :: Prelude.Maybe Prelude.Text,
    -- | A URL with more information about the application, for example the
    -- location of your GitHub repository for the application.
    homePageUrl :: Prelude.Maybe Prelude.Text,
    -- | A link to the readme file in Markdown language that contains a more
    -- detailed description of the application and how it works.
    --
    -- Maximum size 5 MB
    readmeUrl :: Prelude.Maybe Prelude.Text,
    -- | The description of the application.
    --
    -- Minimum length=1. Maximum length=256
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'labels', 'updateApplication_labels' - Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
--
-- 'author', 'updateApplication_author' - The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
--
-- 'readmeBody', 'updateApplication_readmeBody' - A text readme file in Markdown language that contains a more detailed
-- description of the application and how it works.
--
-- Maximum size 5 MB
--
-- 'homePageUrl', 'updateApplication_homePageUrl' - A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
--
-- 'readmeUrl', 'updateApplication_readmeUrl' - A link to the readme file in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- 'description', 'updateApplication_description' - The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- 'applicationId', 'updateApplication_applicationId' - The Amazon Resource Name (ARN) of the application.
newUpdateApplication ::
  -- | 'applicationId'
  Prelude.Text ->
  UpdateApplication
newUpdateApplication pApplicationId_ =
  UpdateApplication'
    { labels = Prelude.Nothing,
      author = Prelude.Nothing,
      readmeBody = Prelude.Nothing,
      homePageUrl = Prelude.Nothing,
      readmeUrl = Prelude.Nothing,
      description = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
updateApplication_labels :: Lens.Lens' UpdateApplication (Prelude.Maybe [Prelude.Text])
updateApplication_labels = Lens.lens (\UpdateApplication' {labels} -> labels) (\s@UpdateApplication' {} a -> s {labels = a} :: UpdateApplication) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
updateApplication_author :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_author = Lens.lens (\UpdateApplication' {author} -> author) (\s@UpdateApplication' {} a -> s {author = a} :: UpdateApplication)

-- | A text readme file in Markdown language that contains a more detailed
-- description of the application and how it works.
--
-- Maximum size 5 MB
updateApplication_readmeBody :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_readmeBody = Lens.lens (\UpdateApplication' {readmeBody} -> readmeBody) (\s@UpdateApplication' {} a -> s {readmeBody = a} :: UpdateApplication)

-- | A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
updateApplication_homePageUrl :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_homePageUrl = Lens.lens (\UpdateApplication' {homePageUrl} -> homePageUrl) (\s@UpdateApplication' {} a -> s {homePageUrl = a} :: UpdateApplication)

-- | A link to the readme file in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
updateApplication_readmeUrl :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_readmeUrl = Lens.lens (\UpdateApplication' {readmeUrl} -> readmeUrl) (\s@UpdateApplication' {} a -> s {readmeUrl = a} :: UpdateApplication)

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
updateApplication_description :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_description = Lens.lens (\UpdateApplication' {description} -> description) (\s@UpdateApplication' {} a -> s {description = a} :: UpdateApplication)

-- | The Amazon Resource Name (ARN) of the application.
updateApplication_applicationId :: Lens.Lens' UpdateApplication Prelude.Text
updateApplication_applicationId = Lens.lens (\UpdateApplication' {applicationId} -> applicationId) (\s@UpdateApplication' {} a -> s {applicationId = a} :: UpdateApplication)

instance Prelude.AWSRequest UpdateApplication where
  type Rs UpdateApplication = UpdateApplicationResponse
  request = Request.patchJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApplicationResponse'
            Prelude.<$> (x Prelude..?> "applicationId")
            Prelude.<*> (x Prelude..?> "creationTime")
            Prelude.<*> (x Prelude..?> "spdxLicenseId")
            Prelude.<*> (x Prelude..?> "licenseUrl")
            Prelude.<*> (x Prelude..?> "verifiedAuthorUrl")
            Prelude.<*> (x Prelude..?> "labels" Prelude..!@ Prelude.mempty)
            Prelude.<*> (x Prelude..?> "author")
            Prelude.<*> (x Prelude..?> "version")
            Prelude.<*> (x Prelude..?> "homePageUrl")
            Prelude.<*> (x Prelude..?> "name")
            Prelude.<*> (x Prelude..?> "isVerifiedAuthor")
            Prelude.<*> (x Prelude..?> "readmeUrl")
            Prelude.<*> (x Prelude..?> "description")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateApplication

instance Prelude.NFData UpdateApplication

instance Prelude.ToHeaders UpdateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON UpdateApplication where
  toJSON UpdateApplication' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("labels" Prelude..=) Prelude.<$> labels,
            ("author" Prelude..=) Prelude.<$> author,
            ("readmeBody" Prelude..=) Prelude.<$> readmeBody,
            ("homePageUrl" Prelude..=) Prelude.<$> homePageUrl,
            ("readmeUrl" Prelude..=) Prelude.<$> readmeUrl,
            ("description" Prelude..=) Prelude.<$> description
          ]
      )

instance Prelude.ToPath UpdateApplication where
  toPath UpdateApplication' {..} =
    Prelude.mconcat
      ["/applications/", Prelude.toBS applicationId]

instance Prelude.ToQuery UpdateApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse'
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateApplicationResponse_applicationId' - The application Amazon Resource Name (ARN).
--
-- 'creationTime', 'updateApplicationResponse_creationTime' - The date and time this resource was created.
--
-- 'spdxLicenseId', 'updateApplicationResponse_spdxLicenseId' - A valid identifier from https:\/\/spdx.org\/licenses\/.
--
-- 'licenseUrl', 'updateApplicationResponse_licenseUrl' - A link to a license file of the app that matches the spdxLicenseID value
-- of your application.
--
-- Maximum size 5 MB
--
-- 'verifiedAuthorUrl', 'updateApplicationResponse_verifiedAuthorUrl' - The URL to the public profile of a verified author. This URL is
-- submitted by the author.
--
-- 'labels', 'updateApplicationResponse_labels' - Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
--
-- 'author', 'updateApplicationResponse_author' - The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
--
-- 'version', 'updateApplicationResponse_version' - Version information about the application.
--
-- 'homePageUrl', 'updateApplicationResponse_homePageUrl' - A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
--
-- 'name', 'updateApplicationResponse_name' - The name of the application.
--
-- Minimum length=1. Maximum length=140
--
-- Pattern: \"[a-zA-Z0-9\\\\-]+\";
--
-- 'isVerifiedAuthor', 'updateApplicationResponse_isVerifiedAuthor' - Whether the author of this application has been verified. This means
-- means that AWS has made a good faith review, as a reasonable and prudent
-- service provider, of the information provided by the requester and has
-- confirmed that the requester\'s identity is as claimed.
--
-- 'readmeUrl', 'updateApplicationResponse_readmeUrl' - A link to the readme file in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- 'description', 'updateApplicationResponse_description' - The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- 'httpStatus', 'updateApplicationResponse_httpStatus' - The response's http status code.
newUpdateApplicationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateApplicationResponse
newUpdateApplicationResponse pHttpStatus_ =
  UpdateApplicationResponse'
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
updateApplicationResponse_applicationId :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_applicationId = Lens.lens (\UpdateApplicationResponse' {applicationId} -> applicationId) (\s@UpdateApplicationResponse' {} a -> s {applicationId = a} :: UpdateApplicationResponse)

-- | The date and time this resource was created.
updateApplicationResponse_creationTime :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_creationTime = Lens.lens (\UpdateApplicationResponse' {creationTime} -> creationTime) (\s@UpdateApplicationResponse' {} a -> s {creationTime = a} :: UpdateApplicationResponse)

-- | A valid identifier from https:\/\/spdx.org\/licenses\/.
updateApplicationResponse_spdxLicenseId :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_spdxLicenseId = Lens.lens (\UpdateApplicationResponse' {spdxLicenseId} -> spdxLicenseId) (\s@UpdateApplicationResponse' {} a -> s {spdxLicenseId = a} :: UpdateApplicationResponse)

-- | A link to a license file of the app that matches the spdxLicenseID value
-- of your application.
--
-- Maximum size 5 MB
updateApplicationResponse_licenseUrl :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_licenseUrl = Lens.lens (\UpdateApplicationResponse' {licenseUrl} -> licenseUrl) (\s@UpdateApplicationResponse' {} a -> s {licenseUrl = a} :: UpdateApplicationResponse)

-- | The URL to the public profile of a verified author. This URL is
-- submitted by the author.
updateApplicationResponse_verifiedAuthorUrl :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_verifiedAuthorUrl = Lens.lens (\UpdateApplicationResponse' {verifiedAuthorUrl} -> verifiedAuthorUrl) (\s@UpdateApplicationResponse' {} a -> s {verifiedAuthorUrl = a} :: UpdateApplicationResponse)

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
updateApplicationResponse_labels :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe [Prelude.Text])
updateApplicationResponse_labels = Lens.lens (\UpdateApplicationResponse' {labels} -> labels) (\s@UpdateApplicationResponse' {} a -> s {labels = a} :: UpdateApplicationResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
updateApplicationResponse_author :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_author = Lens.lens (\UpdateApplicationResponse' {author} -> author) (\s@UpdateApplicationResponse' {} a -> s {author = a} :: UpdateApplicationResponse)

-- | Version information about the application.
updateApplicationResponse_version :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Version)
updateApplicationResponse_version = Lens.lens (\UpdateApplicationResponse' {version} -> version) (\s@UpdateApplicationResponse' {} a -> s {version = a} :: UpdateApplicationResponse)

-- | A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
updateApplicationResponse_homePageUrl :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_homePageUrl = Lens.lens (\UpdateApplicationResponse' {homePageUrl} -> homePageUrl) (\s@UpdateApplicationResponse' {} a -> s {homePageUrl = a} :: UpdateApplicationResponse)

-- | The name of the application.
--
-- Minimum length=1. Maximum length=140
--
-- Pattern: \"[a-zA-Z0-9\\\\-]+\";
updateApplicationResponse_name :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_name = Lens.lens (\UpdateApplicationResponse' {name} -> name) (\s@UpdateApplicationResponse' {} a -> s {name = a} :: UpdateApplicationResponse)

-- | Whether the author of this application has been verified. This means
-- means that AWS has made a good faith review, as a reasonable and prudent
-- service provider, of the information provided by the requester and has
-- confirmed that the requester\'s identity is as claimed.
updateApplicationResponse_isVerifiedAuthor :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Bool)
updateApplicationResponse_isVerifiedAuthor = Lens.lens (\UpdateApplicationResponse' {isVerifiedAuthor} -> isVerifiedAuthor) (\s@UpdateApplicationResponse' {} a -> s {isVerifiedAuthor = a} :: UpdateApplicationResponse)

-- | A link to the readme file in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
updateApplicationResponse_readmeUrl :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_readmeUrl = Lens.lens (\UpdateApplicationResponse' {readmeUrl} -> readmeUrl) (\s@UpdateApplicationResponse' {} a -> s {readmeUrl = a} :: UpdateApplicationResponse)

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
updateApplicationResponse_description :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_description = Lens.lens (\UpdateApplicationResponse' {description} -> description) (\s@UpdateApplicationResponse' {} a -> s {description = a} :: UpdateApplicationResponse)

-- | The response's http status code.
updateApplicationResponse_httpStatus :: Lens.Lens' UpdateApplicationResponse Prelude.Int
updateApplicationResponse_httpStatus = Lens.lens (\UpdateApplicationResponse' {httpStatus} -> httpStatus) (\s@UpdateApplicationResponse' {} a -> s {httpStatus = a} :: UpdateApplicationResponse)

instance Prelude.NFData UpdateApplicationResponse
