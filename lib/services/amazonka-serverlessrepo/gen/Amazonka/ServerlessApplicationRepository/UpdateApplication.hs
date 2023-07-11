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
-- Module      : Amazonka.ServerlessApplicationRepository.UpdateApplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified application.
module Amazonka.ServerlessApplicationRepository.UpdateApplication
  ( -- * Creating a Request
    UpdateApplication (..),
    newUpdateApplication,

    -- * Request Lenses
    updateApplication_author,
    updateApplication_description,
    updateApplication_homePageUrl,
    updateApplication_labels,
    updateApplication_readmeBody,
    updateApplication_readmeUrl,
    updateApplication_applicationId,

    -- * Destructuring the Response
    UpdateApplicationResponse (..),
    newUpdateApplicationResponse,

    -- * Response Lenses
    updateApplicationResponse_applicationId,
    updateApplicationResponse_author,
    updateApplicationResponse_creationTime,
    updateApplicationResponse_description,
    updateApplicationResponse_homePageUrl,
    updateApplicationResponse_isVerifiedAuthor,
    updateApplicationResponse_labels,
    updateApplicationResponse_licenseUrl,
    updateApplicationResponse_name,
    updateApplicationResponse_readmeUrl,
    updateApplicationResponse_spdxLicenseId,
    updateApplicationResponse_verifiedAuthorUrl,
    updateApplicationResponse_version,
    updateApplicationResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServerlessApplicationRepository.Types

-- | /See:/ 'newUpdateApplication' smart constructor.
data UpdateApplication = UpdateApplication'
  { -- | The name of the author publishing the app.
    --
    -- Minimum length=1. Maximum length=127.
    --
    -- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
    author :: Prelude.Maybe Prelude.Text,
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
    -- | A text readme file in Markdown language that contains a more detailed
    -- description of the application and how it works.
    --
    -- Maximum size 5 MB
    readmeBody :: Prelude.Maybe Prelude.Text,
    -- | A link to the readme file in Markdown language that contains a more
    -- detailed description of the application and how it works.
    --
    -- Maximum size 5 MB
    readmeUrl :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the application.
    applicationId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateApplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'author', 'updateApplication_author' - The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
--
-- 'description', 'updateApplication_description' - The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- 'homePageUrl', 'updateApplication_homePageUrl' - A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
--
-- 'labels', 'updateApplication_labels' - Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
--
-- 'readmeBody', 'updateApplication_readmeBody' - A text readme file in Markdown language that contains a more detailed
-- description of the application and how it works.
--
-- Maximum size 5 MB
--
-- 'readmeUrl', 'updateApplication_readmeUrl' - A link to the readme file in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- 'applicationId', 'updateApplication_applicationId' - The Amazon Resource Name (ARN) of the application.
newUpdateApplication ::
  -- | 'applicationId'
  Prelude.Text ->
  UpdateApplication
newUpdateApplication pApplicationId_ =
  UpdateApplication'
    { author = Prelude.Nothing,
      description = Prelude.Nothing,
      homePageUrl = Prelude.Nothing,
      labels = Prelude.Nothing,
      readmeBody = Prelude.Nothing,
      readmeUrl = Prelude.Nothing,
      applicationId = pApplicationId_
    }

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
updateApplication_author :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_author = Lens.lens (\UpdateApplication' {author} -> author) (\s@UpdateApplication' {} a -> s {author = a} :: UpdateApplication)

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
updateApplication_description :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_description = Lens.lens (\UpdateApplication' {description} -> description) (\s@UpdateApplication' {} a -> s {description = a} :: UpdateApplication)

-- | A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
updateApplication_homePageUrl :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_homePageUrl = Lens.lens (\UpdateApplication' {homePageUrl} -> homePageUrl) (\s@UpdateApplication' {} a -> s {homePageUrl = a} :: UpdateApplication)

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
updateApplication_labels :: Lens.Lens' UpdateApplication (Prelude.Maybe [Prelude.Text])
updateApplication_labels = Lens.lens (\UpdateApplication' {labels} -> labels) (\s@UpdateApplication' {} a -> s {labels = a} :: UpdateApplication) Prelude.. Lens.mapping Lens.coerced

-- | A text readme file in Markdown language that contains a more detailed
-- description of the application and how it works.
--
-- Maximum size 5 MB
updateApplication_readmeBody :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_readmeBody = Lens.lens (\UpdateApplication' {readmeBody} -> readmeBody) (\s@UpdateApplication' {} a -> s {readmeBody = a} :: UpdateApplication)

-- | A link to the readme file in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
updateApplication_readmeUrl :: Lens.Lens' UpdateApplication (Prelude.Maybe Prelude.Text)
updateApplication_readmeUrl = Lens.lens (\UpdateApplication' {readmeUrl} -> readmeUrl) (\s@UpdateApplication' {} a -> s {readmeUrl = a} :: UpdateApplication)

-- | The Amazon Resource Name (ARN) of the application.
updateApplication_applicationId :: Lens.Lens' UpdateApplication Prelude.Text
updateApplication_applicationId = Lens.lens (\UpdateApplication' {applicationId} -> applicationId) (\s@UpdateApplication' {} a -> s {applicationId = a} :: UpdateApplication)

instance Core.AWSRequest UpdateApplication where
  type
    AWSResponse UpdateApplication =
      UpdateApplicationResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateApplicationResponse'
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

instance Prelude.Hashable UpdateApplication where
  hashWithSalt _salt UpdateApplication' {..} =
    _salt
      `Prelude.hashWithSalt` author
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` homePageUrl
      `Prelude.hashWithSalt` labels
      `Prelude.hashWithSalt` readmeBody
      `Prelude.hashWithSalt` readmeUrl
      `Prelude.hashWithSalt` applicationId

instance Prelude.NFData UpdateApplication where
  rnf UpdateApplication' {..} =
    Prelude.rnf author
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf homePageUrl
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf readmeBody
      `Prelude.seq` Prelude.rnf readmeUrl
      `Prelude.seq` Prelude.rnf applicationId

instance Data.ToHeaders UpdateApplication where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateApplication where
  toJSON UpdateApplication' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("author" Data..=) Prelude.<$> author,
            ("description" Data..=) Prelude.<$> description,
            ("homePageUrl" Data..=) Prelude.<$> homePageUrl,
            ("labels" Data..=) Prelude.<$> labels,
            ("readmeBody" Data..=) Prelude.<$> readmeBody,
            ("readmeUrl" Data..=) Prelude.<$> readmeUrl
          ]
      )

instance Data.ToPath UpdateApplication where
  toPath UpdateApplication' {..} =
    Prelude.mconcat
      ["/applications/", Data.toBS applicationId]

instance Data.ToQuery UpdateApplication where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateApplicationResponse' smart constructor.
data UpdateApplicationResponse = UpdateApplicationResponse'
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
-- Create a value of 'UpdateApplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'applicationId', 'updateApplicationResponse_applicationId' - The application Amazon Resource Name (ARN).
--
-- 'author', 'updateApplicationResponse_author' - The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
--
-- 'creationTime', 'updateApplicationResponse_creationTime' - The date and time this resource was created.
--
-- 'description', 'updateApplicationResponse_description' - The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- 'homePageUrl', 'updateApplicationResponse_homePageUrl' - A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
--
-- 'isVerifiedAuthor', 'updateApplicationResponse_isVerifiedAuthor' - Whether the author of this application has been verified. This means
-- means that AWS has made a good faith review, as a reasonable and prudent
-- service provider, of the information provided by the requester and has
-- confirmed that the requester\'s identity is as claimed.
--
-- 'labels', 'updateApplicationResponse_labels' - Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
--
-- 'licenseUrl', 'updateApplicationResponse_licenseUrl' - A link to a license file of the app that matches the spdxLicenseID value
-- of your application.
--
-- Maximum size 5 MB
--
-- 'name', 'updateApplicationResponse_name' - The name of the application.
--
-- Minimum length=1. Maximum length=140
--
-- Pattern: \"[a-zA-Z0-9\\\\-]+\";
--
-- 'readmeUrl', 'updateApplicationResponse_readmeUrl' - A link to the readme file in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
--
-- 'spdxLicenseId', 'updateApplicationResponse_spdxLicenseId' - A valid identifier from https:\/\/spdx.org\/licenses\/.
--
-- 'verifiedAuthorUrl', 'updateApplicationResponse_verifiedAuthorUrl' - The URL to the public profile of a verified author. This URL is
-- submitted by the author.
--
-- 'version', 'updateApplicationResponse_version' - Version information about the application.
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
updateApplicationResponse_applicationId :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_applicationId = Lens.lens (\UpdateApplicationResponse' {applicationId} -> applicationId) (\s@UpdateApplicationResponse' {} a -> s {applicationId = a} :: UpdateApplicationResponse)

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
updateApplicationResponse_author :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_author = Lens.lens (\UpdateApplicationResponse' {author} -> author) (\s@UpdateApplicationResponse' {} a -> s {author = a} :: UpdateApplicationResponse)

-- | The date and time this resource was created.
updateApplicationResponse_creationTime :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_creationTime = Lens.lens (\UpdateApplicationResponse' {creationTime} -> creationTime) (\s@UpdateApplicationResponse' {} a -> s {creationTime = a} :: UpdateApplicationResponse)

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
updateApplicationResponse_description :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_description = Lens.lens (\UpdateApplicationResponse' {description} -> description) (\s@UpdateApplicationResponse' {} a -> s {description = a} :: UpdateApplicationResponse)

-- | A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
updateApplicationResponse_homePageUrl :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_homePageUrl = Lens.lens (\UpdateApplicationResponse' {homePageUrl} -> homePageUrl) (\s@UpdateApplicationResponse' {} a -> s {homePageUrl = a} :: UpdateApplicationResponse)

-- | Whether the author of this application has been verified. This means
-- means that AWS has made a good faith review, as a reasonable and prudent
-- service provider, of the information provided by the requester and has
-- confirmed that the requester\'s identity is as claimed.
updateApplicationResponse_isVerifiedAuthor :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Bool)
updateApplicationResponse_isVerifiedAuthor = Lens.lens (\UpdateApplicationResponse' {isVerifiedAuthor} -> isVerifiedAuthor) (\s@UpdateApplicationResponse' {} a -> s {isVerifiedAuthor = a} :: UpdateApplicationResponse)

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
updateApplicationResponse_labels :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe [Prelude.Text])
updateApplicationResponse_labels = Lens.lens (\UpdateApplicationResponse' {labels} -> labels) (\s@UpdateApplicationResponse' {} a -> s {labels = a} :: UpdateApplicationResponse) Prelude.. Lens.mapping Lens.coerced

-- | A link to a license file of the app that matches the spdxLicenseID value
-- of your application.
--
-- Maximum size 5 MB
updateApplicationResponse_licenseUrl :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_licenseUrl = Lens.lens (\UpdateApplicationResponse' {licenseUrl} -> licenseUrl) (\s@UpdateApplicationResponse' {} a -> s {licenseUrl = a} :: UpdateApplicationResponse)

-- | The name of the application.
--
-- Minimum length=1. Maximum length=140
--
-- Pattern: \"[a-zA-Z0-9\\\\-]+\";
updateApplicationResponse_name :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_name = Lens.lens (\UpdateApplicationResponse' {name} -> name) (\s@UpdateApplicationResponse' {} a -> s {name = a} :: UpdateApplicationResponse)

-- | A link to the readme file in Markdown language that contains a more
-- detailed description of the application and how it works.
--
-- Maximum size 5 MB
updateApplicationResponse_readmeUrl :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_readmeUrl = Lens.lens (\UpdateApplicationResponse' {readmeUrl} -> readmeUrl) (\s@UpdateApplicationResponse' {} a -> s {readmeUrl = a} :: UpdateApplicationResponse)

-- | A valid identifier from https:\/\/spdx.org\/licenses\/.
updateApplicationResponse_spdxLicenseId :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_spdxLicenseId = Lens.lens (\UpdateApplicationResponse' {spdxLicenseId} -> spdxLicenseId) (\s@UpdateApplicationResponse' {} a -> s {spdxLicenseId = a} :: UpdateApplicationResponse)

-- | The URL to the public profile of a verified author. This URL is
-- submitted by the author.
updateApplicationResponse_verifiedAuthorUrl :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Prelude.Text)
updateApplicationResponse_verifiedAuthorUrl = Lens.lens (\UpdateApplicationResponse' {verifiedAuthorUrl} -> verifiedAuthorUrl) (\s@UpdateApplicationResponse' {} a -> s {verifiedAuthorUrl = a} :: UpdateApplicationResponse)

-- | Version information about the application.
updateApplicationResponse_version :: Lens.Lens' UpdateApplicationResponse (Prelude.Maybe Version)
updateApplicationResponse_version = Lens.lens (\UpdateApplicationResponse' {version} -> version) (\s@UpdateApplicationResponse' {} a -> s {version = a} :: UpdateApplicationResponse)

-- | The response's http status code.
updateApplicationResponse_httpStatus :: Lens.Lens' UpdateApplicationResponse Prelude.Int
updateApplicationResponse_httpStatus = Lens.lens (\UpdateApplicationResponse' {httpStatus} -> httpStatus) (\s@UpdateApplicationResponse' {} a -> s {httpStatus = a} :: UpdateApplicationResponse)

instance Prelude.NFData UpdateApplicationResponse where
  rnf UpdateApplicationResponse' {..} =
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
