{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ServerlessApplicationRepository.Types.ApplicationSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServerlessApplicationRepository.Types.ApplicationSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Summary of details about the application.
--
-- /See:/ 'newApplicationSummary' smart constructor.
data ApplicationSummary = ApplicationSummary'
  { -- | The date and time this resource was created.
    creationTime :: Prelude.Maybe Prelude.Text,
    -- | A URL with more information about the application, for example the
    -- location of your GitHub repository for the application.
    homePageUrl :: Prelude.Maybe Prelude.Text,
    -- | Labels to improve discovery of apps in search results.
    --
    -- Minimum length=1. Maximum length=127. Maximum number of labels: 10
    --
    -- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
    labels :: Prelude.Maybe [Prelude.Text],
    -- | A valid identifier from <https://spdx.org/licenses/>.
    spdxLicenseId :: Prelude.Maybe Prelude.Text,
    -- | The description of the application.
    --
    -- Minimum length=1. Maximum length=256
    description :: Prelude.Text,
    -- | The name of the author publishing the app.
    --
    -- Minimum length=1. Maximum length=127.
    --
    -- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
    author :: Prelude.Text,
    -- | The application Amazon Resource Name (ARN).
    applicationId :: Prelude.Text,
    -- | The name of the application.
    --
    -- Minimum length=1. Maximum length=140
    --
    -- Pattern: \"[a-zA-Z0-9\\\\-]+\";
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ApplicationSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'applicationSummary_creationTime' - The date and time this resource was created.
--
-- 'homePageUrl', 'applicationSummary_homePageUrl' - A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
--
-- 'labels', 'applicationSummary_labels' - Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
--
-- 'spdxLicenseId', 'applicationSummary_spdxLicenseId' - A valid identifier from <https://spdx.org/licenses/>.
--
-- 'description', 'applicationSummary_description' - The description of the application.
--
-- Minimum length=1. Maximum length=256
--
-- 'author', 'applicationSummary_author' - The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
--
-- 'applicationId', 'applicationSummary_applicationId' - The application Amazon Resource Name (ARN).
--
-- 'name', 'applicationSummary_name' - The name of the application.
--
-- Minimum length=1. Maximum length=140
--
-- Pattern: \"[a-zA-Z0-9\\\\-]+\";
newApplicationSummary ::
  -- | 'description'
  Prelude.Text ->
  -- | 'author'
  Prelude.Text ->
  -- | 'applicationId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  ApplicationSummary
newApplicationSummary
  pDescription_
  pAuthor_
  pApplicationId_
  pName_ =
    ApplicationSummary'
      { creationTime = Prelude.Nothing,
        homePageUrl = Prelude.Nothing,
        labels = Prelude.Nothing,
        spdxLicenseId = Prelude.Nothing,
        description = pDescription_,
        author = pAuthor_,
        applicationId = pApplicationId_,
        name = pName_
      }

-- | The date and time this resource was created.
applicationSummary_creationTime :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Text)
applicationSummary_creationTime = Lens.lens (\ApplicationSummary' {creationTime} -> creationTime) (\s@ApplicationSummary' {} a -> s {creationTime = a} :: ApplicationSummary)

-- | A URL with more information about the application, for example the
-- location of your GitHub repository for the application.
applicationSummary_homePageUrl :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Text)
applicationSummary_homePageUrl = Lens.lens (\ApplicationSummary' {homePageUrl} -> homePageUrl) (\s@ApplicationSummary' {} a -> s {homePageUrl = a} :: ApplicationSummary)

-- | Labels to improve discovery of apps in search results.
--
-- Minimum length=1. Maximum length=127. Maximum number of labels: 10
--
-- Pattern: \"^[a-zA-Z0-9+\\\\-_:\\\\\/\@]+$\";
applicationSummary_labels :: Lens.Lens' ApplicationSummary (Prelude.Maybe [Prelude.Text])
applicationSummary_labels = Lens.lens (\ApplicationSummary' {labels} -> labels) (\s@ApplicationSummary' {} a -> s {labels = a} :: ApplicationSummary) Prelude.. Lens.mapping Lens.coerced

-- | A valid identifier from <https://spdx.org/licenses/>.
applicationSummary_spdxLicenseId :: Lens.Lens' ApplicationSummary (Prelude.Maybe Prelude.Text)
applicationSummary_spdxLicenseId = Lens.lens (\ApplicationSummary' {spdxLicenseId} -> spdxLicenseId) (\s@ApplicationSummary' {} a -> s {spdxLicenseId = a} :: ApplicationSummary)

-- | The description of the application.
--
-- Minimum length=1. Maximum length=256
applicationSummary_description :: Lens.Lens' ApplicationSummary Prelude.Text
applicationSummary_description = Lens.lens (\ApplicationSummary' {description} -> description) (\s@ApplicationSummary' {} a -> s {description = a} :: ApplicationSummary)

-- | The name of the author publishing the app.
--
-- Minimum length=1. Maximum length=127.
--
-- Pattern \"^[a-z0-9](([a-z0-9]|-(?!-))*[a-z0-9])?$\";
applicationSummary_author :: Lens.Lens' ApplicationSummary Prelude.Text
applicationSummary_author = Lens.lens (\ApplicationSummary' {author} -> author) (\s@ApplicationSummary' {} a -> s {author = a} :: ApplicationSummary)

-- | The application Amazon Resource Name (ARN).
applicationSummary_applicationId :: Lens.Lens' ApplicationSummary Prelude.Text
applicationSummary_applicationId = Lens.lens (\ApplicationSummary' {applicationId} -> applicationId) (\s@ApplicationSummary' {} a -> s {applicationId = a} :: ApplicationSummary)

-- | The name of the application.
--
-- Minimum length=1. Maximum length=140
--
-- Pattern: \"[a-zA-Z0-9\\\\-]+\";
applicationSummary_name :: Lens.Lens' ApplicationSummary Prelude.Text
applicationSummary_name = Lens.lens (\ApplicationSummary' {name} -> name) (\s@ApplicationSummary' {} a -> s {name = a} :: ApplicationSummary)

instance Data.FromJSON ApplicationSummary where
  parseJSON =
    Data.withObject
      "ApplicationSummary"
      ( \x ->
          ApplicationSummary'
            Prelude.<$> (x Data..:? "creationTime")
            Prelude.<*> (x Data..:? "homePageUrl")
            Prelude.<*> (x Data..:? "labels" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "spdxLicenseId")
            Prelude.<*> (x Data..: "description")
            Prelude.<*> (x Data..: "author")
            Prelude.<*> (x Data..: "applicationId")
            Prelude.<*> (x Data..: "name")
      )

instance Prelude.Hashable ApplicationSummary where
  hashWithSalt _salt ApplicationSummary' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` homePageUrl
      `Prelude.hashWithSalt` labels
      `Prelude.hashWithSalt` spdxLicenseId
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` author
      `Prelude.hashWithSalt` applicationId
      `Prelude.hashWithSalt` name

instance Prelude.NFData ApplicationSummary where
  rnf ApplicationSummary' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf homePageUrl
      `Prelude.seq` Prelude.rnf labels
      `Prelude.seq` Prelude.rnf spdxLicenseId
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf author
      `Prelude.seq` Prelude.rnf applicationId
      `Prelude.seq` Prelude.rnf name
