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
-- Module      : Amazonka.ECRPublic.Types.RepositoryCatalogData
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ECRPublic.Types.RepositoryCatalogData where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The catalog data for a repository. This data is publicly visible in the
-- Amazon ECR Public Gallery.
--
-- /See:/ 'newRepositoryCatalogData' smart constructor.
data RepositoryCatalogData = RepositoryCatalogData'
  { -- | The longform description of the contents of the repository. This text
    -- appears in the repository details on the Amazon ECR Public Gallery.
    aboutText :: Prelude.Maybe Prelude.Text,
    -- | The architecture tags that are associated with the repository.
    --
    -- Only supported operating system tags appear publicly in the Amazon ECR
    -- Public Gallery. For more information, see RepositoryCatalogDataInput.
    architectures :: Prelude.Maybe [Prelude.Text],
    -- | The short description of the repository.
    description :: Prelude.Maybe Prelude.Text,
    -- | The URL containing the logo associated with the repository.
    logoUrl :: Prelude.Maybe Prelude.Text,
    -- | Whether or not the repository is certified by AWS Marketplace.
    marketplaceCertified :: Prelude.Maybe Prelude.Bool,
    -- | The operating system tags that are associated with the repository.
    --
    -- Only supported operating system tags appear publicly in the Amazon ECR
    -- Public Gallery. For more information, see RepositoryCatalogDataInput.
    operatingSystems :: Prelude.Maybe [Prelude.Text],
    -- | The longform usage details of the contents of the repository. The usage
    -- text provides context for users of the repository.
    usageText :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositoryCatalogData' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aboutText', 'repositoryCatalogData_aboutText' - The longform description of the contents of the repository. This text
-- appears in the repository details on the Amazon ECR Public Gallery.
--
-- 'architectures', 'repositoryCatalogData_architectures' - The architecture tags that are associated with the repository.
--
-- Only supported operating system tags appear publicly in the Amazon ECR
-- Public Gallery. For more information, see RepositoryCatalogDataInput.
--
-- 'description', 'repositoryCatalogData_description' - The short description of the repository.
--
-- 'logoUrl', 'repositoryCatalogData_logoUrl' - The URL containing the logo associated with the repository.
--
-- 'marketplaceCertified', 'repositoryCatalogData_marketplaceCertified' - Whether or not the repository is certified by AWS Marketplace.
--
-- 'operatingSystems', 'repositoryCatalogData_operatingSystems' - The operating system tags that are associated with the repository.
--
-- Only supported operating system tags appear publicly in the Amazon ECR
-- Public Gallery. For more information, see RepositoryCatalogDataInput.
--
-- 'usageText', 'repositoryCatalogData_usageText' - The longform usage details of the contents of the repository. The usage
-- text provides context for users of the repository.
newRepositoryCatalogData ::
  RepositoryCatalogData
newRepositoryCatalogData =
  RepositoryCatalogData'
    { aboutText = Prelude.Nothing,
      architectures = Prelude.Nothing,
      description = Prelude.Nothing,
      logoUrl = Prelude.Nothing,
      marketplaceCertified = Prelude.Nothing,
      operatingSystems = Prelude.Nothing,
      usageText = Prelude.Nothing
    }

-- | The longform description of the contents of the repository. This text
-- appears in the repository details on the Amazon ECR Public Gallery.
repositoryCatalogData_aboutText :: Lens.Lens' RepositoryCatalogData (Prelude.Maybe Prelude.Text)
repositoryCatalogData_aboutText = Lens.lens (\RepositoryCatalogData' {aboutText} -> aboutText) (\s@RepositoryCatalogData' {} a -> s {aboutText = a} :: RepositoryCatalogData)

-- | The architecture tags that are associated with the repository.
--
-- Only supported operating system tags appear publicly in the Amazon ECR
-- Public Gallery. For more information, see RepositoryCatalogDataInput.
repositoryCatalogData_architectures :: Lens.Lens' RepositoryCatalogData (Prelude.Maybe [Prelude.Text])
repositoryCatalogData_architectures = Lens.lens (\RepositoryCatalogData' {architectures} -> architectures) (\s@RepositoryCatalogData' {} a -> s {architectures = a} :: RepositoryCatalogData) Prelude.. Lens.mapping Lens.coerced

-- | The short description of the repository.
repositoryCatalogData_description :: Lens.Lens' RepositoryCatalogData (Prelude.Maybe Prelude.Text)
repositoryCatalogData_description = Lens.lens (\RepositoryCatalogData' {description} -> description) (\s@RepositoryCatalogData' {} a -> s {description = a} :: RepositoryCatalogData)

-- | The URL containing the logo associated with the repository.
repositoryCatalogData_logoUrl :: Lens.Lens' RepositoryCatalogData (Prelude.Maybe Prelude.Text)
repositoryCatalogData_logoUrl = Lens.lens (\RepositoryCatalogData' {logoUrl} -> logoUrl) (\s@RepositoryCatalogData' {} a -> s {logoUrl = a} :: RepositoryCatalogData)

-- | Whether or not the repository is certified by AWS Marketplace.
repositoryCatalogData_marketplaceCertified :: Lens.Lens' RepositoryCatalogData (Prelude.Maybe Prelude.Bool)
repositoryCatalogData_marketplaceCertified = Lens.lens (\RepositoryCatalogData' {marketplaceCertified} -> marketplaceCertified) (\s@RepositoryCatalogData' {} a -> s {marketplaceCertified = a} :: RepositoryCatalogData)

-- | The operating system tags that are associated with the repository.
--
-- Only supported operating system tags appear publicly in the Amazon ECR
-- Public Gallery. For more information, see RepositoryCatalogDataInput.
repositoryCatalogData_operatingSystems :: Lens.Lens' RepositoryCatalogData (Prelude.Maybe [Prelude.Text])
repositoryCatalogData_operatingSystems = Lens.lens (\RepositoryCatalogData' {operatingSystems} -> operatingSystems) (\s@RepositoryCatalogData' {} a -> s {operatingSystems = a} :: RepositoryCatalogData) Prelude.. Lens.mapping Lens.coerced

-- | The longform usage details of the contents of the repository. The usage
-- text provides context for users of the repository.
repositoryCatalogData_usageText :: Lens.Lens' RepositoryCatalogData (Prelude.Maybe Prelude.Text)
repositoryCatalogData_usageText = Lens.lens (\RepositoryCatalogData' {usageText} -> usageText) (\s@RepositoryCatalogData' {} a -> s {usageText = a} :: RepositoryCatalogData)

instance Data.FromJSON RepositoryCatalogData where
  parseJSON =
    Data.withObject
      "RepositoryCatalogData"
      ( \x ->
          RepositoryCatalogData'
            Prelude.<$> (x Data..:? "aboutText")
            Prelude.<*> (x Data..:? "architectures" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "description")
            Prelude.<*> (x Data..:? "logoUrl")
            Prelude.<*> (x Data..:? "marketplaceCertified")
            Prelude.<*> ( x Data..:? "operatingSystems"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "usageText")
      )

instance Prelude.Hashable RepositoryCatalogData where
  hashWithSalt _salt RepositoryCatalogData' {..} =
    _salt `Prelude.hashWithSalt` aboutText
      `Prelude.hashWithSalt` architectures
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` logoUrl
      `Prelude.hashWithSalt` marketplaceCertified
      `Prelude.hashWithSalt` operatingSystems
      `Prelude.hashWithSalt` usageText

instance Prelude.NFData RepositoryCatalogData where
  rnf RepositoryCatalogData' {..} =
    Prelude.rnf aboutText
      `Prelude.seq` Prelude.rnf architectures
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf logoUrl
      `Prelude.seq` Prelude.rnf marketplaceCertified
      `Prelude.seq` Prelude.rnf operatingSystems
      `Prelude.seq` Prelude.rnf usageText
