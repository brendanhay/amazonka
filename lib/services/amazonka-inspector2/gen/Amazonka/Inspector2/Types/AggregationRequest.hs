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
-- Module      : Amazonka.Inspector2.Types.AggregationRequest
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.AggregationRequest where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.AccountAggregation
import Amazonka.Inspector2.Types.AmiAggregation
import Amazonka.Inspector2.Types.AwsEcrContainerAggregation
import Amazonka.Inspector2.Types.Ec2InstanceAggregation
import Amazonka.Inspector2.Types.FindingTypeAggregation
import Amazonka.Inspector2.Types.ImageLayerAggregation
import Amazonka.Inspector2.Types.PackageAggregation
import Amazonka.Inspector2.Types.RepositoryAggregation
import Amazonka.Inspector2.Types.TitleAggregation
import qualified Amazonka.Prelude as Prelude

-- | Contains details about an aggregation request.
--
-- /See:/ 'newAggregationRequest' smart constructor.
data AggregationRequest = AggregationRequest'
  { -- | An object that contains details about an aggregation request based on
    -- Amazon ECR repositories.
    repositoryAggregation :: Prelude.Maybe RepositoryAggregation,
    -- | An object that contains details about an aggregation request based on
    -- container image layers.
    imageLayerAggregation :: Prelude.Maybe ImageLayerAggregation,
    -- | An object that contains details about an aggregation request based on
    -- Amazon Web Services account IDs.
    accountAggregation :: Prelude.Maybe AccountAggregation,
    -- | An object that contains details about an aggregation request based on
    -- Amazon ECR container images.
    awsEcrContainerAggregation :: Prelude.Maybe AwsEcrContainerAggregation,
    -- | An object that contains details about an aggregation request based on
    -- Amazon EC2 instances.
    ec2InstanceAggregation :: Prelude.Maybe Ec2InstanceAggregation,
    -- | An object that contains details about an aggregation request based on
    -- finding types.
    findingTypeAggregation :: Prelude.Maybe FindingTypeAggregation,
    -- | An object that contains details about an aggregation request based on
    -- operating system package type.
    packageAggregation :: Prelude.Maybe PackageAggregation,
    -- | An object that contains details about an aggregation request based on
    -- finding title.
    titleAggregation :: Prelude.Maybe TitleAggregation,
    -- | An object that contains details about an aggregation request based on
    -- Amazon Machine Images (AMIs).
    amiAggregation :: Prelude.Maybe AmiAggregation
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AggregationRequest' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'repositoryAggregation', 'aggregationRequest_repositoryAggregation' - An object that contains details about an aggregation request based on
-- Amazon ECR repositories.
--
-- 'imageLayerAggregation', 'aggregationRequest_imageLayerAggregation' - An object that contains details about an aggregation request based on
-- container image layers.
--
-- 'accountAggregation', 'aggregationRequest_accountAggregation' - An object that contains details about an aggregation request based on
-- Amazon Web Services account IDs.
--
-- 'awsEcrContainerAggregation', 'aggregationRequest_awsEcrContainerAggregation' - An object that contains details about an aggregation request based on
-- Amazon ECR container images.
--
-- 'ec2InstanceAggregation', 'aggregationRequest_ec2InstanceAggregation' - An object that contains details about an aggregation request based on
-- Amazon EC2 instances.
--
-- 'findingTypeAggregation', 'aggregationRequest_findingTypeAggregation' - An object that contains details about an aggregation request based on
-- finding types.
--
-- 'packageAggregation', 'aggregationRequest_packageAggregation' - An object that contains details about an aggregation request based on
-- operating system package type.
--
-- 'titleAggregation', 'aggregationRequest_titleAggregation' - An object that contains details about an aggregation request based on
-- finding title.
--
-- 'amiAggregation', 'aggregationRequest_amiAggregation' - An object that contains details about an aggregation request based on
-- Amazon Machine Images (AMIs).
newAggregationRequest ::
  AggregationRequest
newAggregationRequest =
  AggregationRequest'
    { repositoryAggregation =
        Prelude.Nothing,
      imageLayerAggregation = Prelude.Nothing,
      accountAggregation = Prelude.Nothing,
      awsEcrContainerAggregation = Prelude.Nothing,
      ec2InstanceAggregation = Prelude.Nothing,
      findingTypeAggregation = Prelude.Nothing,
      packageAggregation = Prelude.Nothing,
      titleAggregation = Prelude.Nothing,
      amiAggregation = Prelude.Nothing
    }

-- | An object that contains details about an aggregation request based on
-- Amazon ECR repositories.
aggregationRequest_repositoryAggregation :: Lens.Lens' AggregationRequest (Prelude.Maybe RepositoryAggregation)
aggregationRequest_repositoryAggregation = Lens.lens (\AggregationRequest' {repositoryAggregation} -> repositoryAggregation) (\s@AggregationRequest' {} a -> s {repositoryAggregation = a} :: AggregationRequest)

-- | An object that contains details about an aggregation request based on
-- container image layers.
aggregationRequest_imageLayerAggregation :: Lens.Lens' AggregationRequest (Prelude.Maybe ImageLayerAggregation)
aggregationRequest_imageLayerAggregation = Lens.lens (\AggregationRequest' {imageLayerAggregation} -> imageLayerAggregation) (\s@AggregationRequest' {} a -> s {imageLayerAggregation = a} :: AggregationRequest)

-- | An object that contains details about an aggregation request based on
-- Amazon Web Services account IDs.
aggregationRequest_accountAggregation :: Lens.Lens' AggregationRequest (Prelude.Maybe AccountAggregation)
aggregationRequest_accountAggregation = Lens.lens (\AggregationRequest' {accountAggregation} -> accountAggregation) (\s@AggregationRequest' {} a -> s {accountAggregation = a} :: AggregationRequest)

-- | An object that contains details about an aggregation request based on
-- Amazon ECR container images.
aggregationRequest_awsEcrContainerAggregation :: Lens.Lens' AggregationRequest (Prelude.Maybe AwsEcrContainerAggregation)
aggregationRequest_awsEcrContainerAggregation = Lens.lens (\AggregationRequest' {awsEcrContainerAggregation} -> awsEcrContainerAggregation) (\s@AggregationRequest' {} a -> s {awsEcrContainerAggregation = a} :: AggregationRequest)

-- | An object that contains details about an aggregation request based on
-- Amazon EC2 instances.
aggregationRequest_ec2InstanceAggregation :: Lens.Lens' AggregationRequest (Prelude.Maybe Ec2InstanceAggregation)
aggregationRequest_ec2InstanceAggregation = Lens.lens (\AggregationRequest' {ec2InstanceAggregation} -> ec2InstanceAggregation) (\s@AggregationRequest' {} a -> s {ec2InstanceAggregation = a} :: AggregationRequest)

-- | An object that contains details about an aggregation request based on
-- finding types.
aggregationRequest_findingTypeAggregation :: Lens.Lens' AggregationRequest (Prelude.Maybe FindingTypeAggregation)
aggregationRequest_findingTypeAggregation = Lens.lens (\AggregationRequest' {findingTypeAggregation} -> findingTypeAggregation) (\s@AggregationRequest' {} a -> s {findingTypeAggregation = a} :: AggregationRequest)

-- | An object that contains details about an aggregation request based on
-- operating system package type.
aggregationRequest_packageAggregation :: Lens.Lens' AggregationRequest (Prelude.Maybe PackageAggregation)
aggregationRequest_packageAggregation = Lens.lens (\AggregationRequest' {packageAggregation} -> packageAggregation) (\s@AggregationRequest' {} a -> s {packageAggregation = a} :: AggregationRequest)

-- | An object that contains details about an aggregation request based on
-- finding title.
aggregationRequest_titleAggregation :: Lens.Lens' AggregationRequest (Prelude.Maybe TitleAggregation)
aggregationRequest_titleAggregation = Lens.lens (\AggregationRequest' {titleAggregation} -> titleAggregation) (\s@AggregationRequest' {} a -> s {titleAggregation = a} :: AggregationRequest)

-- | An object that contains details about an aggregation request based on
-- Amazon Machine Images (AMIs).
aggregationRequest_amiAggregation :: Lens.Lens' AggregationRequest (Prelude.Maybe AmiAggregation)
aggregationRequest_amiAggregation = Lens.lens (\AggregationRequest' {amiAggregation} -> amiAggregation) (\s@AggregationRequest' {} a -> s {amiAggregation = a} :: AggregationRequest)

instance Prelude.Hashable AggregationRequest where
  hashWithSalt _salt AggregationRequest' {..} =
    _salt `Prelude.hashWithSalt` repositoryAggregation
      `Prelude.hashWithSalt` imageLayerAggregation
      `Prelude.hashWithSalt` accountAggregation
      `Prelude.hashWithSalt` awsEcrContainerAggregation
      `Prelude.hashWithSalt` ec2InstanceAggregation
      `Prelude.hashWithSalt` findingTypeAggregation
      `Prelude.hashWithSalt` packageAggregation
      `Prelude.hashWithSalt` titleAggregation
      `Prelude.hashWithSalt` amiAggregation

instance Prelude.NFData AggregationRequest where
  rnf AggregationRequest' {..} =
    Prelude.rnf repositoryAggregation
      `Prelude.seq` Prelude.rnf imageLayerAggregation
      `Prelude.seq` Prelude.rnf accountAggregation
      `Prelude.seq` Prelude.rnf awsEcrContainerAggregation
      `Prelude.seq` Prelude.rnf ec2InstanceAggregation
      `Prelude.seq` Prelude.rnf findingTypeAggregation
      `Prelude.seq` Prelude.rnf packageAggregation
      `Prelude.seq` Prelude.rnf titleAggregation
      `Prelude.seq` Prelude.rnf amiAggregation

instance Data.ToJSON AggregationRequest where
  toJSON AggregationRequest' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("repositoryAggregation" Data..=)
              Prelude.<$> repositoryAggregation,
            ("imageLayerAggregation" Data..=)
              Prelude.<$> imageLayerAggregation,
            ("accountAggregation" Data..=)
              Prelude.<$> accountAggregation,
            ("awsEcrContainerAggregation" Data..=)
              Prelude.<$> awsEcrContainerAggregation,
            ("ec2InstanceAggregation" Data..=)
              Prelude.<$> ec2InstanceAggregation,
            ("findingTypeAggregation" Data..=)
              Prelude.<$> findingTypeAggregation,
            ("packageAggregation" Data..=)
              Prelude.<$> packageAggregation,
            ("titleAggregation" Data..=)
              Prelude.<$> titleAggregation,
            ("amiAggregation" Data..=)
              Prelude.<$> amiAggregation
          ]
      )
