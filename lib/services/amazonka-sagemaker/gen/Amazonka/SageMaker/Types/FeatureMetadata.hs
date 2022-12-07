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
-- Module      : Amazonka.SageMaker.Types.FeatureMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SageMaker.Types.FeatureMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SageMaker.Types.FeatureParameter
import Amazonka.SageMaker.Types.FeatureType

-- | The metadata for a feature. It can either be metadata that you specify,
-- or metadata that is updated automatically.
--
-- /See:/ 'newFeatureMetadata' smart constructor.
data FeatureMetadata = FeatureMetadata'
  { -- | The data type of the feature.
    featureType :: Prelude.Maybe FeatureType,
    -- | The name of feature.
    featureName :: Prelude.Maybe Prelude.Text,
    -- | An optional description that you specify to better describe the feature.
    description :: Prelude.Maybe Prelude.Text,
    -- | A timestamp indicating when the feature was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the feature group containing the feature.
    featureGroupName :: Prelude.Maybe Prelude.Text,
    -- | A timestamp indicating when the feature was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Number (ARN) of the feature group.
    featureGroupArn :: Prelude.Maybe Prelude.Text,
    -- | Optional key-value pairs that you specify to better describe the
    -- feature.
    parameters :: Prelude.Maybe [FeatureParameter]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FeatureMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureType', 'featureMetadata_featureType' - The data type of the feature.
--
-- 'featureName', 'featureMetadata_featureName' - The name of feature.
--
-- 'description', 'featureMetadata_description' - An optional description that you specify to better describe the feature.
--
-- 'lastModifiedTime', 'featureMetadata_lastModifiedTime' - A timestamp indicating when the feature was last modified.
--
-- 'featureGroupName', 'featureMetadata_featureGroupName' - The name of the feature group containing the feature.
--
-- 'creationTime', 'featureMetadata_creationTime' - A timestamp indicating when the feature was created.
--
-- 'featureGroupArn', 'featureMetadata_featureGroupArn' - The Amazon Resource Number (ARN) of the feature group.
--
-- 'parameters', 'featureMetadata_parameters' - Optional key-value pairs that you specify to better describe the
-- feature.
newFeatureMetadata ::
  FeatureMetadata
newFeatureMetadata =
  FeatureMetadata'
    { featureType = Prelude.Nothing,
      featureName = Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      featureGroupName = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      featureGroupArn = Prelude.Nothing,
      parameters = Prelude.Nothing
    }

-- | The data type of the feature.
featureMetadata_featureType :: Lens.Lens' FeatureMetadata (Prelude.Maybe FeatureType)
featureMetadata_featureType = Lens.lens (\FeatureMetadata' {featureType} -> featureType) (\s@FeatureMetadata' {} a -> s {featureType = a} :: FeatureMetadata)

-- | The name of feature.
featureMetadata_featureName :: Lens.Lens' FeatureMetadata (Prelude.Maybe Prelude.Text)
featureMetadata_featureName = Lens.lens (\FeatureMetadata' {featureName} -> featureName) (\s@FeatureMetadata' {} a -> s {featureName = a} :: FeatureMetadata)

-- | An optional description that you specify to better describe the feature.
featureMetadata_description :: Lens.Lens' FeatureMetadata (Prelude.Maybe Prelude.Text)
featureMetadata_description = Lens.lens (\FeatureMetadata' {description} -> description) (\s@FeatureMetadata' {} a -> s {description = a} :: FeatureMetadata)

-- | A timestamp indicating when the feature was last modified.
featureMetadata_lastModifiedTime :: Lens.Lens' FeatureMetadata (Prelude.Maybe Prelude.UTCTime)
featureMetadata_lastModifiedTime = Lens.lens (\FeatureMetadata' {lastModifiedTime} -> lastModifiedTime) (\s@FeatureMetadata' {} a -> s {lastModifiedTime = a} :: FeatureMetadata) Prelude.. Lens.mapping Data._Time

-- | The name of the feature group containing the feature.
featureMetadata_featureGroupName :: Lens.Lens' FeatureMetadata (Prelude.Maybe Prelude.Text)
featureMetadata_featureGroupName = Lens.lens (\FeatureMetadata' {featureGroupName} -> featureGroupName) (\s@FeatureMetadata' {} a -> s {featureGroupName = a} :: FeatureMetadata)

-- | A timestamp indicating when the feature was created.
featureMetadata_creationTime :: Lens.Lens' FeatureMetadata (Prelude.Maybe Prelude.UTCTime)
featureMetadata_creationTime = Lens.lens (\FeatureMetadata' {creationTime} -> creationTime) (\s@FeatureMetadata' {} a -> s {creationTime = a} :: FeatureMetadata) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Number (ARN) of the feature group.
featureMetadata_featureGroupArn :: Lens.Lens' FeatureMetadata (Prelude.Maybe Prelude.Text)
featureMetadata_featureGroupArn = Lens.lens (\FeatureMetadata' {featureGroupArn} -> featureGroupArn) (\s@FeatureMetadata' {} a -> s {featureGroupArn = a} :: FeatureMetadata)

-- | Optional key-value pairs that you specify to better describe the
-- feature.
featureMetadata_parameters :: Lens.Lens' FeatureMetadata (Prelude.Maybe [FeatureParameter])
featureMetadata_parameters = Lens.lens (\FeatureMetadata' {parameters} -> parameters) (\s@FeatureMetadata' {} a -> s {parameters = a} :: FeatureMetadata) Prelude.. Lens.mapping Lens.coerced

instance Data.FromJSON FeatureMetadata where
  parseJSON =
    Data.withObject
      "FeatureMetadata"
      ( \x ->
          FeatureMetadata'
            Prelude.<$> (x Data..:? "FeatureType")
            Prelude.<*> (x Data..:? "FeatureName")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "FeatureGroupName")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "FeatureGroupArn")
            Prelude.<*> (x Data..:? "Parameters" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FeatureMetadata where
  hashWithSalt _salt FeatureMetadata' {..} =
    _salt `Prelude.hashWithSalt` featureType
      `Prelude.hashWithSalt` featureName
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` featureGroupName
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` featureGroupArn
      `Prelude.hashWithSalt` parameters

instance Prelude.NFData FeatureMetadata where
  rnf FeatureMetadata' {..} =
    Prelude.rnf featureType
      `Prelude.seq` Prelude.rnf featureName
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf featureGroupName
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf featureGroupArn
      `Prelude.seq` Prelude.rnf parameters
