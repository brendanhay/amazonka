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
-- Copyright   : (c) 2013-2023 Brendan Hay
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
  { -- | A timestamp indicating when the feature was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | An optional description that you specify to better describe the feature.
    description :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Number (ARN) of the feature group.
    featureGroupArn :: Prelude.Maybe Prelude.Text,
    -- | The name of the feature group containing the feature.
    featureGroupName :: Prelude.Maybe Prelude.Text,
    -- | The name of feature.
    featureName :: Prelude.Maybe Prelude.Text,
    -- | The data type of the feature.
    featureType :: Prelude.Maybe FeatureType,
    -- | A timestamp indicating when the feature was last modified.
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
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
-- 'creationTime', 'featureMetadata_creationTime' - A timestamp indicating when the feature was created.
--
-- 'description', 'featureMetadata_description' - An optional description that you specify to better describe the feature.
--
-- 'featureGroupArn', 'featureMetadata_featureGroupArn' - The Amazon Resource Number (ARN) of the feature group.
--
-- 'featureGroupName', 'featureMetadata_featureGroupName' - The name of the feature group containing the feature.
--
-- 'featureName', 'featureMetadata_featureName' - The name of feature.
--
-- 'featureType', 'featureMetadata_featureType' - The data type of the feature.
--
-- 'lastModifiedTime', 'featureMetadata_lastModifiedTime' - A timestamp indicating when the feature was last modified.
--
-- 'parameters', 'featureMetadata_parameters' - Optional key-value pairs that you specify to better describe the
-- feature.
newFeatureMetadata ::
  FeatureMetadata
newFeatureMetadata =
  FeatureMetadata'
    { creationTime = Prelude.Nothing,
      description = Prelude.Nothing,
      featureGroupArn = Prelude.Nothing,
      featureGroupName = Prelude.Nothing,
      featureName = Prelude.Nothing,
      featureType = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      parameters = Prelude.Nothing
    }

-- | A timestamp indicating when the feature was created.
featureMetadata_creationTime :: Lens.Lens' FeatureMetadata (Prelude.Maybe Prelude.UTCTime)
featureMetadata_creationTime = Lens.lens (\FeatureMetadata' {creationTime} -> creationTime) (\s@FeatureMetadata' {} a -> s {creationTime = a} :: FeatureMetadata) Prelude.. Lens.mapping Data._Time

-- | An optional description that you specify to better describe the feature.
featureMetadata_description :: Lens.Lens' FeatureMetadata (Prelude.Maybe Prelude.Text)
featureMetadata_description = Lens.lens (\FeatureMetadata' {description} -> description) (\s@FeatureMetadata' {} a -> s {description = a} :: FeatureMetadata)

-- | The Amazon Resource Number (ARN) of the feature group.
featureMetadata_featureGroupArn :: Lens.Lens' FeatureMetadata (Prelude.Maybe Prelude.Text)
featureMetadata_featureGroupArn = Lens.lens (\FeatureMetadata' {featureGroupArn} -> featureGroupArn) (\s@FeatureMetadata' {} a -> s {featureGroupArn = a} :: FeatureMetadata)

-- | The name of the feature group containing the feature.
featureMetadata_featureGroupName :: Lens.Lens' FeatureMetadata (Prelude.Maybe Prelude.Text)
featureMetadata_featureGroupName = Lens.lens (\FeatureMetadata' {featureGroupName} -> featureGroupName) (\s@FeatureMetadata' {} a -> s {featureGroupName = a} :: FeatureMetadata)

-- | The name of feature.
featureMetadata_featureName :: Lens.Lens' FeatureMetadata (Prelude.Maybe Prelude.Text)
featureMetadata_featureName = Lens.lens (\FeatureMetadata' {featureName} -> featureName) (\s@FeatureMetadata' {} a -> s {featureName = a} :: FeatureMetadata)

-- | The data type of the feature.
featureMetadata_featureType :: Lens.Lens' FeatureMetadata (Prelude.Maybe FeatureType)
featureMetadata_featureType = Lens.lens (\FeatureMetadata' {featureType} -> featureType) (\s@FeatureMetadata' {} a -> s {featureType = a} :: FeatureMetadata)

-- | A timestamp indicating when the feature was last modified.
featureMetadata_lastModifiedTime :: Lens.Lens' FeatureMetadata (Prelude.Maybe Prelude.UTCTime)
featureMetadata_lastModifiedTime = Lens.lens (\FeatureMetadata' {lastModifiedTime} -> lastModifiedTime) (\s@FeatureMetadata' {} a -> s {lastModifiedTime = a} :: FeatureMetadata) Prelude.. Lens.mapping Data._Time

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
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "Description")
            Prelude.<*> (x Data..:? "FeatureGroupArn")
            Prelude.<*> (x Data..:? "FeatureGroupName")
            Prelude.<*> (x Data..:? "FeatureName")
            Prelude.<*> (x Data..:? "FeatureType")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "Parameters" Data..!= Prelude.mempty)
      )

instance Prelude.Hashable FeatureMetadata where
  hashWithSalt _salt FeatureMetadata' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` featureGroupArn
      `Prelude.hashWithSalt` featureGroupName
      `Prelude.hashWithSalt` featureName
      `Prelude.hashWithSalt` featureType
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` parameters

instance Prelude.NFData FeatureMetadata where
  rnf FeatureMetadata' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf featureGroupArn
      `Prelude.seq` Prelude.rnf featureGroupName
      `Prelude.seq` Prelude.rnf featureName
      `Prelude.seq` Prelude.rnf featureType
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf parameters
