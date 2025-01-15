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
-- Module      : Amazonka.Personalize.Types.FeatureTransformation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.FeatureTransformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Provides feature transformation information. Feature transformation is
-- the process of modifying raw input data into a form more suitable for
-- model training.
--
-- /See:/ 'newFeatureTransformation' smart constructor.
data FeatureTransformation = FeatureTransformation'
  { -- | The creation date and time (in Unix time) of the feature transformation.
    creationDateTime :: Prelude.Maybe Data.POSIX,
    -- | Provides the default parameters for feature transformation.
    defaultParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The Amazon Resource Name (ARN) of the FeatureTransformation object.
    featureTransformationArn :: Prelude.Maybe Prelude.Text,
    -- | The last update date and time (in Unix time) of the feature
    -- transformation.
    lastUpdatedDateTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the feature transformation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the feature transformation.
    --
    -- A feature transformation can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    status :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'FeatureTransformation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationDateTime', 'featureTransformation_creationDateTime' - The creation date and time (in Unix time) of the feature transformation.
--
-- 'defaultParameters', 'featureTransformation_defaultParameters' - Provides the default parameters for feature transformation.
--
-- 'featureTransformationArn', 'featureTransformation_featureTransformationArn' - The Amazon Resource Name (ARN) of the FeatureTransformation object.
--
-- 'lastUpdatedDateTime', 'featureTransformation_lastUpdatedDateTime' - The last update date and time (in Unix time) of the feature
-- transformation.
--
-- 'name', 'featureTransformation_name' - The name of the feature transformation.
--
-- 'status', 'featureTransformation_status' - The status of the feature transformation.
--
-- A feature transformation can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
newFeatureTransformation ::
  FeatureTransformation
newFeatureTransformation =
  FeatureTransformation'
    { creationDateTime =
        Prelude.Nothing,
      defaultParameters = Prelude.Nothing,
      featureTransformationArn = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | The creation date and time (in Unix time) of the feature transformation.
featureTransformation_creationDateTime :: Lens.Lens' FeatureTransformation (Prelude.Maybe Prelude.UTCTime)
featureTransformation_creationDateTime = Lens.lens (\FeatureTransformation' {creationDateTime} -> creationDateTime) (\s@FeatureTransformation' {} a -> s {creationDateTime = a} :: FeatureTransformation) Prelude.. Lens.mapping Data._Time

-- | Provides the default parameters for feature transformation.
featureTransformation_defaultParameters :: Lens.Lens' FeatureTransformation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
featureTransformation_defaultParameters = Lens.lens (\FeatureTransformation' {defaultParameters} -> defaultParameters) (\s@FeatureTransformation' {} a -> s {defaultParameters = a} :: FeatureTransformation) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the FeatureTransformation object.
featureTransformation_featureTransformationArn :: Lens.Lens' FeatureTransformation (Prelude.Maybe Prelude.Text)
featureTransformation_featureTransformationArn = Lens.lens (\FeatureTransformation' {featureTransformationArn} -> featureTransformationArn) (\s@FeatureTransformation' {} a -> s {featureTransformationArn = a} :: FeatureTransformation)

-- | The last update date and time (in Unix time) of the feature
-- transformation.
featureTransformation_lastUpdatedDateTime :: Lens.Lens' FeatureTransformation (Prelude.Maybe Prelude.UTCTime)
featureTransformation_lastUpdatedDateTime = Lens.lens (\FeatureTransformation' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@FeatureTransformation' {} a -> s {lastUpdatedDateTime = a} :: FeatureTransformation) Prelude.. Lens.mapping Data._Time

-- | The name of the feature transformation.
featureTransformation_name :: Lens.Lens' FeatureTransformation (Prelude.Maybe Prelude.Text)
featureTransformation_name = Lens.lens (\FeatureTransformation' {name} -> name) (\s@FeatureTransformation' {} a -> s {name = a} :: FeatureTransformation)

-- | The status of the feature transformation.
--
-- A feature transformation can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
featureTransformation_status :: Lens.Lens' FeatureTransformation (Prelude.Maybe Prelude.Text)
featureTransformation_status = Lens.lens (\FeatureTransformation' {status} -> status) (\s@FeatureTransformation' {} a -> s {status = a} :: FeatureTransformation)

instance Data.FromJSON FeatureTransformation where
  parseJSON =
    Data.withObject
      "FeatureTransformation"
      ( \x ->
          FeatureTransformation'
            Prelude.<$> (x Data..:? "creationDateTime")
            Prelude.<*> ( x
                            Data..:? "defaultParameters"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "featureTransformationArn")
            Prelude.<*> (x Data..:? "lastUpdatedDateTime")
            Prelude.<*> (x Data..:? "name")
            Prelude.<*> (x Data..:? "status")
      )

instance Prelude.Hashable FeatureTransformation where
  hashWithSalt _salt FeatureTransformation' {..} =
    _salt
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` defaultParameters
      `Prelude.hashWithSalt` featureTransformationArn
      `Prelude.hashWithSalt` lastUpdatedDateTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status

instance Prelude.NFData FeatureTransformation where
  rnf FeatureTransformation' {..} =
    Prelude.rnf creationDateTime `Prelude.seq`
      Prelude.rnf defaultParameters `Prelude.seq`
        Prelude.rnf featureTransformationArn `Prelude.seq`
          Prelude.rnf lastUpdatedDateTime `Prelude.seq`
            Prelude.rnf name `Prelude.seq`
              Prelude.rnf status
