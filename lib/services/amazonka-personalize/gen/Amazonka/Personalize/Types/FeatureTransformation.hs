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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Personalize.Types.FeatureTransformation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Provides feature transformation information. Feature transformation is
-- the process of modifying raw input data into a form more suitable for
-- model training.
--
-- /See:/ 'newFeatureTransformation' smart constructor.
data FeatureTransformation = FeatureTransformation'
  { -- | The name of the feature transformation.
    name :: Prelude.Maybe Prelude.Text,
    -- | The creation date and time (in Unix time) of the feature transformation.
    creationDateTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the FeatureTransformation object.
    featureTransformationArn :: Prelude.Maybe Prelude.Text,
    -- | The status of the feature transformation.
    --
    -- A feature transformation can be in one of the following states:
    --
    -- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
    status :: Prelude.Maybe Prelude.Text,
    -- | Provides the default parameters for feature transformation.
    defaultParameters :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The last update date and time (in Unix time) of the feature
    -- transformation.
    lastUpdatedDateTime :: Prelude.Maybe Core.POSIX
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
-- 'name', 'featureTransformation_name' - The name of the feature transformation.
--
-- 'creationDateTime', 'featureTransformation_creationDateTime' - The creation date and time (in Unix time) of the feature transformation.
--
-- 'featureTransformationArn', 'featureTransformation_featureTransformationArn' - The Amazon Resource Name (ARN) of the FeatureTransformation object.
--
-- 'status', 'featureTransformation_status' - The status of the feature transformation.
--
-- A feature transformation can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
--
-- 'defaultParameters', 'featureTransformation_defaultParameters' - Provides the default parameters for feature transformation.
--
-- 'lastUpdatedDateTime', 'featureTransformation_lastUpdatedDateTime' - The last update date and time (in Unix time) of the feature
-- transformation.
newFeatureTransformation ::
  FeatureTransformation
newFeatureTransformation =
  FeatureTransformation'
    { name = Prelude.Nothing,
      creationDateTime = Prelude.Nothing,
      featureTransformationArn = Prelude.Nothing,
      status = Prelude.Nothing,
      defaultParameters = Prelude.Nothing,
      lastUpdatedDateTime = Prelude.Nothing
    }

-- | The name of the feature transformation.
featureTransformation_name :: Lens.Lens' FeatureTransformation (Prelude.Maybe Prelude.Text)
featureTransformation_name = Lens.lens (\FeatureTransformation' {name} -> name) (\s@FeatureTransformation' {} a -> s {name = a} :: FeatureTransformation)

-- | The creation date and time (in Unix time) of the feature transformation.
featureTransformation_creationDateTime :: Lens.Lens' FeatureTransformation (Prelude.Maybe Prelude.UTCTime)
featureTransformation_creationDateTime = Lens.lens (\FeatureTransformation' {creationDateTime} -> creationDateTime) (\s@FeatureTransformation' {} a -> s {creationDateTime = a} :: FeatureTransformation) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the FeatureTransformation object.
featureTransformation_featureTransformationArn :: Lens.Lens' FeatureTransformation (Prelude.Maybe Prelude.Text)
featureTransformation_featureTransformationArn = Lens.lens (\FeatureTransformation' {featureTransformationArn} -> featureTransformationArn) (\s@FeatureTransformation' {} a -> s {featureTransformationArn = a} :: FeatureTransformation)

-- | The status of the feature transformation.
--
-- A feature transformation can be in one of the following states:
--
-- -   CREATE PENDING > CREATE IN_PROGRESS > ACTIVE -or- CREATE FAILED
featureTransformation_status :: Lens.Lens' FeatureTransformation (Prelude.Maybe Prelude.Text)
featureTransformation_status = Lens.lens (\FeatureTransformation' {status} -> status) (\s@FeatureTransformation' {} a -> s {status = a} :: FeatureTransformation)

-- | Provides the default parameters for feature transformation.
featureTransformation_defaultParameters :: Lens.Lens' FeatureTransformation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
featureTransformation_defaultParameters = Lens.lens (\FeatureTransformation' {defaultParameters} -> defaultParameters) (\s@FeatureTransformation' {} a -> s {defaultParameters = a} :: FeatureTransformation) Prelude.. Lens.mapping Lens.coerced

-- | The last update date and time (in Unix time) of the feature
-- transformation.
featureTransformation_lastUpdatedDateTime :: Lens.Lens' FeatureTransformation (Prelude.Maybe Prelude.UTCTime)
featureTransformation_lastUpdatedDateTime = Lens.lens (\FeatureTransformation' {lastUpdatedDateTime} -> lastUpdatedDateTime) (\s@FeatureTransformation' {} a -> s {lastUpdatedDateTime = a} :: FeatureTransformation) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON FeatureTransformation where
  parseJSON =
    Core.withObject
      "FeatureTransformation"
      ( \x ->
          FeatureTransformation'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> (x Core..:? "creationDateTime")
            Prelude.<*> (x Core..:? "featureTransformationArn")
            Prelude.<*> (x Core..:? "status")
            Prelude.<*> ( x Core..:? "defaultParameters"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "lastUpdatedDateTime")
      )

instance Prelude.Hashable FeatureTransformation where
  hashWithSalt _salt FeatureTransformation' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` creationDateTime
      `Prelude.hashWithSalt` featureTransformationArn
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` defaultParameters
      `Prelude.hashWithSalt` lastUpdatedDateTime

instance Prelude.NFData FeatureTransformation where
  rnf FeatureTransformation' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf creationDateTime
      `Prelude.seq` Prelude.rnf featureTransformationArn
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf defaultParameters
      `Prelude.seq` Prelude.rnf lastUpdatedDateTime
