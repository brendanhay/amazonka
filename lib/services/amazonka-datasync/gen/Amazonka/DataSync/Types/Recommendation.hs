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
-- Module      : Amazonka.DataSync.Types.Recommendation
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DataSync.Types.Recommendation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details about an Amazon Web Services storage service that DataSync
-- Discovery recommends for a resource in your on-premises storage system.
--
-- For more information, see
-- <https://docs.aws.amazon.com/datasync/latest/userguide/discovery-understand-recommendations.html Recommendations provided by DataSync Discovery>.
--
-- /See:/ 'newRecommendation' smart constructor.
data Recommendation = Recommendation'
  { -- | The estimated monthly cost of the recommended Amazon Web Services
    -- storage service.
    estimatedMonthlyStorageCost :: Prelude.Maybe Prelude.Text,
    -- | Information about how you can set up a recommended Amazon Web Services
    -- storage service.
    storageConfiguration :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A recommended Amazon Web Services storage service that you can migrate
    -- data to based on information that DataSync Discovery collects about your
    -- on-premises storage system.
    storageType :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Recommendation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'estimatedMonthlyStorageCost', 'recommendation_estimatedMonthlyStorageCost' - The estimated monthly cost of the recommended Amazon Web Services
-- storage service.
--
-- 'storageConfiguration', 'recommendation_storageConfiguration' - Information about how you can set up a recommended Amazon Web Services
-- storage service.
--
-- 'storageType', 'recommendation_storageType' - A recommended Amazon Web Services storage service that you can migrate
-- data to based on information that DataSync Discovery collects about your
-- on-premises storage system.
newRecommendation ::
  Recommendation
newRecommendation =
  Recommendation'
    { estimatedMonthlyStorageCost =
        Prelude.Nothing,
      storageConfiguration = Prelude.Nothing,
      storageType = Prelude.Nothing
    }

-- | The estimated monthly cost of the recommended Amazon Web Services
-- storage service.
recommendation_estimatedMonthlyStorageCost :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_estimatedMonthlyStorageCost = Lens.lens (\Recommendation' {estimatedMonthlyStorageCost} -> estimatedMonthlyStorageCost) (\s@Recommendation' {} a -> s {estimatedMonthlyStorageCost = a} :: Recommendation)

-- | Information about how you can set up a recommended Amazon Web Services
-- storage service.
recommendation_storageConfiguration :: Lens.Lens' Recommendation (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
recommendation_storageConfiguration = Lens.lens (\Recommendation' {storageConfiguration} -> storageConfiguration) (\s@Recommendation' {} a -> s {storageConfiguration = a} :: Recommendation) Prelude.. Lens.mapping Lens.coerced

-- | A recommended Amazon Web Services storage service that you can migrate
-- data to based on information that DataSync Discovery collects about your
-- on-premises storage system.
recommendation_storageType :: Lens.Lens' Recommendation (Prelude.Maybe Prelude.Text)
recommendation_storageType = Lens.lens (\Recommendation' {storageType} -> storageType) (\s@Recommendation' {} a -> s {storageType = a} :: Recommendation)

instance Data.FromJSON Recommendation where
  parseJSON =
    Data.withObject
      "Recommendation"
      ( \x ->
          Recommendation'
            Prelude.<$> (x Data..:? "EstimatedMonthlyStorageCost")
            Prelude.<*> ( x
                            Data..:? "StorageConfiguration"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "StorageType")
      )

instance Prelude.Hashable Recommendation where
  hashWithSalt _salt Recommendation' {..} =
    _salt
      `Prelude.hashWithSalt` estimatedMonthlyStorageCost
      `Prelude.hashWithSalt` storageConfiguration
      `Prelude.hashWithSalt` storageType

instance Prelude.NFData Recommendation where
  rnf Recommendation' {..} =
    Prelude.rnf estimatedMonthlyStorageCost
      `Prelude.seq` Prelude.rnf storageConfiguration
      `Prelude.seq` Prelude.rnf storageType
