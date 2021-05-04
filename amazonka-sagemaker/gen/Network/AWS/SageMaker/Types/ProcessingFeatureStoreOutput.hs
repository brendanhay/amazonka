{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.SageMaker.Types.ProcessingFeatureStoreOutput
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.SageMaker.Types.ProcessingFeatureStoreOutput where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Configuration for processing job outputs in Amazon SageMaker Feature
-- Store.
--
-- /See:/ 'newProcessingFeatureStoreOutput' smart constructor.
data ProcessingFeatureStoreOutput = ProcessingFeatureStoreOutput'
  { -- | The name of the Amazon SageMaker FeatureGroup to use as the destination
    -- for processing job output. Note that your processing script is
    -- responsible for putting records into your Feature Store.
    featureGroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ProcessingFeatureStoreOutput' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'featureGroupName', 'processingFeatureStoreOutput_featureGroupName' - The name of the Amazon SageMaker FeatureGroup to use as the destination
-- for processing job output. Note that your processing script is
-- responsible for putting records into your Feature Store.
newProcessingFeatureStoreOutput ::
  -- | 'featureGroupName'
  Prelude.Text ->
  ProcessingFeatureStoreOutput
newProcessingFeatureStoreOutput pFeatureGroupName_ =
  ProcessingFeatureStoreOutput'
    { featureGroupName =
        pFeatureGroupName_
    }

-- | The name of the Amazon SageMaker FeatureGroup to use as the destination
-- for processing job output. Note that your processing script is
-- responsible for putting records into your Feature Store.
processingFeatureStoreOutput_featureGroupName :: Lens.Lens' ProcessingFeatureStoreOutput Prelude.Text
processingFeatureStoreOutput_featureGroupName = Lens.lens (\ProcessingFeatureStoreOutput' {featureGroupName} -> featureGroupName) (\s@ProcessingFeatureStoreOutput' {} a -> s {featureGroupName = a} :: ProcessingFeatureStoreOutput)

instance
  Prelude.FromJSON
    ProcessingFeatureStoreOutput
  where
  parseJSON =
    Prelude.withObject
      "ProcessingFeatureStoreOutput"
      ( \x ->
          ProcessingFeatureStoreOutput'
            Prelude.<$> (x Prelude..: "FeatureGroupName")
      )

instance
  Prelude.Hashable
    ProcessingFeatureStoreOutput

instance Prelude.NFData ProcessingFeatureStoreOutput

instance Prelude.ToJSON ProcessingFeatureStoreOutput where
  toJSON ProcessingFeatureStoreOutput' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("FeatureGroupName" Prelude..= featureGroupName)
          ]
      )
