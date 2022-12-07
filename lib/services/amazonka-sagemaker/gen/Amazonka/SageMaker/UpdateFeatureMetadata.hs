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
-- Module      : Amazonka.SageMaker.UpdateFeatureMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the description and parameters of the feature group.
module Amazonka.SageMaker.UpdateFeatureMetadata
  ( -- * Creating a Request
    UpdateFeatureMetadata (..),
    newUpdateFeatureMetadata,

    -- * Request Lenses
    updateFeatureMetadata_parameterRemovals,
    updateFeatureMetadata_description,
    updateFeatureMetadata_parameterAdditions,
    updateFeatureMetadata_featureGroupName,
    updateFeatureMetadata_featureName,

    -- * Destructuring the Response
    UpdateFeatureMetadataResponse (..),
    newUpdateFeatureMetadataResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newUpdateFeatureMetadata' smart constructor.
data UpdateFeatureMetadata = UpdateFeatureMetadata'
  { -- | A list of parameter keys that you can specify to remove parameters that
    -- describe your feature.
    parameterRemovals :: Prelude.Maybe [Prelude.Text],
    -- | A description that you can write to better describe the feature.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of key-value pairs that you can add to better describe the
    -- feature.
    parameterAdditions :: Prelude.Maybe [FeatureParameter],
    -- | The name of the feature group containing the feature that you\'re
    -- updating.
    featureGroupName :: Prelude.Text,
    -- | The name of the feature that you\'re updating.
    featureName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFeatureMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'parameterRemovals', 'updateFeatureMetadata_parameterRemovals' - A list of parameter keys that you can specify to remove parameters that
-- describe your feature.
--
-- 'description', 'updateFeatureMetadata_description' - A description that you can write to better describe the feature.
--
-- 'parameterAdditions', 'updateFeatureMetadata_parameterAdditions' - A list of key-value pairs that you can add to better describe the
-- feature.
--
-- 'featureGroupName', 'updateFeatureMetadata_featureGroupName' - The name of the feature group containing the feature that you\'re
-- updating.
--
-- 'featureName', 'updateFeatureMetadata_featureName' - The name of the feature that you\'re updating.
newUpdateFeatureMetadata ::
  -- | 'featureGroupName'
  Prelude.Text ->
  -- | 'featureName'
  Prelude.Text ->
  UpdateFeatureMetadata
newUpdateFeatureMetadata
  pFeatureGroupName_
  pFeatureName_ =
    UpdateFeatureMetadata'
      { parameterRemovals =
          Prelude.Nothing,
        description = Prelude.Nothing,
        parameterAdditions = Prelude.Nothing,
        featureGroupName = pFeatureGroupName_,
        featureName = pFeatureName_
      }

-- | A list of parameter keys that you can specify to remove parameters that
-- describe your feature.
updateFeatureMetadata_parameterRemovals :: Lens.Lens' UpdateFeatureMetadata (Prelude.Maybe [Prelude.Text])
updateFeatureMetadata_parameterRemovals = Lens.lens (\UpdateFeatureMetadata' {parameterRemovals} -> parameterRemovals) (\s@UpdateFeatureMetadata' {} a -> s {parameterRemovals = a} :: UpdateFeatureMetadata) Prelude.. Lens.mapping Lens.coerced

-- | A description that you can write to better describe the feature.
updateFeatureMetadata_description :: Lens.Lens' UpdateFeatureMetadata (Prelude.Maybe Prelude.Text)
updateFeatureMetadata_description = Lens.lens (\UpdateFeatureMetadata' {description} -> description) (\s@UpdateFeatureMetadata' {} a -> s {description = a} :: UpdateFeatureMetadata)

-- | A list of key-value pairs that you can add to better describe the
-- feature.
updateFeatureMetadata_parameterAdditions :: Lens.Lens' UpdateFeatureMetadata (Prelude.Maybe [FeatureParameter])
updateFeatureMetadata_parameterAdditions = Lens.lens (\UpdateFeatureMetadata' {parameterAdditions} -> parameterAdditions) (\s@UpdateFeatureMetadata' {} a -> s {parameterAdditions = a} :: UpdateFeatureMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The name of the feature group containing the feature that you\'re
-- updating.
updateFeatureMetadata_featureGroupName :: Lens.Lens' UpdateFeatureMetadata Prelude.Text
updateFeatureMetadata_featureGroupName = Lens.lens (\UpdateFeatureMetadata' {featureGroupName} -> featureGroupName) (\s@UpdateFeatureMetadata' {} a -> s {featureGroupName = a} :: UpdateFeatureMetadata)

-- | The name of the feature that you\'re updating.
updateFeatureMetadata_featureName :: Lens.Lens' UpdateFeatureMetadata Prelude.Text
updateFeatureMetadata_featureName = Lens.lens (\UpdateFeatureMetadata' {featureName} -> featureName) (\s@UpdateFeatureMetadata' {} a -> s {featureName = a} :: UpdateFeatureMetadata)

instance Core.AWSRequest UpdateFeatureMetadata where
  type
    AWSResponse UpdateFeatureMetadata =
      UpdateFeatureMetadataResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateFeatureMetadataResponse'

instance Prelude.Hashable UpdateFeatureMetadata where
  hashWithSalt _salt UpdateFeatureMetadata' {..} =
    _salt `Prelude.hashWithSalt` parameterRemovals
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` parameterAdditions
      `Prelude.hashWithSalt` featureGroupName
      `Prelude.hashWithSalt` featureName

instance Prelude.NFData UpdateFeatureMetadata where
  rnf UpdateFeatureMetadata' {..} =
    Prelude.rnf parameterRemovals
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf parameterAdditions
      `Prelude.seq` Prelude.rnf featureGroupName
      `Prelude.seq` Prelude.rnf featureName

instance Data.ToHeaders UpdateFeatureMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.UpdateFeatureMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateFeatureMetadata where
  toJSON UpdateFeatureMetadata' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ParameterRemovals" Data..=)
              Prelude.<$> parameterRemovals,
            ("Description" Data..=) Prelude.<$> description,
            ("ParameterAdditions" Data..=)
              Prelude.<$> parameterAdditions,
            Prelude.Just
              ("FeatureGroupName" Data..= featureGroupName),
            Prelude.Just ("FeatureName" Data..= featureName)
          ]
      )

instance Data.ToPath UpdateFeatureMetadata where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateFeatureMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateFeatureMetadataResponse' smart constructor.
data UpdateFeatureMetadataResponse = UpdateFeatureMetadataResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateFeatureMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateFeatureMetadataResponse ::
  UpdateFeatureMetadataResponse
newUpdateFeatureMetadataResponse =
  UpdateFeatureMetadataResponse'

instance Prelude.NFData UpdateFeatureMetadataResponse where
  rnf _ = ()
