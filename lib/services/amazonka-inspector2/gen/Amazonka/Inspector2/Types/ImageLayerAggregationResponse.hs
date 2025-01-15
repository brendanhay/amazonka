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
-- Module      : Amazonka.Inspector2.Types.ImageLayerAggregationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.ImageLayerAggregationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.SeverityCounts
import qualified Amazonka.Prelude as Prelude

-- | A response that contains the results of a finding aggregation by image
-- layer.
--
-- /See:/ 'newImageLayerAggregationResponse' smart constructor.
data ImageLayerAggregationResponse = ImageLayerAggregationResponse'
  { -- | An object that represents the count of matched findings per severity.
    severityCounts :: Prelude.Maybe SeverityCounts,
    -- | The ID of the Amazon Web Services account that owns the container image
    -- hosting the layer image.
    accountId :: Prelude.Text,
    -- | The layer hash.
    layerHash :: Prelude.Text,
    -- | The repository the layer resides in.
    repository :: Prelude.Text,
    -- | The resource ID of the container image layer.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ImageLayerAggregationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'severityCounts', 'imageLayerAggregationResponse_severityCounts' - An object that represents the count of matched findings per severity.
--
-- 'accountId', 'imageLayerAggregationResponse_accountId' - The ID of the Amazon Web Services account that owns the container image
-- hosting the layer image.
--
-- 'layerHash', 'imageLayerAggregationResponse_layerHash' - The layer hash.
--
-- 'repository', 'imageLayerAggregationResponse_repository' - The repository the layer resides in.
--
-- 'resourceId', 'imageLayerAggregationResponse_resourceId' - The resource ID of the container image layer.
newImageLayerAggregationResponse ::
  -- | 'accountId'
  Prelude.Text ->
  -- | 'layerHash'
  Prelude.Text ->
  -- | 'repository'
  Prelude.Text ->
  -- | 'resourceId'
  Prelude.Text ->
  ImageLayerAggregationResponse
newImageLayerAggregationResponse
  pAccountId_
  pLayerHash_
  pRepository_
  pResourceId_ =
    ImageLayerAggregationResponse'
      { severityCounts =
          Prelude.Nothing,
        accountId = pAccountId_,
        layerHash = pLayerHash_,
        repository = pRepository_,
        resourceId = pResourceId_
      }

-- | An object that represents the count of matched findings per severity.
imageLayerAggregationResponse_severityCounts :: Lens.Lens' ImageLayerAggregationResponse (Prelude.Maybe SeverityCounts)
imageLayerAggregationResponse_severityCounts = Lens.lens (\ImageLayerAggregationResponse' {severityCounts} -> severityCounts) (\s@ImageLayerAggregationResponse' {} a -> s {severityCounts = a} :: ImageLayerAggregationResponse)

-- | The ID of the Amazon Web Services account that owns the container image
-- hosting the layer image.
imageLayerAggregationResponse_accountId :: Lens.Lens' ImageLayerAggregationResponse Prelude.Text
imageLayerAggregationResponse_accountId = Lens.lens (\ImageLayerAggregationResponse' {accountId} -> accountId) (\s@ImageLayerAggregationResponse' {} a -> s {accountId = a} :: ImageLayerAggregationResponse)

-- | The layer hash.
imageLayerAggregationResponse_layerHash :: Lens.Lens' ImageLayerAggregationResponse Prelude.Text
imageLayerAggregationResponse_layerHash = Lens.lens (\ImageLayerAggregationResponse' {layerHash} -> layerHash) (\s@ImageLayerAggregationResponse' {} a -> s {layerHash = a} :: ImageLayerAggregationResponse)

-- | The repository the layer resides in.
imageLayerAggregationResponse_repository :: Lens.Lens' ImageLayerAggregationResponse Prelude.Text
imageLayerAggregationResponse_repository = Lens.lens (\ImageLayerAggregationResponse' {repository} -> repository) (\s@ImageLayerAggregationResponse' {} a -> s {repository = a} :: ImageLayerAggregationResponse)

-- | The resource ID of the container image layer.
imageLayerAggregationResponse_resourceId :: Lens.Lens' ImageLayerAggregationResponse Prelude.Text
imageLayerAggregationResponse_resourceId = Lens.lens (\ImageLayerAggregationResponse' {resourceId} -> resourceId) (\s@ImageLayerAggregationResponse' {} a -> s {resourceId = a} :: ImageLayerAggregationResponse)

instance Data.FromJSON ImageLayerAggregationResponse where
  parseJSON =
    Data.withObject
      "ImageLayerAggregationResponse"
      ( \x ->
          ImageLayerAggregationResponse'
            Prelude.<$> (x Data..:? "severityCounts")
            Prelude.<*> (x Data..: "accountId")
            Prelude.<*> (x Data..: "layerHash")
            Prelude.<*> (x Data..: "repository")
            Prelude.<*> (x Data..: "resourceId")
      )

instance
  Prelude.Hashable
    ImageLayerAggregationResponse
  where
  hashWithSalt _salt ImageLayerAggregationResponse' {..} =
    _salt
      `Prelude.hashWithSalt` severityCounts
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` layerHash
      `Prelude.hashWithSalt` repository
      `Prelude.hashWithSalt` resourceId

instance Prelude.NFData ImageLayerAggregationResponse where
  rnf ImageLayerAggregationResponse' {..} =
    Prelude.rnf severityCounts `Prelude.seq`
      Prelude.rnf accountId `Prelude.seq`
        Prelude.rnf layerHash `Prelude.seq`
          Prelude.rnf repository `Prelude.seq`
            Prelude.rnf resourceId
