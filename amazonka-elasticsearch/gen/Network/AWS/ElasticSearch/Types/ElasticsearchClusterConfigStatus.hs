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
-- Module      : Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfigStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfigStatus where

import Network.AWS.ElasticSearch.Types.ElasticsearchClusterConfig
import Network.AWS.ElasticSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Specifies the configuration status for the specified Elasticsearch
-- domain.
--
-- /See:/ 'newElasticsearchClusterConfigStatus' smart constructor.
data ElasticsearchClusterConfigStatus = ElasticsearchClusterConfigStatus'
  { -- | Specifies the cluster configuration for the specified Elasticsearch
    -- domain.
    options :: ElasticsearchClusterConfig,
    -- | Specifies the status of the configuration for the specified
    -- Elasticsearch domain.
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ElasticsearchClusterConfigStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'elasticsearchClusterConfigStatus_options' - Specifies the cluster configuration for the specified Elasticsearch
-- domain.
--
-- 'status', 'elasticsearchClusterConfigStatus_status' - Specifies the status of the configuration for the specified
-- Elasticsearch domain.
newElasticsearchClusterConfigStatus ::
  -- | 'options'
  ElasticsearchClusterConfig ->
  -- | 'status'
  OptionStatus ->
  ElasticsearchClusterConfigStatus
newElasticsearchClusterConfigStatus
  pOptions_
  pStatus_ =
    ElasticsearchClusterConfigStatus'
      { options =
          pOptions_,
        status = pStatus_
      }

-- | Specifies the cluster configuration for the specified Elasticsearch
-- domain.
elasticsearchClusterConfigStatus_options :: Lens.Lens' ElasticsearchClusterConfigStatus ElasticsearchClusterConfig
elasticsearchClusterConfigStatus_options = Lens.lens (\ElasticsearchClusterConfigStatus' {options} -> options) (\s@ElasticsearchClusterConfigStatus' {} a -> s {options = a} :: ElasticsearchClusterConfigStatus)

-- | Specifies the status of the configuration for the specified
-- Elasticsearch domain.
elasticsearchClusterConfigStatus_status :: Lens.Lens' ElasticsearchClusterConfigStatus OptionStatus
elasticsearchClusterConfigStatus_status = Lens.lens (\ElasticsearchClusterConfigStatus' {status} -> status) (\s@ElasticsearchClusterConfigStatus' {} a -> s {status = a} :: ElasticsearchClusterConfigStatus)

instance
  Prelude.FromJSON
    ElasticsearchClusterConfigStatus
  where
  parseJSON =
    Prelude.withObject
      "ElasticsearchClusterConfigStatus"
      ( \x ->
          ElasticsearchClusterConfigStatus'
            Prelude.<$> (x Prelude..: "Options")
            Prelude.<*> (x Prelude..: "Status")
      )

instance
  Prelude.Hashable
    ElasticsearchClusterConfigStatus

instance
  Prelude.NFData
    ElasticsearchClusterConfigStatus
