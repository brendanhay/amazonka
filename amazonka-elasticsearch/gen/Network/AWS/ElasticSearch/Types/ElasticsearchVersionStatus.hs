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
-- Module      : Network.AWS.ElasticSearch.Types.ElasticsearchVersionStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElasticSearch.Types.ElasticsearchVersionStatus where

import Network.AWS.ElasticSearch.Types.OptionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Status of the Elasticsearch version options for the specified
-- Elasticsearch domain.
--
-- /See:/ 'newElasticsearchVersionStatus' smart constructor.
data ElasticsearchVersionStatus = ElasticsearchVersionStatus'
  { -- | Specifies the Elasticsearch version for the specified Elasticsearch
    -- domain.
    options :: Prelude.Text,
    -- | Specifies the status of the Elasticsearch version options for the
    -- specified Elasticsearch domain.
    status :: OptionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ElasticsearchVersionStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'options', 'elasticsearchVersionStatus_options' - Specifies the Elasticsearch version for the specified Elasticsearch
-- domain.
--
-- 'status', 'elasticsearchVersionStatus_status' - Specifies the status of the Elasticsearch version options for the
-- specified Elasticsearch domain.
newElasticsearchVersionStatus ::
  -- | 'options'
  Prelude.Text ->
  -- | 'status'
  OptionStatus ->
  ElasticsearchVersionStatus
newElasticsearchVersionStatus pOptions_ pStatus_ =
  ElasticsearchVersionStatus'
    { options = pOptions_,
      status = pStatus_
    }

-- | Specifies the Elasticsearch version for the specified Elasticsearch
-- domain.
elasticsearchVersionStatus_options :: Lens.Lens' ElasticsearchVersionStatus Prelude.Text
elasticsearchVersionStatus_options = Lens.lens (\ElasticsearchVersionStatus' {options} -> options) (\s@ElasticsearchVersionStatus' {} a -> s {options = a} :: ElasticsearchVersionStatus)

-- | Specifies the status of the Elasticsearch version options for the
-- specified Elasticsearch domain.
elasticsearchVersionStatus_status :: Lens.Lens' ElasticsearchVersionStatus OptionStatus
elasticsearchVersionStatus_status = Lens.lens (\ElasticsearchVersionStatus' {status} -> status) (\s@ElasticsearchVersionStatus' {} a -> s {status = a} :: ElasticsearchVersionStatus)

instance Prelude.FromJSON ElasticsearchVersionStatus where
  parseJSON =
    Prelude.withObject
      "ElasticsearchVersionStatus"
      ( \x ->
          ElasticsearchVersionStatus'
            Prelude.<$> (x Prelude..: "Options")
            Prelude.<*> (x Prelude..: "Status")
      )

instance Prelude.Hashable ElasticsearchVersionStatus

instance Prelude.NFData ElasticsearchVersionStatus
