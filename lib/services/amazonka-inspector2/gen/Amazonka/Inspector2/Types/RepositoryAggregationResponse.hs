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
-- Module      : Amazonka.Inspector2.Types.RepositoryAggregationResponse
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.RepositoryAggregationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types.SeverityCounts
import qualified Amazonka.Prelude as Prelude

-- | A response that contains details on the results of a finding aggregation
-- by repository.
--
-- /See:/ 'newRepositoryAggregationResponse' smart constructor.
data RepositoryAggregationResponse = RepositoryAggregationResponse'
  { -- | The ID of the Amazon Web Services account associated with the findings.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The number of container images impacted by the findings.
    affectedImages :: Prelude.Maybe Prelude.Integer,
    -- | An object that represent the count of matched findings per severity.
    severityCounts :: Prelude.Maybe SeverityCounts,
    -- | The name of the repository associated with the findings.
    repository :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RepositoryAggregationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'repositoryAggregationResponse_accountId' - The ID of the Amazon Web Services account associated with the findings.
--
-- 'affectedImages', 'repositoryAggregationResponse_affectedImages' - The number of container images impacted by the findings.
--
-- 'severityCounts', 'repositoryAggregationResponse_severityCounts' - An object that represent the count of matched findings per severity.
--
-- 'repository', 'repositoryAggregationResponse_repository' - The name of the repository associated with the findings.
newRepositoryAggregationResponse ::
  -- | 'repository'
  Prelude.Text ->
  RepositoryAggregationResponse
newRepositoryAggregationResponse pRepository_ =
  RepositoryAggregationResponse'
    { accountId =
        Prelude.Nothing,
      affectedImages = Prelude.Nothing,
      severityCounts = Prelude.Nothing,
      repository = pRepository_
    }

-- | The ID of the Amazon Web Services account associated with the findings.
repositoryAggregationResponse_accountId :: Lens.Lens' RepositoryAggregationResponse (Prelude.Maybe Prelude.Text)
repositoryAggregationResponse_accountId = Lens.lens (\RepositoryAggregationResponse' {accountId} -> accountId) (\s@RepositoryAggregationResponse' {} a -> s {accountId = a} :: RepositoryAggregationResponse)

-- | The number of container images impacted by the findings.
repositoryAggregationResponse_affectedImages :: Lens.Lens' RepositoryAggregationResponse (Prelude.Maybe Prelude.Integer)
repositoryAggregationResponse_affectedImages = Lens.lens (\RepositoryAggregationResponse' {affectedImages} -> affectedImages) (\s@RepositoryAggregationResponse' {} a -> s {affectedImages = a} :: RepositoryAggregationResponse)

-- | An object that represent the count of matched findings per severity.
repositoryAggregationResponse_severityCounts :: Lens.Lens' RepositoryAggregationResponse (Prelude.Maybe SeverityCounts)
repositoryAggregationResponse_severityCounts = Lens.lens (\RepositoryAggregationResponse' {severityCounts} -> severityCounts) (\s@RepositoryAggregationResponse' {} a -> s {severityCounts = a} :: RepositoryAggregationResponse)

-- | The name of the repository associated with the findings.
repositoryAggregationResponse_repository :: Lens.Lens' RepositoryAggregationResponse Prelude.Text
repositoryAggregationResponse_repository = Lens.lens (\RepositoryAggregationResponse' {repository} -> repository) (\s@RepositoryAggregationResponse' {} a -> s {repository = a} :: RepositoryAggregationResponse)

instance Data.FromJSON RepositoryAggregationResponse where
  parseJSON =
    Data.withObject
      "RepositoryAggregationResponse"
      ( \x ->
          RepositoryAggregationResponse'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "affectedImages")
            Prelude.<*> (x Data..:? "severityCounts")
            Prelude.<*> (x Data..: "repository")
      )

instance
  Prelude.Hashable
    RepositoryAggregationResponse
  where
  hashWithSalt _salt RepositoryAggregationResponse' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` affectedImages
      `Prelude.hashWithSalt` severityCounts
      `Prelude.hashWithSalt` repository

instance Prelude.NFData RepositoryAggregationResponse where
  rnf RepositoryAggregationResponse' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf affectedImages
      `Prelude.seq` Prelude.rnf severityCounts
      `Prelude.seq` Prelude.rnf repository
