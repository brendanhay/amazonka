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
-- Module      : Amazonka.Inspector2.Types.TitleAggregationResponse
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector2.Types.TitleAggregationResponse where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Inspector2.Types.SeverityCounts
import qualified Amazonka.Prelude as Prelude

-- | A response that contains details on the results of a finding aggregation
-- by title.
--
-- /See:/ 'newTitleAggregationResponse' smart constructor.
data TitleAggregationResponse = TitleAggregationResponse'
  { -- | An object that represent the count of matched findings per severity.
    severityCounts :: Prelude.Maybe SeverityCounts,
    -- | The ID of the Amazon Web Services account associated with the findings.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The vulnerability ID of the finding.
    vulnerabilityId :: Prelude.Maybe Prelude.Text,
    -- | The title that the findings were aggregated on.
    title :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TitleAggregationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'severityCounts', 'titleAggregationResponse_severityCounts' - An object that represent the count of matched findings per severity.
--
-- 'accountId', 'titleAggregationResponse_accountId' - The ID of the Amazon Web Services account associated with the findings.
--
-- 'vulnerabilityId', 'titleAggregationResponse_vulnerabilityId' - The vulnerability ID of the finding.
--
-- 'title', 'titleAggregationResponse_title' - The title that the findings were aggregated on.
newTitleAggregationResponse ::
  -- | 'title'
  Prelude.Text ->
  TitleAggregationResponse
newTitleAggregationResponse pTitle_ =
  TitleAggregationResponse'
    { severityCounts =
        Prelude.Nothing,
      accountId = Prelude.Nothing,
      vulnerabilityId = Prelude.Nothing,
      title = pTitle_
    }

-- | An object that represent the count of matched findings per severity.
titleAggregationResponse_severityCounts :: Lens.Lens' TitleAggregationResponse (Prelude.Maybe SeverityCounts)
titleAggregationResponse_severityCounts = Lens.lens (\TitleAggregationResponse' {severityCounts} -> severityCounts) (\s@TitleAggregationResponse' {} a -> s {severityCounts = a} :: TitleAggregationResponse)

-- | The ID of the Amazon Web Services account associated with the findings.
titleAggregationResponse_accountId :: Lens.Lens' TitleAggregationResponse (Prelude.Maybe Prelude.Text)
titleAggregationResponse_accountId = Lens.lens (\TitleAggregationResponse' {accountId} -> accountId) (\s@TitleAggregationResponse' {} a -> s {accountId = a} :: TitleAggregationResponse)

-- | The vulnerability ID of the finding.
titleAggregationResponse_vulnerabilityId :: Lens.Lens' TitleAggregationResponse (Prelude.Maybe Prelude.Text)
titleAggregationResponse_vulnerabilityId = Lens.lens (\TitleAggregationResponse' {vulnerabilityId} -> vulnerabilityId) (\s@TitleAggregationResponse' {} a -> s {vulnerabilityId = a} :: TitleAggregationResponse)

-- | The title that the findings were aggregated on.
titleAggregationResponse_title :: Lens.Lens' TitleAggregationResponse Prelude.Text
titleAggregationResponse_title = Lens.lens (\TitleAggregationResponse' {title} -> title) (\s@TitleAggregationResponse' {} a -> s {title = a} :: TitleAggregationResponse)

instance Core.FromJSON TitleAggregationResponse where
  parseJSON =
    Core.withObject
      "TitleAggregationResponse"
      ( \x ->
          TitleAggregationResponse'
            Prelude.<$> (x Core..:? "severityCounts")
            Prelude.<*> (x Core..:? "accountId")
            Prelude.<*> (x Core..:? "vulnerabilityId")
            Prelude.<*> (x Core..: "title")
      )

instance Prelude.Hashable TitleAggregationResponse where
  hashWithSalt _salt TitleAggregationResponse' {..} =
    _salt `Prelude.hashWithSalt` severityCounts
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` vulnerabilityId
      `Prelude.hashWithSalt` title

instance Prelude.NFData TitleAggregationResponse where
  rnf TitleAggregationResponse' {..} =
    Prelude.rnf severityCounts
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf vulnerabilityId
      `Prelude.seq` Prelude.rnf title
