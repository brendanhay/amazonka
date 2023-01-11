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
-- Module      : Amazonka.SecurityHub.UpdateInsight
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the Security Hub insight identified by the specified insight
-- ARN.
module Amazonka.SecurityHub.UpdateInsight
  ( -- * Creating a Request
    UpdateInsight (..),
    newUpdateInsight,

    -- * Request Lenses
    updateInsight_filters,
    updateInsight_groupByAttribute,
    updateInsight_name,
    updateInsight_insightArn,

    -- * Destructuring the Response
    UpdateInsightResponse (..),
    newUpdateInsightResponse,

    -- * Response Lenses
    updateInsightResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newUpdateInsight' smart constructor.
data UpdateInsight = UpdateInsight'
  { -- | The updated filters that define this insight.
    filters :: Prelude.Maybe AwsSecurityFindingFilters,
    -- | The updated @GroupBy@ attribute that defines this insight.
    groupByAttribute :: Prelude.Maybe Prelude.Text,
    -- | The updated name for the insight.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the insight that you want to update.
    insightArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInsight' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'updateInsight_filters' - The updated filters that define this insight.
--
-- 'groupByAttribute', 'updateInsight_groupByAttribute' - The updated @GroupBy@ attribute that defines this insight.
--
-- 'name', 'updateInsight_name' - The updated name for the insight.
--
-- 'insightArn', 'updateInsight_insightArn' - The ARN of the insight that you want to update.
newUpdateInsight ::
  -- | 'insightArn'
  Prelude.Text ->
  UpdateInsight
newUpdateInsight pInsightArn_ =
  UpdateInsight'
    { filters = Prelude.Nothing,
      groupByAttribute = Prelude.Nothing,
      name = Prelude.Nothing,
      insightArn = pInsightArn_
    }

-- | The updated filters that define this insight.
updateInsight_filters :: Lens.Lens' UpdateInsight (Prelude.Maybe AwsSecurityFindingFilters)
updateInsight_filters = Lens.lens (\UpdateInsight' {filters} -> filters) (\s@UpdateInsight' {} a -> s {filters = a} :: UpdateInsight)

-- | The updated @GroupBy@ attribute that defines this insight.
updateInsight_groupByAttribute :: Lens.Lens' UpdateInsight (Prelude.Maybe Prelude.Text)
updateInsight_groupByAttribute = Lens.lens (\UpdateInsight' {groupByAttribute} -> groupByAttribute) (\s@UpdateInsight' {} a -> s {groupByAttribute = a} :: UpdateInsight)

-- | The updated name for the insight.
updateInsight_name :: Lens.Lens' UpdateInsight (Prelude.Maybe Prelude.Text)
updateInsight_name = Lens.lens (\UpdateInsight' {name} -> name) (\s@UpdateInsight' {} a -> s {name = a} :: UpdateInsight)

-- | The ARN of the insight that you want to update.
updateInsight_insightArn :: Lens.Lens' UpdateInsight Prelude.Text
updateInsight_insightArn = Lens.lens (\UpdateInsight' {insightArn} -> insightArn) (\s@UpdateInsight' {} a -> s {insightArn = a} :: UpdateInsight)

instance Core.AWSRequest UpdateInsight where
  type
    AWSResponse UpdateInsight =
      UpdateInsightResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateInsightResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateInsight where
  hashWithSalt _salt UpdateInsight' {..} =
    _salt `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` groupByAttribute
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` insightArn

instance Prelude.NFData UpdateInsight where
  rnf UpdateInsight' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf groupByAttribute
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf insightArn

instance Data.ToHeaders UpdateInsight where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateInsight where
  toJSON UpdateInsight' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("GroupByAttribute" Data..=)
              Prelude.<$> groupByAttribute,
            ("Name" Data..=) Prelude.<$> name
          ]
      )

instance Data.ToPath UpdateInsight where
  toPath UpdateInsight' {..} =
    Prelude.mconcat
      ["/insights/", Data.toBS insightArn]

instance Data.ToQuery UpdateInsight where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateInsightResponse' smart constructor.
data UpdateInsightResponse = UpdateInsightResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateInsightResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateInsightResponse_httpStatus' - The response's http status code.
newUpdateInsightResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateInsightResponse
newUpdateInsightResponse pHttpStatus_ =
  UpdateInsightResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateInsightResponse_httpStatus :: Lens.Lens' UpdateInsightResponse Prelude.Int
updateInsightResponse_httpStatus = Lens.lens (\UpdateInsightResponse' {httpStatus} -> httpStatus) (\s@UpdateInsightResponse' {} a -> s {httpStatus = a} :: UpdateInsightResponse)

instance Prelude.NFData UpdateInsightResponse where
  rnf UpdateInsightResponse' {..} =
    Prelude.rnf httpStatus
