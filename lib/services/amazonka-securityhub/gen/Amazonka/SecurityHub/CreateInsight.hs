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
-- Module      : Amazonka.SecurityHub.CreateInsight
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a custom insight in Security Hub. An insight is a consolidation
-- of findings that relate to a security issue that requires attention or
-- remediation.
--
-- To group the related findings in the insight, use the
-- @GroupByAttribute@.
module Amazonka.SecurityHub.CreateInsight
  ( -- * Creating a Request
    CreateInsight (..),
    newCreateInsight,

    -- * Request Lenses
    createInsight_name,
    createInsight_filters,
    createInsight_groupByAttribute,

    -- * Destructuring the Response
    CreateInsightResponse (..),
    newCreateInsightResponse,

    -- * Response Lenses
    createInsightResponse_httpStatus,
    createInsightResponse_insightArn,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newCreateInsight' smart constructor.
data CreateInsight = CreateInsight'
  { -- | The name of the custom insight to create.
    name :: Prelude.Text,
    -- | One or more attributes used to filter the findings included in the
    -- insight. The insight only includes findings that match the criteria
    -- defined in the filters.
    filters :: AwsSecurityFindingFilters,
    -- | The attribute used to group the findings for the insight. The grouping
    -- attribute identifies the type of item that the insight applies to. For
    -- example, if an insight is grouped by resource identifier, then the
    -- insight produces a list of resource identifiers.
    groupByAttribute :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInsight' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createInsight_name' - The name of the custom insight to create.
--
-- 'filters', 'createInsight_filters' - One or more attributes used to filter the findings included in the
-- insight. The insight only includes findings that match the criteria
-- defined in the filters.
--
-- 'groupByAttribute', 'createInsight_groupByAttribute' - The attribute used to group the findings for the insight. The grouping
-- attribute identifies the type of item that the insight applies to. For
-- example, if an insight is grouped by resource identifier, then the
-- insight produces a list of resource identifiers.
newCreateInsight ::
  -- | 'name'
  Prelude.Text ->
  -- | 'filters'
  AwsSecurityFindingFilters ->
  -- | 'groupByAttribute'
  Prelude.Text ->
  CreateInsight
newCreateInsight pName_ pFilters_ pGroupByAttribute_ =
  CreateInsight'
    { name = pName_,
      filters = pFilters_,
      groupByAttribute = pGroupByAttribute_
    }

-- | The name of the custom insight to create.
createInsight_name :: Lens.Lens' CreateInsight Prelude.Text
createInsight_name = Lens.lens (\CreateInsight' {name} -> name) (\s@CreateInsight' {} a -> s {name = a} :: CreateInsight)

-- | One or more attributes used to filter the findings included in the
-- insight. The insight only includes findings that match the criteria
-- defined in the filters.
createInsight_filters :: Lens.Lens' CreateInsight AwsSecurityFindingFilters
createInsight_filters = Lens.lens (\CreateInsight' {filters} -> filters) (\s@CreateInsight' {} a -> s {filters = a} :: CreateInsight)

-- | The attribute used to group the findings for the insight. The grouping
-- attribute identifies the type of item that the insight applies to. For
-- example, if an insight is grouped by resource identifier, then the
-- insight produces a list of resource identifiers.
createInsight_groupByAttribute :: Lens.Lens' CreateInsight Prelude.Text
createInsight_groupByAttribute = Lens.lens (\CreateInsight' {groupByAttribute} -> groupByAttribute) (\s@CreateInsight' {} a -> s {groupByAttribute = a} :: CreateInsight)

instance Core.AWSRequest CreateInsight where
  type
    AWSResponse CreateInsight =
      CreateInsightResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateInsightResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "InsightArn")
      )

instance Prelude.Hashable CreateInsight where
  hashWithSalt _salt CreateInsight' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` groupByAttribute

instance Prelude.NFData CreateInsight where
  rnf CreateInsight' {..} =
    Prelude.rnf name `Prelude.seq`
      Prelude.rnf filters `Prelude.seq`
        Prelude.rnf groupByAttribute

instance Data.ToHeaders CreateInsight where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateInsight where
  toJSON CreateInsight' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Filters" Data..= filters),
            Prelude.Just
              ("GroupByAttribute" Data..= groupByAttribute)
          ]
      )

instance Data.ToPath CreateInsight where
  toPath = Prelude.const "/insights"

instance Data.ToQuery CreateInsight where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateInsightResponse' smart constructor.
data CreateInsightResponse = CreateInsightResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The ARN of the insight created.
    insightArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateInsightResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createInsightResponse_httpStatus' - The response's http status code.
--
-- 'insightArn', 'createInsightResponse_insightArn' - The ARN of the insight created.
newCreateInsightResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'insightArn'
  Prelude.Text ->
  CreateInsightResponse
newCreateInsightResponse pHttpStatus_ pInsightArn_ =
  CreateInsightResponse'
    { httpStatus = pHttpStatus_,
      insightArn = pInsightArn_
    }

-- | The response's http status code.
createInsightResponse_httpStatus :: Lens.Lens' CreateInsightResponse Prelude.Int
createInsightResponse_httpStatus = Lens.lens (\CreateInsightResponse' {httpStatus} -> httpStatus) (\s@CreateInsightResponse' {} a -> s {httpStatus = a} :: CreateInsightResponse)

-- | The ARN of the insight created.
createInsightResponse_insightArn :: Lens.Lens' CreateInsightResponse Prelude.Text
createInsightResponse_insightArn = Lens.lens (\CreateInsightResponse' {insightArn} -> insightArn) (\s@CreateInsightResponse' {} a -> s {insightArn = a} :: CreateInsightResponse)

instance Prelude.NFData CreateInsightResponse where
  rnf CreateInsightResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf insightArn
