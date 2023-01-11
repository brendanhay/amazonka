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
-- Module      : Amazonka.Glue.GetDataQualityRuleset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an existing ruleset by identifier or name.
module Amazonka.Glue.GetDataQualityRuleset
  ( -- * Creating a Request
    GetDataQualityRuleset (..),
    newGetDataQualityRuleset,

    -- * Request Lenses
    getDataQualityRuleset_name,

    -- * Destructuring the Response
    GetDataQualityRulesetResponse (..),
    newGetDataQualityRulesetResponse,

    -- * Response Lenses
    getDataQualityRulesetResponse_createdOn,
    getDataQualityRulesetResponse_description,
    getDataQualityRulesetResponse_lastModifiedOn,
    getDataQualityRulesetResponse_name,
    getDataQualityRulesetResponse_recommendationRunId,
    getDataQualityRulesetResponse_ruleset,
    getDataQualityRulesetResponse_targetTable,
    getDataQualityRulesetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetDataQualityRuleset' smart constructor.
data GetDataQualityRuleset = GetDataQualityRuleset'
  { -- | The name of the ruleset.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataQualityRuleset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'getDataQualityRuleset_name' - The name of the ruleset.
newGetDataQualityRuleset ::
  -- | 'name'
  Prelude.Text ->
  GetDataQualityRuleset
newGetDataQualityRuleset pName_ =
  GetDataQualityRuleset' {name = pName_}

-- | The name of the ruleset.
getDataQualityRuleset_name :: Lens.Lens' GetDataQualityRuleset Prelude.Text
getDataQualityRuleset_name = Lens.lens (\GetDataQualityRuleset' {name} -> name) (\s@GetDataQualityRuleset' {} a -> s {name = a} :: GetDataQualityRuleset)

instance Core.AWSRequest GetDataQualityRuleset where
  type
    AWSResponse GetDataQualityRuleset =
      GetDataQualityRulesetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDataQualityRulesetResponse'
            Prelude.<$> (x Data..?> "CreatedOn")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "LastModifiedOn")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "RecommendationRunId")
            Prelude.<*> (x Data..?> "Ruleset")
            Prelude.<*> (x Data..?> "TargetTable")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetDataQualityRuleset where
  hashWithSalt _salt GetDataQualityRuleset' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData GetDataQualityRuleset where
  rnf GetDataQualityRuleset' {..} = Prelude.rnf name

instance Data.ToHeaders GetDataQualityRuleset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.GetDataQualityRuleset" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetDataQualityRuleset where
  toJSON GetDataQualityRuleset' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )

instance Data.ToPath GetDataQualityRuleset where
  toPath = Prelude.const "/"

instance Data.ToQuery GetDataQualityRuleset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetDataQualityRulesetResponse' smart constructor.
data GetDataQualityRulesetResponse = GetDataQualityRulesetResponse'
  { -- | A timestamp. The time and date that this data quality ruleset was
    -- created.
    createdOn :: Prelude.Maybe Data.POSIX,
    -- | A description of the ruleset.
    description :: Prelude.Maybe Prelude.Text,
    -- | A timestamp. The last point in time when this data quality ruleset was
    -- modified.
    lastModifiedOn :: Prelude.Maybe Data.POSIX,
    -- | The name of the ruleset.
    name :: Prelude.Maybe Prelude.Text,
    -- | When a ruleset was created from a recommendation run, this run ID is
    -- generated to link the two together.
    recommendationRunId :: Prelude.Maybe Prelude.Text,
    -- | A Data Quality Definition Language (DQDL) ruleset. For more information,
    -- see the Glue developer guide.
    ruleset :: Prelude.Maybe Prelude.Text,
    -- | The name and database name of the target table.
    targetTable :: Prelude.Maybe DataQualityTargetTable,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetDataQualityRulesetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdOn', 'getDataQualityRulesetResponse_createdOn' - A timestamp. The time and date that this data quality ruleset was
-- created.
--
-- 'description', 'getDataQualityRulesetResponse_description' - A description of the ruleset.
--
-- 'lastModifiedOn', 'getDataQualityRulesetResponse_lastModifiedOn' - A timestamp. The last point in time when this data quality ruleset was
-- modified.
--
-- 'name', 'getDataQualityRulesetResponse_name' - The name of the ruleset.
--
-- 'recommendationRunId', 'getDataQualityRulesetResponse_recommendationRunId' - When a ruleset was created from a recommendation run, this run ID is
-- generated to link the two together.
--
-- 'ruleset', 'getDataQualityRulesetResponse_ruleset' - A Data Quality Definition Language (DQDL) ruleset. For more information,
-- see the Glue developer guide.
--
-- 'targetTable', 'getDataQualityRulesetResponse_targetTable' - The name and database name of the target table.
--
-- 'httpStatus', 'getDataQualityRulesetResponse_httpStatus' - The response's http status code.
newGetDataQualityRulesetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetDataQualityRulesetResponse
newGetDataQualityRulesetResponse pHttpStatus_ =
  GetDataQualityRulesetResponse'
    { createdOn =
        Prelude.Nothing,
      description = Prelude.Nothing,
      lastModifiedOn = Prelude.Nothing,
      name = Prelude.Nothing,
      recommendationRunId = Prelude.Nothing,
      ruleset = Prelude.Nothing,
      targetTable = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A timestamp. The time and date that this data quality ruleset was
-- created.
getDataQualityRulesetResponse_createdOn :: Lens.Lens' GetDataQualityRulesetResponse (Prelude.Maybe Prelude.UTCTime)
getDataQualityRulesetResponse_createdOn = Lens.lens (\GetDataQualityRulesetResponse' {createdOn} -> createdOn) (\s@GetDataQualityRulesetResponse' {} a -> s {createdOn = a} :: GetDataQualityRulesetResponse) Prelude.. Lens.mapping Data._Time

-- | A description of the ruleset.
getDataQualityRulesetResponse_description :: Lens.Lens' GetDataQualityRulesetResponse (Prelude.Maybe Prelude.Text)
getDataQualityRulesetResponse_description = Lens.lens (\GetDataQualityRulesetResponse' {description} -> description) (\s@GetDataQualityRulesetResponse' {} a -> s {description = a} :: GetDataQualityRulesetResponse)

-- | A timestamp. The last point in time when this data quality ruleset was
-- modified.
getDataQualityRulesetResponse_lastModifiedOn :: Lens.Lens' GetDataQualityRulesetResponse (Prelude.Maybe Prelude.UTCTime)
getDataQualityRulesetResponse_lastModifiedOn = Lens.lens (\GetDataQualityRulesetResponse' {lastModifiedOn} -> lastModifiedOn) (\s@GetDataQualityRulesetResponse' {} a -> s {lastModifiedOn = a} :: GetDataQualityRulesetResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the ruleset.
getDataQualityRulesetResponse_name :: Lens.Lens' GetDataQualityRulesetResponse (Prelude.Maybe Prelude.Text)
getDataQualityRulesetResponse_name = Lens.lens (\GetDataQualityRulesetResponse' {name} -> name) (\s@GetDataQualityRulesetResponse' {} a -> s {name = a} :: GetDataQualityRulesetResponse)

-- | When a ruleset was created from a recommendation run, this run ID is
-- generated to link the two together.
getDataQualityRulesetResponse_recommendationRunId :: Lens.Lens' GetDataQualityRulesetResponse (Prelude.Maybe Prelude.Text)
getDataQualityRulesetResponse_recommendationRunId = Lens.lens (\GetDataQualityRulesetResponse' {recommendationRunId} -> recommendationRunId) (\s@GetDataQualityRulesetResponse' {} a -> s {recommendationRunId = a} :: GetDataQualityRulesetResponse)

-- | A Data Quality Definition Language (DQDL) ruleset. For more information,
-- see the Glue developer guide.
getDataQualityRulesetResponse_ruleset :: Lens.Lens' GetDataQualityRulesetResponse (Prelude.Maybe Prelude.Text)
getDataQualityRulesetResponse_ruleset = Lens.lens (\GetDataQualityRulesetResponse' {ruleset} -> ruleset) (\s@GetDataQualityRulesetResponse' {} a -> s {ruleset = a} :: GetDataQualityRulesetResponse)

-- | The name and database name of the target table.
getDataQualityRulesetResponse_targetTable :: Lens.Lens' GetDataQualityRulesetResponse (Prelude.Maybe DataQualityTargetTable)
getDataQualityRulesetResponse_targetTable = Lens.lens (\GetDataQualityRulesetResponse' {targetTable} -> targetTable) (\s@GetDataQualityRulesetResponse' {} a -> s {targetTable = a} :: GetDataQualityRulesetResponse)

-- | The response's http status code.
getDataQualityRulesetResponse_httpStatus :: Lens.Lens' GetDataQualityRulesetResponse Prelude.Int
getDataQualityRulesetResponse_httpStatus = Lens.lens (\GetDataQualityRulesetResponse' {httpStatus} -> httpStatus) (\s@GetDataQualityRulesetResponse' {} a -> s {httpStatus = a} :: GetDataQualityRulesetResponse)

instance Prelude.NFData GetDataQualityRulesetResponse where
  rnf GetDataQualityRulesetResponse' {..} =
    Prelude.rnf createdOn
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf lastModifiedOn
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf recommendationRunId
      `Prelude.seq` Prelude.rnf ruleset
      `Prelude.seq` Prelude.rnf targetTable
      `Prelude.seq` Prelude.rnf httpStatus
