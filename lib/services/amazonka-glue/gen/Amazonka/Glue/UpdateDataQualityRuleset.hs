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
-- Module      : Amazonka.Glue.UpdateDataQualityRuleset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified data quality ruleset.
module Amazonka.Glue.UpdateDataQualityRuleset
  ( -- * Creating a Request
    UpdateDataQualityRuleset (..),
    newUpdateDataQualityRuleset,

    -- * Request Lenses
    updateDataQualityRuleset_description,
    updateDataQualityRuleset_ruleset,
    updateDataQualityRuleset_updatedName,
    updateDataQualityRuleset_name,

    -- * Destructuring the Response
    UpdateDataQualityRulesetResponse (..),
    newUpdateDataQualityRulesetResponse,

    -- * Response Lenses
    updateDataQualityRulesetResponse_description,
    updateDataQualityRulesetResponse_name,
    updateDataQualityRulesetResponse_ruleset,
    updateDataQualityRulesetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDataQualityRuleset' smart constructor.
data UpdateDataQualityRuleset = UpdateDataQualityRuleset'
  { -- | A description of the ruleset.
    description :: Prelude.Maybe Prelude.Text,
    -- | A Data Quality Definition Language (DQDL) ruleset. For more information,
    -- see the Glue developer guide.
    ruleset :: Prelude.Maybe Prelude.Text,
    -- | The new name of the ruleset, if you are renaming it.
    updatedName :: Prelude.Maybe Prelude.Text,
    -- | The name of the data quality ruleset.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataQualityRuleset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateDataQualityRuleset_description' - A description of the ruleset.
--
-- 'ruleset', 'updateDataQualityRuleset_ruleset' - A Data Quality Definition Language (DQDL) ruleset. For more information,
-- see the Glue developer guide.
--
-- 'updatedName', 'updateDataQualityRuleset_updatedName' - The new name of the ruleset, if you are renaming it.
--
-- 'name', 'updateDataQualityRuleset_name' - The name of the data quality ruleset.
newUpdateDataQualityRuleset ::
  -- | 'name'
  Prelude.Text ->
  UpdateDataQualityRuleset
newUpdateDataQualityRuleset pName_ =
  UpdateDataQualityRuleset'
    { description =
        Prelude.Nothing,
      ruleset = Prelude.Nothing,
      updatedName = Prelude.Nothing,
      name = pName_
    }

-- | A description of the ruleset.
updateDataQualityRuleset_description :: Lens.Lens' UpdateDataQualityRuleset (Prelude.Maybe Prelude.Text)
updateDataQualityRuleset_description = Lens.lens (\UpdateDataQualityRuleset' {description} -> description) (\s@UpdateDataQualityRuleset' {} a -> s {description = a} :: UpdateDataQualityRuleset)

-- | A Data Quality Definition Language (DQDL) ruleset. For more information,
-- see the Glue developer guide.
updateDataQualityRuleset_ruleset :: Lens.Lens' UpdateDataQualityRuleset (Prelude.Maybe Prelude.Text)
updateDataQualityRuleset_ruleset = Lens.lens (\UpdateDataQualityRuleset' {ruleset} -> ruleset) (\s@UpdateDataQualityRuleset' {} a -> s {ruleset = a} :: UpdateDataQualityRuleset)

-- | The new name of the ruleset, if you are renaming it.
updateDataQualityRuleset_updatedName :: Lens.Lens' UpdateDataQualityRuleset (Prelude.Maybe Prelude.Text)
updateDataQualityRuleset_updatedName = Lens.lens (\UpdateDataQualityRuleset' {updatedName} -> updatedName) (\s@UpdateDataQualityRuleset' {} a -> s {updatedName = a} :: UpdateDataQualityRuleset)

-- | The name of the data quality ruleset.
updateDataQualityRuleset_name :: Lens.Lens' UpdateDataQualityRuleset Prelude.Text
updateDataQualityRuleset_name = Lens.lens (\UpdateDataQualityRuleset' {name} -> name) (\s@UpdateDataQualityRuleset' {} a -> s {name = a} :: UpdateDataQualityRuleset)

instance Core.AWSRequest UpdateDataQualityRuleset where
  type
    AWSResponse UpdateDataQualityRuleset =
      UpdateDataQualityRulesetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDataQualityRulesetResponse'
            Prelude.<$> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "Ruleset")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDataQualityRuleset where
  hashWithSalt _salt UpdateDataQualityRuleset' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` ruleset
      `Prelude.hashWithSalt` updatedName
      `Prelude.hashWithSalt` name

instance Prelude.NFData UpdateDataQualityRuleset where
  rnf UpdateDataQualityRuleset' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf ruleset
      `Prelude.seq` Prelude.rnf updatedName
      `Prelude.seq` Prelude.rnf name

instance Data.ToHeaders UpdateDataQualityRuleset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.UpdateDataQualityRuleset" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateDataQualityRuleset where
  toJSON UpdateDataQualityRuleset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Ruleset" Data..=) Prelude.<$> ruleset,
            ("UpdatedName" Data..=) Prelude.<$> updatedName,
            Prelude.Just ("Name" Data..= name)
          ]
      )

instance Data.ToPath UpdateDataQualityRuleset where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateDataQualityRuleset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDataQualityRulesetResponse' smart constructor.
data UpdateDataQualityRulesetResponse = UpdateDataQualityRulesetResponse'
  { -- | A description of the ruleset.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the data quality ruleset.
    name :: Prelude.Maybe Prelude.Text,
    -- | A Data Quality Definition Language (DQDL) ruleset. For more information,
    -- see the Glue developer guide.
    ruleset :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataQualityRulesetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateDataQualityRulesetResponse_description' - A description of the ruleset.
--
-- 'name', 'updateDataQualityRulesetResponse_name' - The name of the data quality ruleset.
--
-- 'ruleset', 'updateDataQualityRulesetResponse_ruleset' - A Data Quality Definition Language (DQDL) ruleset. For more information,
-- see the Glue developer guide.
--
-- 'httpStatus', 'updateDataQualityRulesetResponse_httpStatus' - The response's http status code.
newUpdateDataQualityRulesetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateDataQualityRulesetResponse
newUpdateDataQualityRulesetResponse pHttpStatus_ =
  UpdateDataQualityRulesetResponse'
    { description =
        Prelude.Nothing,
      name = Prelude.Nothing,
      ruleset = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A description of the ruleset.
updateDataQualityRulesetResponse_description :: Lens.Lens' UpdateDataQualityRulesetResponse (Prelude.Maybe Prelude.Text)
updateDataQualityRulesetResponse_description = Lens.lens (\UpdateDataQualityRulesetResponse' {description} -> description) (\s@UpdateDataQualityRulesetResponse' {} a -> s {description = a} :: UpdateDataQualityRulesetResponse)

-- | The name of the data quality ruleset.
updateDataQualityRulesetResponse_name :: Lens.Lens' UpdateDataQualityRulesetResponse (Prelude.Maybe Prelude.Text)
updateDataQualityRulesetResponse_name = Lens.lens (\UpdateDataQualityRulesetResponse' {name} -> name) (\s@UpdateDataQualityRulesetResponse' {} a -> s {name = a} :: UpdateDataQualityRulesetResponse)

-- | A Data Quality Definition Language (DQDL) ruleset. For more information,
-- see the Glue developer guide.
updateDataQualityRulesetResponse_ruleset :: Lens.Lens' UpdateDataQualityRulesetResponse (Prelude.Maybe Prelude.Text)
updateDataQualityRulesetResponse_ruleset = Lens.lens (\UpdateDataQualityRulesetResponse' {ruleset} -> ruleset) (\s@UpdateDataQualityRulesetResponse' {} a -> s {ruleset = a} :: UpdateDataQualityRulesetResponse)

-- | The response's http status code.
updateDataQualityRulesetResponse_httpStatus :: Lens.Lens' UpdateDataQualityRulesetResponse Prelude.Int
updateDataQualityRulesetResponse_httpStatus = Lens.lens (\UpdateDataQualityRulesetResponse' {httpStatus} -> httpStatus) (\s@UpdateDataQualityRulesetResponse' {} a -> s {httpStatus = a} :: UpdateDataQualityRulesetResponse)

instance
  Prelude.NFData
    UpdateDataQualityRulesetResponse
  where
  rnf UpdateDataQualityRulesetResponse' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ruleset
      `Prelude.seq` Prelude.rnf httpStatus
