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
-- Module      : Amazonka.Glue.CreateDataQualityRuleset
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a data quality ruleset with DQDL rules applied to a specified
-- Glue table.
--
-- You create the ruleset using the Data Quality Definition Language
-- (DQDL). For more information, see the Glue developer guide.
module Amazonka.Glue.CreateDataQualityRuleset
  ( -- * Creating a Request
    CreateDataQualityRuleset (..),
    newCreateDataQualityRuleset,

    -- * Request Lenses
    createDataQualityRuleset_clientToken,
    createDataQualityRuleset_description,
    createDataQualityRuleset_tags,
    createDataQualityRuleset_targetTable,
    createDataQualityRuleset_name,
    createDataQualityRuleset_ruleset,

    -- * Destructuring the Response
    CreateDataQualityRulesetResponse (..),
    newCreateDataQualityRulesetResponse,

    -- * Response Lenses
    createDataQualityRulesetResponse_name,
    createDataQualityRulesetResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateDataQualityRuleset' smart constructor.
data CreateDataQualityRuleset = CreateDataQualityRuleset'
  { -- | Used for idempotency and is recommended to be set to a random ID (such
    -- as a UUID) to avoid creating or starting multiple instances of the same
    -- resource.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | A description of the data quality ruleset.
    description :: Prelude.Maybe Prelude.Text,
    -- | A list of tags applied to the data quality ruleset.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | A target table associated with the data quality ruleset.
    targetTable :: Prelude.Maybe DataQualityTargetTable,
    -- | A unique name for the data quality ruleset.
    name :: Prelude.Text,
    -- | A Data Quality Definition Language (DQDL) ruleset. For more information,
    -- see the Glue developer guide.
    ruleset :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataQualityRuleset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createDataQualityRuleset_clientToken' - Used for idempotency and is recommended to be set to a random ID (such
-- as a UUID) to avoid creating or starting multiple instances of the same
-- resource.
--
-- 'description', 'createDataQualityRuleset_description' - A description of the data quality ruleset.
--
-- 'tags', 'createDataQualityRuleset_tags' - A list of tags applied to the data quality ruleset.
--
-- 'targetTable', 'createDataQualityRuleset_targetTable' - A target table associated with the data quality ruleset.
--
-- 'name', 'createDataQualityRuleset_name' - A unique name for the data quality ruleset.
--
-- 'ruleset', 'createDataQualityRuleset_ruleset' - A Data Quality Definition Language (DQDL) ruleset. For more information,
-- see the Glue developer guide.
newCreateDataQualityRuleset ::
  -- | 'name'
  Prelude.Text ->
  -- | 'ruleset'
  Prelude.Text ->
  CreateDataQualityRuleset
newCreateDataQualityRuleset pName_ pRuleset_ =
  CreateDataQualityRuleset'
    { clientToken =
        Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      targetTable = Prelude.Nothing,
      name = pName_,
      ruleset = pRuleset_
    }

-- | Used for idempotency and is recommended to be set to a random ID (such
-- as a UUID) to avoid creating or starting multiple instances of the same
-- resource.
createDataQualityRuleset_clientToken :: Lens.Lens' CreateDataQualityRuleset (Prelude.Maybe Prelude.Text)
createDataQualityRuleset_clientToken = Lens.lens (\CreateDataQualityRuleset' {clientToken} -> clientToken) (\s@CreateDataQualityRuleset' {} a -> s {clientToken = a} :: CreateDataQualityRuleset)

-- | A description of the data quality ruleset.
createDataQualityRuleset_description :: Lens.Lens' CreateDataQualityRuleset (Prelude.Maybe Prelude.Text)
createDataQualityRuleset_description = Lens.lens (\CreateDataQualityRuleset' {description} -> description) (\s@CreateDataQualityRuleset' {} a -> s {description = a} :: CreateDataQualityRuleset)

-- | A list of tags applied to the data quality ruleset.
createDataQualityRuleset_tags :: Lens.Lens' CreateDataQualityRuleset (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createDataQualityRuleset_tags = Lens.lens (\CreateDataQualityRuleset' {tags} -> tags) (\s@CreateDataQualityRuleset' {} a -> s {tags = a} :: CreateDataQualityRuleset) Prelude.. Lens.mapping Lens.coerced

-- | A target table associated with the data quality ruleset.
createDataQualityRuleset_targetTable :: Lens.Lens' CreateDataQualityRuleset (Prelude.Maybe DataQualityTargetTable)
createDataQualityRuleset_targetTable = Lens.lens (\CreateDataQualityRuleset' {targetTable} -> targetTable) (\s@CreateDataQualityRuleset' {} a -> s {targetTable = a} :: CreateDataQualityRuleset)

-- | A unique name for the data quality ruleset.
createDataQualityRuleset_name :: Lens.Lens' CreateDataQualityRuleset Prelude.Text
createDataQualityRuleset_name = Lens.lens (\CreateDataQualityRuleset' {name} -> name) (\s@CreateDataQualityRuleset' {} a -> s {name = a} :: CreateDataQualityRuleset)

-- | A Data Quality Definition Language (DQDL) ruleset. For more information,
-- see the Glue developer guide.
createDataQualityRuleset_ruleset :: Lens.Lens' CreateDataQualityRuleset Prelude.Text
createDataQualityRuleset_ruleset = Lens.lens (\CreateDataQualityRuleset' {ruleset} -> ruleset) (\s@CreateDataQualityRuleset' {} a -> s {ruleset = a} :: CreateDataQualityRuleset)

instance Core.AWSRequest CreateDataQualityRuleset where
  type
    AWSResponse CreateDataQualityRuleset =
      CreateDataQualityRulesetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDataQualityRulesetResponse'
            Prelude.<$> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateDataQualityRuleset where
  hashWithSalt _salt CreateDataQualityRuleset' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetTable
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` ruleset

instance Prelude.NFData CreateDataQualityRuleset where
  rnf CreateDataQualityRuleset' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetTable
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf ruleset

instance Data.ToHeaders CreateDataQualityRuleset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSGlue.CreateDataQualityRuleset" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateDataQualityRuleset where
  toJSON CreateDataQualityRuleset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("ClientToken" Data..=) Prelude.<$> clientToken,
            ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TargetTable" Data..=) Prelude.<$> targetTable,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("Ruleset" Data..= ruleset)
          ]
      )

instance Data.ToPath CreateDataQualityRuleset where
  toPath = Prelude.const "/"

instance Data.ToQuery CreateDataQualityRuleset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDataQualityRulesetResponse' smart constructor.
data CreateDataQualityRulesetResponse = CreateDataQualityRulesetResponse'
  { -- | A unique name for the data quality ruleset.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDataQualityRulesetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'createDataQualityRulesetResponse_name' - A unique name for the data quality ruleset.
--
-- 'httpStatus', 'createDataQualityRulesetResponse_httpStatus' - The response's http status code.
newCreateDataQualityRulesetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDataQualityRulesetResponse
newCreateDataQualityRulesetResponse pHttpStatus_ =
  CreateDataQualityRulesetResponse'
    { name =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A unique name for the data quality ruleset.
createDataQualityRulesetResponse_name :: Lens.Lens' CreateDataQualityRulesetResponse (Prelude.Maybe Prelude.Text)
createDataQualityRulesetResponse_name = Lens.lens (\CreateDataQualityRulesetResponse' {name} -> name) (\s@CreateDataQualityRulesetResponse' {} a -> s {name = a} :: CreateDataQualityRulesetResponse)

-- | The response's http status code.
createDataQualityRulesetResponse_httpStatus :: Lens.Lens' CreateDataQualityRulesetResponse Prelude.Int
createDataQualityRulesetResponse_httpStatus = Lens.lens (\CreateDataQualityRulesetResponse' {httpStatus} -> httpStatus) (\s@CreateDataQualityRulesetResponse' {} a -> s {httpStatus = a} :: CreateDataQualityRulesetResponse)

instance
  Prelude.NFData
    CreateDataQualityRulesetResponse
  where
  rnf CreateDataQualityRulesetResponse' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
