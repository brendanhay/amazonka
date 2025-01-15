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
-- Module      : Amazonka.DataBrew.UpdateRuleset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates specified ruleset.
module Amazonka.DataBrew.UpdateRuleset
  ( -- * Creating a Request
    UpdateRuleset (..),
    newUpdateRuleset,

    -- * Request Lenses
    updateRuleset_description,
    updateRuleset_name,
    updateRuleset_rules,

    -- * Destructuring the Response
    UpdateRulesetResponse (..),
    newUpdateRulesetResponse,

    -- * Response Lenses
    updateRulesetResponse_httpStatus,
    updateRulesetResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRuleset' smart constructor.
data UpdateRuleset = UpdateRuleset'
  { -- | The description of the ruleset.
    description :: Prelude.Maybe Prelude.Text,
    -- | The name of the ruleset to be updated.
    name :: Prelude.Text,
    -- | A list of rules that are defined with the ruleset. A rule includes one
    -- or more checks to be validated on a DataBrew dataset.
    rules :: Prelude.NonEmpty Rule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRuleset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateRuleset_description' - The description of the ruleset.
--
-- 'name', 'updateRuleset_name' - The name of the ruleset to be updated.
--
-- 'rules', 'updateRuleset_rules' - A list of rules that are defined with the ruleset. A rule includes one
-- or more checks to be validated on a DataBrew dataset.
newUpdateRuleset ::
  -- | 'name'
  Prelude.Text ->
  -- | 'rules'
  Prelude.NonEmpty Rule ->
  UpdateRuleset
newUpdateRuleset pName_ pRules_ =
  UpdateRuleset'
    { description = Prelude.Nothing,
      name = pName_,
      rules = Lens.coerced Lens.# pRules_
    }

-- | The description of the ruleset.
updateRuleset_description :: Lens.Lens' UpdateRuleset (Prelude.Maybe Prelude.Text)
updateRuleset_description = Lens.lens (\UpdateRuleset' {description} -> description) (\s@UpdateRuleset' {} a -> s {description = a} :: UpdateRuleset)

-- | The name of the ruleset to be updated.
updateRuleset_name :: Lens.Lens' UpdateRuleset Prelude.Text
updateRuleset_name = Lens.lens (\UpdateRuleset' {name} -> name) (\s@UpdateRuleset' {} a -> s {name = a} :: UpdateRuleset)

-- | A list of rules that are defined with the ruleset. A rule includes one
-- or more checks to be validated on a DataBrew dataset.
updateRuleset_rules :: Lens.Lens' UpdateRuleset (Prelude.NonEmpty Rule)
updateRuleset_rules = Lens.lens (\UpdateRuleset' {rules} -> rules) (\s@UpdateRuleset' {} a -> s {rules = a} :: UpdateRuleset) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateRuleset where
  type
    AWSResponse UpdateRuleset =
      UpdateRulesetResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRulesetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Name")
      )

instance Prelude.Hashable UpdateRuleset where
  hashWithSalt _salt UpdateRuleset' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` rules

instance Prelude.NFData UpdateRuleset where
  rnf UpdateRuleset' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf name `Prelude.seq`
        Prelude.rnf rules

instance Data.ToHeaders UpdateRuleset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRuleset where
  toJSON UpdateRuleset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            Prelude.Just ("Rules" Data..= rules)
          ]
      )

instance Data.ToPath UpdateRuleset where
  toPath UpdateRuleset' {..} =
    Prelude.mconcat ["/rulesets/", Data.toBS name]

instance Data.ToQuery UpdateRuleset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRulesetResponse' smart constructor.
data UpdateRulesetResponse = UpdateRulesetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The name of the updated ruleset.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRulesetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateRulesetResponse_httpStatus' - The response's http status code.
--
-- 'name', 'updateRulesetResponse_name' - The name of the updated ruleset.
newUpdateRulesetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  UpdateRulesetResponse
newUpdateRulesetResponse pHttpStatus_ pName_ =
  UpdateRulesetResponse'
    { httpStatus = pHttpStatus_,
      name = pName_
    }

-- | The response's http status code.
updateRulesetResponse_httpStatus :: Lens.Lens' UpdateRulesetResponse Prelude.Int
updateRulesetResponse_httpStatus = Lens.lens (\UpdateRulesetResponse' {httpStatus} -> httpStatus) (\s@UpdateRulesetResponse' {} a -> s {httpStatus = a} :: UpdateRulesetResponse)

-- | The name of the updated ruleset.
updateRulesetResponse_name :: Lens.Lens' UpdateRulesetResponse Prelude.Text
updateRulesetResponse_name = Lens.lens (\UpdateRulesetResponse' {name} -> name) (\s@UpdateRulesetResponse' {} a -> s {name = a} :: UpdateRulesetResponse)

instance Prelude.NFData UpdateRulesetResponse where
  rnf UpdateRulesetResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf name
