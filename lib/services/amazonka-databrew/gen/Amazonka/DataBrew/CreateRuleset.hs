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
-- Module      : Amazonka.DataBrew.CreateRuleset
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new ruleset that can be used in a profile job to validate the
-- data quality of a dataset.
module Amazonka.DataBrew.CreateRuleset
  ( -- * Creating a Request
    CreateRuleset (..),
    newCreateRuleset,

    -- * Request Lenses
    createRuleset_description,
    createRuleset_tags,
    createRuleset_name,
    createRuleset_targetArn,
    createRuleset_rules,

    -- * Destructuring the Response
    CreateRulesetResponse (..),
    newCreateRulesetResponse,

    -- * Response Lenses
    createRulesetResponse_httpStatus,
    createRulesetResponse_name,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DataBrew.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateRuleset' smart constructor.
data CreateRuleset = CreateRuleset'
  { -- | The description of the ruleset.
    description :: Prelude.Maybe Prelude.Text,
    -- | Metadata tags to apply to the ruleset.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The name of the ruleset to be created. Valid characters are alphanumeric
    -- (A-Z, a-z, 0-9), hyphen (-), period (.), and space.
    name :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a resource (dataset) that the ruleset
    -- is associated with.
    targetArn :: Prelude.Text,
    -- | A list of rules that are defined with the ruleset. A rule includes one
    -- or more checks to be validated on a DataBrew dataset.
    rules :: Prelude.NonEmpty Rule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRuleset' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createRuleset_description' - The description of the ruleset.
--
-- 'tags', 'createRuleset_tags' - Metadata tags to apply to the ruleset.
--
-- 'name', 'createRuleset_name' - The name of the ruleset to be created. Valid characters are alphanumeric
-- (A-Z, a-z, 0-9), hyphen (-), period (.), and space.
--
-- 'targetArn', 'createRuleset_targetArn' - The Amazon Resource Name (ARN) of a resource (dataset) that the ruleset
-- is associated with.
--
-- 'rules', 'createRuleset_rules' - A list of rules that are defined with the ruleset. A rule includes one
-- or more checks to be validated on a DataBrew dataset.
newCreateRuleset ::
  -- | 'name'
  Prelude.Text ->
  -- | 'targetArn'
  Prelude.Text ->
  -- | 'rules'
  Prelude.NonEmpty Rule ->
  CreateRuleset
newCreateRuleset pName_ pTargetArn_ pRules_ =
  CreateRuleset'
    { description = Prelude.Nothing,
      tags = Prelude.Nothing,
      name = pName_,
      targetArn = pTargetArn_,
      rules = Lens.coerced Lens.# pRules_
    }

-- | The description of the ruleset.
createRuleset_description :: Lens.Lens' CreateRuleset (Prelude.Maybe Prelude.Text)
createRuleset_description = Lens.lens (\CreateRuleset' {description} -> description) (\s@CreateRuleset' {} a -> s {description = a} :: CreateRuleset)

-- | Metadata tags to apply to the ruleset.
createRuleset_tags :: Lens.Lens' CreateRuleset (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createRuleset_tags = Lens.lens (\CreateRuleset' {tags} -> tags) (\s@CreateRuleset' {} a -> s {tags = a} :: CreateRuleset) Prelude.. Lens.mapping Lens.coerced

-- | The name of the ruleset to be created. Valid characters are alphanumeric
-- (A-Z, a-z, 0-9), hyphen (-), period (.), and space.
createRuleset_name :: Lens.Lens' CreateRuleset Prelude.Text
createRuleset_name = Lens.lens (\CreateRuleset' {name} -> name) (\s@CreateRuleset' {} a -> s {name = a} :: CreateRuleset)

-- | The Amazon Resource Name (ARN) of a resource (dataset) that the ruleset
-- is associated with.
createRuleset_targetArn :: Lens.Lens' CreateRuleset Prelude.Text
createRuleset_targetArn = Lens.lens (\CreateRuleset' {targetArn} -> targetArn) (\s@CreateRuleset' {} a -> s {targetArn = a} :: CreateRuleset)

-- | A list of rules that are defined with the ruleset. A rule includes one
-- or more checks to be validated on a DataBrew dataset.
createRuleset_rules :: Lens.Lens' CreateRuleset (Prelude.NonEmpty Rule)
createRuleset_rules = Lens.lens (\CreateRuleset' {rules} -> rules) (\s@CreateRuleset' {} a -> s {rules = a} :: CreateRuleset) Prelude.. Lens.coerced

instance Core.AWSRequest CreateRuleset where
  type
    AWSResponse CreateRuleset =
      CreateRulesetResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateRulesetResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "Name")
      )

instance Prelude.Hashable CreateRuleset where
  hashWithSalt _salt CreateRuleset' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` targetArn
      `Prelude.hashWithSalt` rules

instance Prelude.NFData CreateRuleset where
  rnf CreateRuleset' {..} =
    Prelude.rnf description `Prelude.seq`
      Prelude.rnf tags `Prelude.seq`
        Prelude.rnf name `Prelude.seq`
          Prelude.rnf targetArn `Prelude.seq`
            Prelude.rnf rules

instance Data.ToHeaders CreateRuleset where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateRuleset where
  toJSON CreateRuleset' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("Tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("Name" Data..= name),
            Prelude.Just ("TargetArn" Data..= targetArn),
            Prelude.Just ("Rules" Data..= rules)
          ]
      )

instance Data.ToPath CreateRuleset where
  toPath = Prelude.const "/rulesets"

instance Data.ToQuery CreateRuleset where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateRulesetResponse' smart constructor.
data CreateRulesetResponse = CreateRulesetResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The unique name of the created ruleset.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateRulesetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createRulesetResponse_httpStatus' - The response's http status code.
--
-- 'name', 'createRulesetResponse_name' - The unique name of the created ruleset.
newCreateRulesetResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'name'
  Prelude.Text ->
  CreateRulesetResponse
newCreateRulesetResponse pHttpStatus_ pName_ =
  CreateRulesetResponse'
    { httpStatus = pHttpStatus_,
      name = pName_
    }

-- | The response's http status code.
createRulesetResponse_httpStatus :: Lens.Lens' CreateRulesetResponse Prelude.Int
createRulesetResponse_httpStatus = Lens.lens (\CreateRulesetResponse' {httpStatus} -> httpStatus) (\s@CreateRulesetResponse' {} a -> s {httpStatus = a} :: CreateRulesetResponse)

-- | The unique name of the created ruleset.
createRulesetResponse_name :: Lens.Lens' CreateRulesetResponse Prelude.Text
createRulesetResponse_name = Lens.lens (\CreateRulesetResponse' {name} -> name) (\s@CreateRulesetResponse' {} a -> s {name = a} :: CreateRulesetResponse)

instance Prelude.NFData CreateRulesetResponse where
  rnf CreateRulesetResponse' {..} =
    Prelude.rnf httpStatus `Prelude.seq`
      Prelude.rnf name
