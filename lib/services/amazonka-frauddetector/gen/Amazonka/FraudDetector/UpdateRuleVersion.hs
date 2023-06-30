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
-- Module      : Amazonka.FraudDetector.UpdateRuleVersion
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a rule version resulting in a new rule version. Updates a rule
-- version resulting in a new rule version (version 1, 2, 3 ...).
module Amazonka.FraudDetector.UpdateRuleVersion
  ( -- * Creating a Request
    UpdateRuleVersion (..),
    newUpdateRuleVersion,

    -- * Request Lenses
    updateRuleVersion_description,
    updateRuleVersion_tags,
    updateRuleVersion_rule,
    updateRuleVersion_expression,
    updateRuleVersion_language,
    updateRuleVersion_outcomes,

    -- * Destructuring the Response
    UpdateRuleVersionResponse (..),
    newUpdateRuleVersionResponse,

    -- * Response Lenses
    updateRuleVersionResponse_rule,
    updateRuleVersionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.FraudDetector.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateRuleVersion' smart constructor.
data UpdateRuleVersion = UpdateRuleVersion'
  { -- | The description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The tags to assign to the rule version.
    tags :: Prelude.Maybe [Tag],
    -- | The rule to update.
    rule :: Rule,
    -- | The rule expression.
    expression :: Data.Sensitive Prelude.Text,
    -- | The language.
    language :: Language,
    -- | The outcomes.
    outcomes :: Prelude.NonEmpty Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRuleVersion' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updateRuleVersion_description' - The description.
--
-- 'tags', 'updateRuleVersion_tags' - The tags to assign to the rule version.
--
-- 'rule', 'updateRuleVersion_rule' - The rule to update.
--
-- 'expression', 'updateRuleVersion_expression' - The rule expression.
--
-- 'language', 'updateRuleVersion_language' - The language.
--
-- 'outcomes', 'updateRuleVersion_outcomes' - The outcomes.
newUpdateRuleVersion ::
  -- | 'rule'
  Rule ->
  -- | 'expression'
  Prelude.Text ->
  -- | 'language'
  Language ->
  -- | 'outcomes'
  Prelude.NonEmpty Prelude.Text ->
  UpdateRuleVersion
newUpdateRuleVersion
  pRule_
  pExpression_
  pLanguage_
  pOutcomes_ =
    UpdateRuleVersion'
      { description = Prelude.Nothing,
        tags = Prelude.Nothing,
        rule = pRule_,
        expression = Data._Sensitive Lens.# pExpression_,
        language = pLanguage_,
        outcomes = Lens.coerced Lens.# pOutcomes_
      }

-- | The description.
updateRuleVersion_description :: Lens.Lens' UpdateRuleVersion (Prelude.Maybe Prelude.Text)
updateRuleVersion_description = Lens.lens (\UpdateRuleVersion' {description} -> description) (\s@UpdateRuleVersion' {} a -> s {description = a} :: UpdateRuleVersion)

-- | The tags to assign to the rule version.
updateRuleVersion_tags :: Lens.Lens' UpdateRuleVersion (Prelude.Maybe [Tag])
updateRuleVersion_tags = Lens.lens (\UpdateRuleVersion' {tags} -> tags) (\s@UpdateRuleVersion' {} a -> s {tags = a} :: UpdateRuleVersion) Prelude.. Lens.mapping Lens.coerced

-- | The rule to update.
updateRuleVersion_rule :: Lens.Lens' UpdateRuleVersion Rule
updateRuleVersion_rule = Lens.lens (\UpdateRuleVersion' {rule} -> rule) (\s@UpdateRuleVersion' {} a -> s {rule = a} :: UpdateRuleVersion)

-- | The rule expression.
updateRuleVersion_expression :: Lens.Lens' UpdateRuleVersion Prelude.Text
updateRuleVersion_expression = Lens.lens (\UpdateRuleVersion' {expression} -> expression) (\s@UpdateRuleVersion' {} a -> s {expression = a} :: UpdateRuleVersion) Prelude.. Data._Sensitive

-- | The language.
updateRuleVersion_language :: Lens.Lens' UpdateRuleVersion Language
updateRuleVersion_language = Lens.lens (\UpdateRuleVersion' {language} -> language) (\s@UpdateRuleVersion' {} a -> s {language = a} :: UpdateRuleVersion)

-- | The outcomes.
updateRuleVersion_outcomes :: Lens.Lens' UpdateRuleVersion (Prelude.NonEmpty Prelude.Text)
updateRuleVersion_outcomes = Lens.lens (\UpdateRuleVersion' {outcomes} -> outcomes) (\s@UpdateRuleVersion' {} a -> s {outcomes = a} :: UpdateRuleVersion) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateRuleVersion where
  type
    AWSResponse UpdateRuleVersion =
      UpdateRuleVersionResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateRuleVersionResponse'
            Prelude.<$> (x Data..?> "rule")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateRuleVersion where
  hashWithSalt _salt UpdateRuleVersion' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` rule
      `Prelude.hashWithSalt` expression
      `Prelude.hashWithSalt` language
      `Prelude.hashWithSalt` outcomes

instance Prelude.NFData UpdateRuleVersion where
  rnf UpdateRuleVersion' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf rule
      `Prelude.seq` Prelude.rnf expression
      `Prelude.seq` Prelude.rnf language
      `Prelude.seq` Prelude.rnf outcomes

instance Data.ToHeaders UpdateRuleVersion where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSHawksNestServiceFacade.UpdateRuleVersion" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateRuleVersion where
  toJSON UpdateRuleVersion' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("description" Data..=) Prelude.<$> description,
            ("tags" Data..=) Prelude.<$> tags,
            Prelude.Just ("rule" Data..= rule),
            Prelude.Just ("expression" Data..= expression),
            Prelude.Just ("language" Data..= language),
            Prelude.Just ("outcomes" Data..= outcomes)
          ]
      )

instance Data.ToPath UpdateRuleVersion where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateRuleVersion where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateRuleVersionResponse' smart constructor.
data UpdateRuleVersionResponse = UpdateRuleVersionResponse'
  { -- | The new rule version that was created.
    rule :: Prelude.Maybe Rule,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateRuleVersionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'rule', 'updateRuleVersionResponse_rule' - The new rule version that was created.
--
-- 'httpStatus', 'updateRuleVersionResponse_httpStatus' - The response's http status code.
newUpdateRuleVersionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateRuleVersionResponse
newUpdateRuleVersionResponse pHttpStatus_ =
  UpdateRuleVersionResponse'
    { rule = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The new rule version that was created.
updateRuleVersionResponse_rule :: Lens.Lens' UpdateRuleVersionResponse (Prelude.Maybe Rule)
updateRuleVersionResponse_rule = Lens.lens (\UpdateRuleVersionResponse' {rule} -> rule) (\s@UpdateRuleVersionResponse' {} a -> s {rule = a} :: UpdateRuleVersionResponse)

-- | The response's http status code.
updateRuleVersionResponse_httpStatus :: Lens.Lens' UpdateRuleVersionResponse Prelude.Int
updateRuleVersionResponse_httpStatus = Lens.lens (\UpdateRuleVersionResponse' {httpStatus} -> httpStatus) (\s@UpdateRuleVersionResponse' {} a -> s {httpStatus = a} :: UpdateRuleVersionResponse)

instance Prelude.NFData UpdateRuleVersionResponse where
  rnf UpdateRuleVersionResponse' {..} =
    Prelude.rnf rule
      `Prelude.seq` Prelude.rnf httpStatus
