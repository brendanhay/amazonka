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
-- Module      : Amazonka.AccessAnalyzer.UpdateArchiveRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the criteria and values for the specified archive rule.
module Amazonka.AccessAnalyzer.UpdateArchiveRule
  ( -- * Creating a Request
    UpdateArchiveRule (..),
    newUpdateArchiveRule,

    -- * Request Lenses
    updateArchiveRule_clientToken,
    updateArchiveRule_analyzerName,
    updateArchiveRule_ruleName,
    updateArchiveRule_filter,

    -- * Destructuring the Response
    UpdateArchiveRuleResponse (..),
    newUpdateArchiveRuleResponse,
  )
where

import Amazonka.AccessAnalyzer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Updates the specified archive rule.
--
-- /See:/ 'newUpdateArchiveRule' smart constructor.
data UpdateArchiveRule = UpdateArchiveRule'
  { -- | A client token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the analyzer to update the archive rules for.
    analyzerName :: Prelude.Text,
    -- | The name of the rule to update.
    ruleName :: Prelude.Text,
    -- | A filter to match for the rules to update. Only rules that match the
    -- filter are updated.
    filter' :: Prelude.HashMap Prelude.Text Criterion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateArchiveRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'updateArchiveRule_clientToken' - A client token.
--
-- 'analyzerName', 'updateArchiveRule_analyzerName' - The name of the analyzer to update the archive rules for.
--
-- 'ruleName', 'updateArchiveRule_ruleName' - The name of the rule to update.
--
-- 'filter'', 'updateArchiveRule_filter' - A filter to match for the rules to update. Only rules that match the
-- filter are updated.
newUpdateArchiveRule ::
  -- | 'analyzerName'
  Prelude.Text ->
  -- | 'ruleName'
  Prelude.Text ->
  UpdateArchiveRule
newUpdateArchiveRule pAnalyzerName_ pRuleName_ =
  UpdateArchiveRule'
    { clientToken = Prelude.Nothing,
      analyzerName = pAnalyzerName_,
      ruleName = pRuleName_,
      filter' = Prelude.mempty
    }

-- | A client token.
updateArchiveRule_clientToken :: Lens.Lens' UpdateArchiveRule (Prelude.Maybe Prelude.Text)
updateArchiveRule_clientToken = Lens.lens (\UpdateArchiveRule' {clientToken} -> clientToken) (\s@UpdateArchiveRule' {} a -> s {clientToken = a} :: UpdateArchiveRule)

-- | The name of the analyzer to update the archive rules for.
updateArchiveRule_analyzerName :: Lens.Lens' UpdateArchiveRule Prelude.Text
updateArchiveRule_analyzerName = Lens.lens (\UpdateArchiveRule' {analyzerName} -> analyzerName) (\s@UpdateArchiveRule' {} a -> s {analyzerName = a} :: UpdateArchiveRule)

-- | The name of the rule to update.
updateArchiveRule_ruleName :: Lens.Lens' UpdateArchiveRule Prelude.Text
updateArchiveRule_ruleName = Lens.lens (\UpdateArchiveRule' {ruleName} -> ruleName) (\s@UpdateArchiveRule' {} a -> s {ruleName = a} :: UpdateArchiveRule)

-- | A filter to match for the rules to update. Only rules that match the
-- filter are updated.
updateArchiveRule_filter :: Lens.Lens' UpdateArchiveRule (Prelude.HashMap Prelude.Text Criterion)
updateArchiveRule_filter = Lens.lens (\UpdateArchiveRule' {filter'} -> filter') (\s@UpdateArchiveRule' {} a -> s {filter' = a} :: UpdateArchiveRule) Prelude.. Lens.coerced

instance Core.AWSRequest UpdateArchiveRule where
  type
    AWSResponse UpdateArchiveRule =
      UpdateArchiveRuleResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull UpdateArchiveRuleResponse'

instance Prelude.Hashable UpdateArchiveRule where
  hashWithSalt _salt UpdateArchiveRule' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` analyzerName
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` filter'

instance Prelude.NFData UpdateArchiveRule where
  rnf UpdateArchiveRule' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf analyzerName
      `Prelude.seq` Prelude.rnf ruleName
      `Prelude.seq` Prelude.rnf filter'

instance Data.ToHeaders UpdateArchiveRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateArchiveRule where
  toJSON UpdateArchiveRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("filter" Data..= filter')
          ]
      )

instance Data.ToPath UpdateArchiveRule where
  toPath UpdateArchiveRule' {..} =
    Prelude.mconcat
      [ "/analyzer/",
        Data.toBS analyzerName,
        "/archive-rule/",
        Data.toBS ruleName
      ]

instance Data.ToQuery UpdateArchiveRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateArchiveRuleResponse' smart constructor.
data UpdateArchiveRuleResponse = UpdateArchiveRuleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateArchiveRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateArchiveRuleResponse ::
  UpdateArchiveRuleResponse
newUpdateArchiveRuleResponse =
  UpdateArchiveRuleResponse'

instance Prelude.NFData UpdateArchiveRuleResponse where
  rnf _ = ()
