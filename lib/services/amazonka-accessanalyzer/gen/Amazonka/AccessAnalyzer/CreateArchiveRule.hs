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
-- Module      : Amazonka.AccessAnalyzer.CreateArchiveRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an archive rule for the specified analyzer. Archive rules
-- automatically archive new findings that meet the criteria you define
-- when you create the rule.
--
-- To learn about filter keys that you can use to create an archive rule,
-- see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access-analyzer-reference-filter-keys.html IAM Access Analyzer filter keys>
-- in the __IAM User Guide__.
module Amazonka.AccessAnalyzer.CreateArchiveRule
  ( -- * Creating a Request
    CreateArchiveRule (..),
    newCreateArchiveRule,

    -- * Request Lenses
    createArchiveRule_clientToken,
    createArchiveRule_analyzerName,
    createArchiveRule_ruleName,
    createArchiveRule_filter,

    -- * Destructuring the Response
    CreateArchiveRuleResponse (..),
    newCreateArchiveRuleResponse,
  )
where

import Amazonka.AccessAnalyzer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Creates an archive rule.
--
-- /See:/ 'newCreateArchiveRule' smart constructor.
data CreateArchiveRule = CreateArchiveRule'
  { -- | A client token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the created analyzer.
    analyzerName :: Prelude.Text,
    -- | The name of the rule to create.
    ruleName :: Prelude.Text,
    -- | The criteria for the rule.
    filter' :: Prelude.HashMap Prelude.Text Criterion
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateArchiveRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'createArchiveRule_clientToken' - A client token.
--
-- 'analyzerName', 'createArchiveRule_analyzerName' - The name of the created analyzer.
--
-- 'ruleName', 'createArchiveRule_ruleName' - The name of the rule to create.
--
-- 'filter'', 'createArchiveRule_filter' - The criteria for the rule.
newCreateArchiveRule ::
  -- | 'analyzerName'
  Prelude.Text ->
  -- | 'ruleName'
  Prelude.Text ->
  CreateArchiveRule
newCreateArchiveRule pAnalyzerName_ pRuleName_ =
  CreateArchiveRule'
    { clientToken = Prelude.Nothing,
      analyzerName = pAnalyzerName_,
      ruleName = pRuleName_,
      filter' = Prelude.mempty
    }

-- | A client token.
createArchiveRule_clientToken :: Lens.Lens' CreateArchiveRule (Prelude.Maybe Prelude.Text)
createArchiveRule_clientToken = Lens.lens (\CreateArchiveRule' {clientToken} -> clientToken) (\s@CreateArchiveRule' {} a -> s {clientToken = a} :: CreateArchiveRule)

-- | The name of the created analyzer.
createArchiveRule_analyzerName :: Lens.Lens' CreateArchiveRule Prelude.Text
createArchiveRule_analyzerName = Lens.lens (\CreateArchiveRule' {analyzerName} -> analyzerName) (\s@CreateArchiveRule' {} a -> s {analyzerName = a} :: CreateArchiveRule)

-- | The name of the rule to create.
createArchiveRule_ruleName :: Lens.Lens' CreateArchiveRule Prelude.Text
createArchiveRule_ruleName = Lens.lens (\CreateArchiveRule' {ruleName} -> ruleName) (\s@CreateArchiveRule' {} a -> s {ruleName = a} :: CreateArchiveRule)

-- | The criteria for the rule.
createArchiveRule_filter :: Lens.Lens' CreateArchiveRule (Prelude.HashMap Prelude.Text Criterion)
createArchiveRule_filter = Lens.lens (\CreateArchiveRule' {filter'} -> filter') (\s@CreateArchiveRule' {} a -> s {filter' = a} :: CreateArchiveRule) Prelude.. Lens.coerced

instance Core.AWSRequest CreateArchiveRule where
  type
    AWSResponse CreateArchiveRule =
      CreateArchiveRuleResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveNull CreateArchiveRuleResponse'

instance Prelude.Hashable CreateArchiveRule where
  hashWithSalt _salt CreateArchiveRule' {..} =
    _salt
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` analyzerName
      `Prelude.hashWithSalt` ruleName
      `Prelude.hashWithSalt` filter'

instance Prelude.NFData CreateArchiveRule where
  rnf CreateArchiveRule' {..} =
    Prelude.rnf clientToken `Prelude.seq`
      Prelude.rnf analyzerName `Prelude.seq`
        Prelude.rnf ruleName `Prelude.seq`
          Prelude.rnf filter'

instance Data.ToHeaders CreateArchiveRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateArchiveRule where
  toJSON CreateArchiveRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("clientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("ruleName" Data..= ruleName),
            Prelude.Just ("filter" Data..= filter')
          ]
      )

instance Data.ToPath CreateArchiveRule where
  toPath CreateArchiveRule' {..} =
    Prelude.mconcat
      [ "/analyzer/",
        Data.toBS analyzerName,
        "/archive-rule"
      ]

instance Data.ToQuery CreateArchiveRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateArchiveRuleResponse' smart constructor.
data CreateArchiveRuleResponse = CreateArchiveRuleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateArchiveRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newCreateArchiveRuleResponse ::
  CreateArchiveRuleResponse
newCreateArchiveRuleResponse =
  CreateArchiveRuleResponse'

instance Prelude.NFData CreateArchiveRuleResponse where
  rnf _ = ()
