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
-- Module      : Amazonka.AccessAnalyzer.DeleteArchiveRule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified archive rule.
module Amazonka.AccessAnalyzer.DeleteArchiveRule
  ( -- * Creating a Request
    DeleteArchiveRule (..),
    newDeleteArchiveRule,

    -- * Request Lenses
    deleteArchiveRule_clientToken,
    deleteArchiveRule_analyzerName,
    deleteArchiveRule_ruleName,

    -- * Destructuring the Response
    DeleteArchiveRuleResponse (..),
    newDeleteArchiveRuleResponse,
  )
where

import Amazonka.AccessAnalyzer.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Deletes an archive rule.
--
-- /See:/ 'newDeleteArchiveRule' smart constructor.
data DeleteArchiveRule = DeleteArchiveRule'
  { -- | A client token.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the analyzer that associated with the archive rule to
    -- delete.
    analyzerName :: Prelude.Text,
    -- | The name of the rule to delete.
    ruleName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteArchiveRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientToken', 'deleteArchiveRule_clientToken' - A client token.
--
-- 'analyzerName', 'deleteArchiveRule_analyzerName' - The name of the analyzer that associated with the archive rule to
-- delete.
--
-- 'ruleName', 'deleteArchiveRule_ruleName' - The name of the rule to delete.
newDeleteArchiveRule ::
  -- | 'analyzerName'
  Prelude.Text ->
  -- | 'ruleName'
  Prelude.Text ->
  DeleteArchiveRule
newDeleteArchiveRule pAnalyzerName_ pRuleName_ =
  DeleteArchiveRule'
    { clientToken = Prelude.Nothing,
      analyzerName = pAnalyzerName_,
      ruleName = pRuleName_
    }

-- | A client token.
deleteArchiveRule_clientToken :: Lens.Lens' DeleteArchiveRule (Prelude.Maybe Prelude.Text)
deleteArchiveRule_clientToken = Lens.lens (\DeleteArchiveRule' {clientToken} -> clientToken) (\s@DeleteArchiveRule' {} a -> s {clientToken = a} :: DeleteArchiveRule)

-- | The name of the analyzer that associated with the archive rule to
-- delete.
deleteArchiveRule_analyzerName :: Lens.Lens' DeleteArchiveRule Prelude.Text
deleteArchiveRule_analyzerName = Lens.lens (\DeleteArchiveRule' {analyzerName} -> analyzerName) (\s@DeleteArchiveRule' {} a -> s {analyzerName = a} :: DeleteArchiveRule)

-- | The name of the rule to delete.
deleteArchiveRule_ruleName :: Lens.Lens' DeleteArchiveRule Prelude.Text
deleteArchiveRule_ruleName = Lens.lens (\DeleteArchiveRule' {ruleName} -> ruleName) (\s@DeleteArchiveRule' {} a -> s {ruleName = a} :: DeleteArchiveRule)

instance Core.AWSRequest DeleteArchiveRule where
  type
    AWSResponse DeleteArchiveRule =
      DeleteArchiveRuleResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveNull DeleteArchiveRuleResponse'

instance Prelude.Hashable DeleteArchiveRule where
  hashWithSalt _salt DeleteArchiveRule' {..} =
    _salt `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` analyzerName
      `Prelude.hashWithSalt` ruleName

instance Prelude.NFData DeleteArchiveRule where
  rnf DeleteArchiveRule' {..} =
    Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf analyzerName
      `Prelude.seq` Prelude.rnf ruleName

instance Core.ToHeaders DeleteArchiveRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath DeleteArchiveRule where
  toPath DeleteArchiveRule' {..} =
    Prelude.mconcat
      [ "/analyzer/",
        Core.toBS analyzerName,
        "/archive-rule/",
        Core.toBS ruleName
      ]

instance Core.ToQuery DeleteArchiveRule where
  toQuery DeleteArchiveRule' {..} =
    Prelude.mconcat ["clientToken" Core.=: clientToken]

-- | /See:/ 'newDeleteArchiveRuleResponse' smart constructor.
data DeleteArchiveRuleResponse = DeleteArchiveRuleResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteArchiveRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newDeleteArchiveRuleResponse ::
  DeleteArchiveRuleResponse
newDeleteArchiveRuleResponse =
  DeleteArchiveRuleResponse'

instance Prelude.NFData DeleteArchiveRuleResponse where
  rnf _ = ()
