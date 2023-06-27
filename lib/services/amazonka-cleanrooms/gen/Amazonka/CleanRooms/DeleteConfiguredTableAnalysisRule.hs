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
-- Module      : Amazonka.CleanRooms.DeleteConfiguredTableAnalysisRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a configured table analysis rule.
module Amazonka.CleanRooms.DeleteConfiguredTableAnalysisRule
  ( -- * Creating a Request
    DeleteConfiguredTableAnalysisRule (..),
    newDeleteConfiguredTableAnalysisRule,

    -- * Request Lenses
    deleteConfiguredTableAnalysisRule_configuredTableIdentifier,
    deleteConfiguredTableAnalysisRule_analysisRuleType,

    -- * Destructuring the Response
    DeleteConfiguredTableAnalysisRuleResponse (..),
    newDeleteConfiguredTableAnalysisRuleResponse,

    -- * Response Lenses
    deleteConfiguredTableAnalysisRuleResponse_httpStatus,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteConfiguredTableAnalysisRule' smart constructor.
data DeleteConfiguredTableAnalysisRule = DeleteConfiguredTableAnalysisRule'
  { -- | The unique identifier for the configured table that the analysis rule
    -- applies to. Currently accepts the configured table ID.
    configuredTableIdentifier :: Prelude.Text,
    -- | The analysis rule type to be deleted. Configured table analysis rules
    -- are uniquely identified by their configured table identifier and
    -- analysis rule type.
    analysisRuleType :: ConfiguredTableAnalysisRuleType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfiguredTableAnalysisRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuredTableIdentifier', 'deleteConfiguredTableAnalysisRule_configuredTableIdentifier' - The unique identifier for the configured table that the analysis rule
-- applies to. Currently accepts the configured table ID.
--
-- 'analysisRuleType', 'deleteConfiguredTableAnalysisRule_analysisRuleType' - The analysis rule type to be deleted. Configured table analysis rules
-- are uniquely identified by their configured table identifier and
-- analysis rule type.
newDeleteConfiguredTableAnalysisRule ::
  -- | 'configuredTableIdentifier'
  Prelude.Text ->
  -- | 'analysisRuleType'
  ConfiguredTableAnalysisRuleType ->
  DeleteConfiguredTableAnalysisRule
newDeleteConfiguredTableAnalysisRule
  pConfiguredTableIdentifier_
  pAnalysisRuleType_ =
    DeleteConfiguredTableAnalysisRule'
      { configuredTableIdentifier =
          pConfiguredTableIdentifier_,
        analysisRuleType = pAnalysisRuleType_
      }

-- | The unique identifier for the configured table that the analysis rule
-- applies to. Currently accepts the configured table ID.
deleteConfiguredTableAnalysisRule_configuredTableIdentifier :: Lens.Lens' DeleteConfiguredTableAnalysisRule Prelude.Text
deleteConfiguredTableAnalysisRule_configuredTableIdentifier = Lens.lens (\DeleteConfiguredTableAnalysisRule' {configuredTableIdentifier} -> configuredTableIdentifier) (\s@DeleteConfiguredTableAnalysisRule' {} a -> s {configuredTableIdentifier = a} :: DeleteConfiguredTableAnalysisRule)

-- | The analysis rule type to be deleted. Configured table analysis rules
-- are uniquely identified by their configured table identifier and
-- analysis rule type.
deleteConfiguredTableAnalysisRule_analysisRuleType :: Lens.Lens' DeleteConfiguredTableAnalysisRule ConfiguredTableAnalysisRuleType
deleteConfiguredTableAnalysisRule_analysisRuleType = Lens.lens (\DeleteConfiguredTableAnalysisRule' {analysisRuleType} -> analysisRuleType) (\s@DeleteConfiguredTableAnalysisRule' {} a -> s {analysisRuleType = a} :: DeleteConfiguredTableAnalysisRule)

instance
  Core.AWSRequest
    DeleteConfiguredTableAnalysisRule
  where
  type
    AWSResponse DeleteConfiguredTableAnalysisRule =
      DeleteConfiguredTableAnalysisRuleResponse
  request overrides =
    Request.delete (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          DeleteConfiguredTableAnalysisRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DeleteConfiguredTableAnalysisRule
  where
  hashWithSalt
    _salt
    DeleteConfiguredTableAnalysisRule' {..} =
      _salt
        `Prelude.hashWithSalt` configuredTableIdentifier
        `Prelude.hashWithSalt` analysisRuleType

instance
  Prelude.NFData
    DeleteConfiguredTableAnalysisRule
  where
  rnf DeleteConfiguredTableAnalysisRule' {..} =
    Prelude.rnf configuredTableIdentifier
      `Prelude.seq` Prelude.rnf analysisRuleType

instance
  Data.ToHeaders
    DeleteConfiguredTableAnalysisRule
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToPath
    DeleteConfiguredTableAnalysisRule
  where
  toPath DeleteConfiguredTableAnalysisRule' {..} =
    Prelude.mconcat
      [ "/configuredTables/",
        Data.toBS configuredTableIdentifier,
        "/analysisRule/",
        Data.toBS analysisRuleType
      ]

instance
  Data.ToQuery
    DeleteConfiguredTableAnalysisRule
  where
  toQuery = Prelude.const Prelude.mempty

-- | An empty response that indicates a successful delete.
--
-- /See:/ 'newDeleteConfiguredTableAnalysisRuleResponse' smart constructor.
data DeleteConfiguredTableAnalysisRuleResponse = DeleteConfiguredTableAnalysisRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeleteConfiguredTableAnalysisRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'deleteConfiguredTableAnalysisRuleResponse_httpStatus' - The response's http status code.
newDeleteConfiguredTableAnalysisRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DeleteConfiguredTableAnalysisRuleResponse
newDeleteConfiguredTableAnalysisRuleResponse
  pHttpStatus_ =
    DeleteConfiguredTableAnalysisRuleResponse'
      { httpStatus =
          pHttpStatus_
      }

-- | The response's http status code.
deleteConfiguredTableAnalysisRuleResponse_httpStatus :: Lens.Lens' DeleteConfiguredTableAnalysisRuleResponse Prelude.Int
deleteConfiguredTableAnalysisRuleResponse_httpStatus = Lens.lens (\DeleteConfiguredTableAnalysisRuleResponse' {httpStatus} -> httpStatus) (\s@DeleteConfiguredTableAnalysisRuleResponse' {} a -> s {httpStatus = a} :: DeleteConfiguredTableAnalysisRuleResponse)

instance
  Prelude.NFData
    DeleteConfiguredTableAnalysisRuleResponse
  where
  rnf DeleteConfiguredTableAnalysisRuleResponse' {..} =
    Prelude.rnf httpStatus
