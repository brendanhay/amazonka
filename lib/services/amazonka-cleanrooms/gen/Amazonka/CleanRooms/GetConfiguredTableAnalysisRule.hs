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
-- Module      : Amazonka.CleanRooms.GetConfiguredTableAnalysisRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a configured table analysis rule.
module Amazonka.CleanRooms.GetConfiguredTableAnalysisRule
  ( -- * Creating a Request
    GetConfiguredTableAnalysisRule (..),
    newGetConfiguredTableAnalysisRule,

    -- * Request Lenses
    getConfiguredTableAnalysisRule_configuredTableIdentifier,
    getConfiguredTableAnalysisRule_analysisRuleType,

    -- * Destructuring the Response
    GetConfiguredTableAnalysisRuleResponse (..),
    newGetConfiguredTableAnalysisRuleResponse,

    -- * Response Lenses
    getConfiguredTableAnalysisRuleResponse_httpStatus,
    getConfiguredTableAnalysisRuleResponse_analysisRule,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetConfiguredTableAnalysisRule' smart constructor.
data GetConfiguredTableAnalysisRule = GetConfiguredTableAnalysisRule'
  { -- | The unique identifier for the configured table to retrieve. Currently
    -- accepts the configured table ID.
    configuredTableIdentifier :: Prelude.Text,
    -- | The analysis rule to be retrieved. Configured table analysis rules are
    -- uniquely identified by their configured table identifier and analysis
    -- rule type.
    analysisRuleType :: ConfiguredTableAnalysisRuleType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConfiguredTableAnalysisRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuredTableIdentifier', 'getConfiguredTableAnalysisRule_configuredTableIdentifier' - The unique identifier for the configured table to retrieve. Currently
-- accepts the configured table ID.
--
-- 'analysisRuleType', 'getConfiguredTableAnalysisRule_analysisRuleType' - The analysis rule to be retrieved. Configured table analysis rules are
-- uniquely identified by their configured table identifier and analysis
-- rule type.
newGetConfiguredTableAnalysisRule ::
  -- | 'configuredTableIdentifier'
  Prelude.Text ->
  -- | 'analysisRuleType'
  ConfiguredTableAnalysisRuleType ->
  GetConfiguredTableAnalysisRule
newGetConfiguredTableAnalysisRule
  pConfiguredTableIdentifier_
  pAnalysisRuleType_ =
    GetConfiguredTableAnalysisRule'
      { configuredTableIdentifier =
          pConfiguredTableIdentifier_,
        analysisRuleType = pAnalysisRuleType_
      }

-- | The unique identifier for the configured table to retrieve. Currently
-- accepts the configured table ID.
getConfiguredTableAnalysisRule_configuredTableIdentifier :: Lens.Lens' GetConfiguredTableAnalysisRule Prelude.Text
getConfiguredTableAnalysisRule_configuredTableIdentifier = Lens.lens (\GetConfiguredTableAnalysisRule' {configuredTableIdentifier} -> configuredTableIdentifier) (\s@GetConfiguredTableAnalysisRule' {} a -> s {configuredTableIdentifier = a} :: GetConfiguredTableAnalysisRule)

-- | The analysis rule to be retrieved. Configured table analysis rules are
-- uniquely identified by their configured table identifier and analysis
-- rule type.
getConfiguredTableAnalysisRule_analysisRuleType :: Lens.Lens' GetConfiguredTableAnalysisRule ConfiguredTableAnalysisRuleType
getConfiguredTableAnalysisRule_analysisRuleType = Lens.lens (\GetConfiguredTableAnalysisRule' {analysisRuleType} -> analysisRuleType) (\s@GetConfiguredTableAnalysisRule' {} a -> s {analysisRuleType = a} :: GetConfiguredTableAnalysisRule)

instance
  Core.AWSRequest
    GetConfiguredTableAnalysisRule
  where
  type
    AWSResponse GetConfiguredTableAnalysisRule =
      GetConfiguredTableAnalysisRuleResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConfiguredTableAnalysisRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "analysisRule")
      )

instance
  Prelude.Hashable
    GetConfiguredTableAnalysisRule
  where
  hashWithSalt
    _salt
    GetConfiguredTableAnalysisRule' {..} =
      _salt
        `Prelude.hashWithSalt` configuredTableIdentifier
        `Prelude.hashWithSalt` analysisRuleType

instance
  Prelude.NFData
    GetConfiguredTableAnalysisRule
  where
  rnf GetConfiguredTableAnalysisRule' {..} =
    Prelude.rnf configuredTableIdentifier
      `Prelude.seq` Prelude.rnf analysisRuleType

instance
  Data.ToHeaders
    GetConfiguredTableAnalysisRule
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

instance Data.ToPath GetConfiguredTableAnalysisRule where
  toPath GetConfiguredTableAnalysisRule' {..} =
    Prelude.mconcat
      [ "/configuredTables/",
        Data.toBS configuredTableIdentifier,
        "/analysisRule/",
        Data.toBS analysisRuleType
      ]

instance Data.ToQuery GetConfiguredTableAnalysisRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConfiguredTableAnalysisRuleResponse' smart constructor.
data GetConfiguredTableAnalysisRuleResponse = GetConfiguredTableAnalysisRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The entire analysis rule output.
    analysisRule :: ConfiguredTableAnalysisRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConfiguredTableAnalysisRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getConfiguredTableAnalysisRuleResponse_httpStatus' - The response's http status code.
--
-- 'analysisRule', 'getConfiguredTableAnalysisRuleResponse_analysisRule' - The entire analysis rule output.
newGetConfiguredTableAnalysisRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'analysisRule'
  ConfiguredTableAnalysisRule ->
  GetConfiguredTableAnalysisRuleResponse
newGetConfiguredTableAnalysisRuleResponse
  pHttpStatus_
  pAnalysisRule_ =
    GetConfiguredTableAnalysisRuleResponse'
      { httpStatus =
          pHttpStatus_,
        analysisRule = pAnalysisRule_
      }

-- | The response's http status code.
getConfiguredTableAnalysisRuleResponse_httpStatus :: Lens.Lens' GetConfiguredTableAnalysisRuleResponse Prelude.Int
getConfiguredTableAnalysisRuleResponse_httpStatus = Lens.lens (\GetConfiguredTableAnalysisRuleResponse' {httpStatus} -> httpStatus) (\s@GetConfiguredTableAnalysisRuleResponse' {} a -> s {httpStatus = a} :: GetConfiguredTableAnalysisRuleResponse)

-- | The entire analysis rule output.
getConfiguredTableAnalysisRuleResponse_analysisRule :: Lens.Lens' GetConfiguredTableAnalysisRuleResponse ConfiguredTableAnalysisRule
getConfiguredTableAnalysisRuleResponse_analysisRule = Lens.lens (\GetConfiguredTableAnalysisRuleResponse' {analysisRule} -> analysisRule) (\s@GetConfiguredTableAnalysisRuleResponse' {} a -> s {analysisRule = a} :: GetConfiguredTableAnalysisRuleResponse)

instance
  Prelude.NFData
    GetConfiguredTableAnalysisRuleResponse
  where
  rnf GetConfiguredTableAnalysisRuleResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf analysisRule
