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
-- Module      : Amazonka.CleanRooms.UpdateConfiguredTableAnalysisRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a configured table analysis rule.
module Amazonka.CleanRooms.UpdateConfiguredTableAnalysisRule
  ( -- * Creating a Request
    UpdateConfiguredTableAnalysisRule (..),
    newUpdateConfiguredTableAnalysisRule,

    -- * Request Lenses
    updateConfiguredTableAnalysisRule_configuredTableIdentifier,
    updateConfiguredTableAnalysisRule_analysisRuleType,
    updateConfiguredTableAnalysisRule_analysisRulePolicy,

    -- * Destructuring the Response
    UpdateConfiguredTableAnalysisRuleResponse (..),
    newUpdateConfiguredTableAnalysisRuleResponse,

    -- * Response Lenses
    updateConfiguredTableAnalysisRuleResponse_httpStatus,
    updateConfiguredTableAnalysisRuleResponse_analysisRule,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateConfiguredTableAnalysisRule' smart constructor.
data UpdateConfiguredTableAnalysisRule = UpdateConfiguredTableAnalysisRule'
  { -- | The unique identifier for the configured table that the analysis rule
    -- applies to. Currently accepts the configured table ID.
    configuredTableIdentifier :: Prelude.Text,
    -- | The analysis rule type to be updated. Configured table analysis rules
    -- are uniquely identified by their configured table identifier and
    -- analysis rule type.
    analysisRuleType :: ConfiguredTableAnalysisRuleType,
    -- | The new analysis rule policy for the configured table analysis rule.
    analysisRulePolicy :: ConfiguredTableAnalysisRulePolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfiguredTableAnalysisRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuredTableIdentifier', 'updateConfiguredTableAnalysisRule_configuredTableIdentifier' - The unique identifier for the configured table that the analysis rule
-- applies to. Currently accepts the configured table ID.
--
-- 'analysisRuleType', 'updateConfiguredTableAnalysisRule_analysisRuleType' - The analysis rule type to be updated. Configured table analysis rules
-- are uniquely identified by their configured table identifier and
-- analysis rule type.
--
-- 'analysisRulePolicy', 'updateConfiguredTableAnalysisRule_analysisRulePolicy' - The new analysis rule policy for the configured table analysis rule.
newUpdateConfiguredTableAnalysisRule ::
  -- | 'configuredTableIdentifier'
  Prelude.Text ->
  -- | 'analysisRuleType'
  ConfiguredTableAnalysisRuleType ->
  -- | 'analysisRulePolicy'
  ConfiguredTableAnalysisRulePolicy ->
  UpdateConfiguredTableAnalysisRule
newUpdateConfiguredTableAnalysisRule
  pConfiguredTableIdentifier_
  pAnalysisRuleType_
  pAnalysisRulePolicy_ =
    UpdateConfiguredTableAnalysisRule'
      { configuredTableIdentifier =
          pConfiguredTableIdentifier_,
        analysisRuleType = pAnalysisRuleType_,
        analysisRulePolicy =
          pAnalysisRulePolicy_
      }

-- | The unique identifier for the configured table that the analysis rule
-- applies to. Currently accepts the configured table ID.
updateConfiguredTableAnalysisRule_configuredTableIdentifier :: Lens.Lens' UpdateConfiguredTableAnalysisRule Prelude.Text
updateConfiguredTableAnalysisRule_configuredTableIdentifier = Lens.lens (\UpdateConfiguredTableAnalysisRule' {configuredTableIdentifier} -> configuredTableIdentifier) (\s@UpdateConfiguredTableAnalysisRule' {} a -> s {configuredTableIdentifier = a} :: UpdateConfiguredTableAnalysisRule)

-- | The analysis rule type to be updated. Configured table analysis rules
-- are uniquely identified by their configured table identifier and
-- analysis rule type.
updateConfiguredTableAnalysisRule_analysisRuleType :: Lens.Lens' UpdateConfiguredTableAnalysisRule ConfiguredTableAnalysisRuleType
updateConfiguredTableAnalysisRule_analysisRuleType = Lens.lens (\UpdateConfiguredTableAnalysisRule' {analysisRuleType} -> analysisRuleType) (\s@UpdateConfiguredTableAnalysisRule' {} a -> s {analysisRuleType = a} :: UpdateConfiguredTableAnalysisRule)

-- | The new analysis rule policy for the configured table analysis rule.
updateConfiguredTableAnalysisRule_analysisRulePolicy :: Lens.Lens' UpdateConfiguredTableAnalysisRule ConfiguredTableAnalysisRulePolicy
updateConfiguredTableAnalysisRule_analysisRulePolicy = Lens.lens (\UpdateConfiguredTableAnalysisRule' {analysisRulePolicy} -> analysisRulePolicy) (\s@UpdateConfiguredTableAnalysisRule' {} a -> s {analysisRulePolicy = a} :: UpdateConfiguredTableAnalysisRule)

instance
  Core.AWSRequest
    UpdateConfiguredTableAnalysisRule
  where
  type
    AWSResponse UpdateConfiguredTableAnalysisRule =
      UpdateConfiguredTableAnalysisRuleResponse
  request overrides =
    Request.patchJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateConfiguredTableAnalysisRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "analysisRule")
      )

instance
  Prelude.Hashable
    UpdateConfiguredTableAnalysisRule
  where
  hashWithSalt
    _salt
    UpdateConfiguredTableAnalysisRule' {..} =
      _salt
        `Prelude.hashWithSalt` configuredTableIdentifier
        `Prelude.hashWithSalt` analysisRuleType
        `Prelude.hashWithSalt` analysisRulePolicy

instance
  Prelude.NFData
    UpdateConfiguredTableAnalysisRule
  where
  rnf UpdateConfiguredTableAnalysisRule' {..} =
    Prelude.rnf configuredTableIdentifier
      `Prelude.seq` Prelude.rnf analysisRuleType
      `Prelude.seq` Prelude.rnf analysisRulePolicy

instance
  Data.ToHeaders
    UpdateConfiguredTableAnalysisRule
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
  Data.ToJSON
    UpdateConfiguredTableAnalysisRule
  where
  toJSON UpdateConfiguredTableAnalysisRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("analysisRulePolicy" Data..= analysisRulePolicy)
          ]
      )

instance
  Data.ToPath
    UpdateConfiguredTableAnalysisRule
  where
  toPath UpdateConfiguredTableAnalysisRule' {..} =
    Prelude.mconcat
      [ "/configuredTables/",
        Data.toBS configuredTableIdentifier,
        "/analysisRule/",
        Data.toBS analysisRuleType
      ]

instance
  Data.ToQuery
    UpdateConfiguredTableAnalysisRule
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateConfiguredTableAnalysisRuleResponse' smart constructor.
data UpdateConfiguredTableAnalysisRuleResponse = UpdateConfiguredTableAnalysisRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The entire updated analysis rule.
    analysisRule :: ConfiguredTableAnalysisRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateConfiguredTableAnalysisRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateConfiguredTableAnalysisRuleResponse_httpStatus' - The response's http status code.
--
-- 'analysisRule', 'updateConfiguredTableAnalysisRuleResponse_analysisRule' - The entire updated analysis rule.
newUpdateConfiguredTableAnalysisRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'analysisRule'
  ConfiguredTableAnalysisRule ->
  UpdateConfiguredTableAnalysisRuleResponse
newUpdateConfiguredTableAnalysisRuleResponse
  pHttpStatus_
  pAnalysisRule_ =
    UpdateConfiguredTableAnalysisRuleResponse'
      { httpStatus =
          pHttpStatus_,
        analysisRule = pAnalysisRule_
      }

-- | The response's http status code.
updateConfiguredTableAnalysisRuleResponse_httpStatus :: Lens.Lens' UpdateConfiguredTableAnalysisRuleResponse Prelude.Int
updateConfiguredTableAnalysisRuleResponse_httpStatus = Lens.lens (\UpdateConfiguredTableAnalysisRuleResponse' {httpStatus} -> httpStatus) (\s@UpdateConfiguredTableAnalysisRuleResponse' {} a -> s {httpStatus = a} :: UpdateConfiguredTableAnalysisRuleResponse)

-- | The entire updated analysis rule.
updateConfiguredTableAnalysisRuleResponse_analysisRule :: Lens.Lens' UpdateConfiguredTableAnalysisRuleResponse ConfiguredTableAnalysisRule
updateConfiguredTableAnalysisRuleResponse_analysisRule = Lens.lens (\UpdateConfiguredTableAnalysisRuleResponse' {analysisRule} -> analysisRule) (\s@UpdateConfiguredTableAnalysisRuleResponse' {} a -> s {analysisRule = a} :: UpdateConfiguredTableAnalysisRuleResponse)

instance
  Prelude.NFData
    UpdateConfiguredTableAnalysisRuleResponse
  where
  rnf UpdateConfiguredTableAnalysisRuleResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf analysisRule
