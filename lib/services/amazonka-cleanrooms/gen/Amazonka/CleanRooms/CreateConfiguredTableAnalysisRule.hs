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
-- Module      : Amazonka.CleanRooms.CreateConfiguredTableAnalysisRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new analysis rule for a configured table. Currently, only one
-- analysis rule can be created for a given configured table.
module Amazonka.CleanRooms.CreateConfiguredTableAnalysisRule
  ( -- * Creating a Request
    CreateConfiguredTableAnalysisRule (..),
    newCreateConfiguredTableAnalysisRule,

    -- * Request Lenses
    createConfiguredTableAnalysisRule_configuredTableIdentifier,
    createConfiguredTableAnalysisRule_analysisRuleType,
    createConfiguredTableAnalysisRule_analysisRulePolicy,

    -- * Destructuring the Response
    CreateConfiguredTableAnalysisRuleResponse (..),
    newCreateConfiguredTableAnalysisRuleResponse,

    -- * Response Lenses
    createConfiguredTableAnalysisRuleResponse_httpStatus,
    createConfiguredTableAnalysisRuleResponse_analysisRule,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateConfiguredTableAnalysisRule' smart constructor.
data CreateConfiguredTableAnalysisRule = CreateConfiguredTableAnalysisRule'
  { -- | The identifier for the configured table to create the analysis rule for.
    -- Currently accepts the configured table ID.
    configuredTableIdentifier :: Prelude.Text,
    -- | The type of analysis rule. Valid values are AGGREGATION and LIST.
    analysisRuleType :: ConfiguredTableAnalysisRuleType,
    -- | The entire created configured table analysis rule object.
    analysisRulePolicy :: ConfiguredTableAnalysisRulePolicy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConfiguredTableAnalysisRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'configuredTableIdentifier', 'createConfiguredTableAnalysisRule_configuredTableIdentifier' - The identifier for the configured table to create the analysis rule for.
-- Currently accepts the configured table ID.
--
-- 'analysisRuleType', 'createConfiguredTableAnalysisRule_analysisRuleType' - The type of analysis rule. Valid values are AGGREGATION and LIST.
--
-- 'analysisRulePolicy', 'createConfiguredTableAnalysisRule_analysisRulePolicy' - The entire created configured table analysis rule object.
newCreateConfiguredTableAnalysisRule ::
  -- | 'configuredTableIdentifier'
  Prelude.Text ->
  -- | 'analysisRuleType'
  ConfiguredTableAnalysisRuleType ->
  -- | 'analysisRulePolicy'
  ConfiguredTableAnalysisRulePolicy ->
  CreateConfiguredTableAnalysisRule
newCreateConfiguredTableAnalysisRule
  pConfiguredTableIdentifier_
  pAnalysisRuleType_
  pAnalysisRulePolicy_ =
    CreateConfiguredTableAnalysisRule'
      { configuredTableIdentifier =
          pConfiguredTableIdentifier_,
        analysisRuleType = pAnalysisRuleType_,
        analysisRulePolicy =
          pAnalysisRulePolicy_
      }

-- | The identifier for the configured table to create the analysis rule for.
-- Currently accepts the configured table ID.
createConfiguredTableAnalysisRule_configuredTableIdentifier :: Lens.Lens' CreateConfiguredTableAnalysisRule Prelude.Text
createConfiguredTableAnalysisRule_configuredTableIdentifier = Lens.lens (\CreateConfiguredTableAnalysisRule' {configuredTableIdentifier} -> configuredTableIdentifier) (\s@CreateConfiguredTableAnalysisRule' {} a -> s {configuredTableIdentifier = a} :: CreateConfiguredTableAnalysisRule)

-- | The type of analysis rule. Valid values are AGGREGATION and LIST.
createConfiguredTableAnalysisRule_analysisRuleType :: Lens.Lens' CreateConfiguredTableAnalysisRule ConfiguredTableAnalysisRuleType
createConfiguredTableAnalysisRule_analysisRuleType = Lens.lens (\CreateConfiguredTableAnalysisRule' {analysisRuleType} -> analysisRuleType) (\s@CreateConfiguredTableAnalysisRule' {} a -> s {analysisRuleType = a} :: CreateConfiguredTableAnalysisRule)

-- | The entire created configured table analysis rule object.
createConfiguredTableAnalysisRule_analysisRulePolicy :: Lens.Lens' CreateConfiguredTableAnalysisRule ConfiguredTableAnalysisRulePolicy
createConfiguredTableAnalysisRule_analysisRulePolicy = Lens.lens (\CreateConfiguredTableAnalysisRule' {analysisRulePolicy} -> analysisRulePolicy) (\s@CreateConfiguredTableAnalysisRule' {} a -> s {analysisRulePolicy = a} :: CreateConfiguredTableAnalysisRule)

instance
  Core.AWSRequest
    CreateConfiguredTableAnalysisRule
  where
  type
    AWSResponse CreateConfiguredTableAnalysisRule =
      CreateConfiguredTableAnalysisRuleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateConfiguredTableAnalysisRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "analysisRule")
      )

instance
  Prelude.Hashable
    CreateConfiguredTableAnalysisRule
  where
  hashWithSalt
    _salt
    CreateConfiguredTableAnalysisRule' {..} =
      _salt
        `Prelude.hashWithSalt` configuredTableIdentifier
        `Prelude.hashWithSalt` analysisRuleType
        `Prelude.hashWithSalt` analysisRulePolicy

instance
  Prelude.NFData
    CreateConfiguredTableAnalysisRule
  where
  rnf CreateConfiguredTableAnalysisRule' {..} =
    Prelude.rnf configuredTableIdentifier
      `Prelude.seq` Prelude.rnf analysisRuleType
      `Prelude.seq` Prelude.rnf analysisRulePolicy

instance
  Data.ToHeaders
    CreateConfiguredTableAnalysisRule
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
    CreateConfiguredTableAnalysisRule
  where
  toJSON CreateConfiguredTableAnalysisRule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("analysisRuleType" Data..= analysisRuleType),
            Prelude.Just
              ("analysisRulePolicy" Data..= analysisRulePolicy)
          ]
      )

instance
  Data.ToPath
    CreateConfiguredTableAnalysisRule
  where
  toPath CreateConfiguredTableAnalysisRule' {..} =
    Prelude.mconcat
      [ "/configuredTables/",
        Data.toBS configuredTableIdentifier,
        "/analysisRule"
      ]

instance
  Data.ToQuery
    CreateConfiguredTableAnalysisRule
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateConfiguredTableAnalysisRuleResponse' smart constructor.
data CreateConfiguredTableAnalysisRuleResponse = CreateConfiguredTableAnalysisRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The entire created analysis rule.
    analysisRule :: ConfiguredTableAnalysisRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateConfiguredTableAnalysisRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'createConfiguredTableAnalysisRuleResponse_httpStatus' - The response's http status code.
--
-- 'analysisRule', 'createConfiguredTableAnalysisRuleResponse_analysisRule' - The entire created analysis rule.
newCreateConfiguredTableAnalysisRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'analysisRule'
  ConfiguredTableAnalysisRule ->
  CreateConfiguredTableAnalysisRuleResponse
newCreateConfiguredTableAnalysisRuleResponse
  pHttpStatus_
  pAnalysisRule_ =
    CreateConfiguredTableAnalysisRuleResponse'
      { httpStatus =
          pHttpStatus_,
        analysisRule = pAnalysisRule_
      }

-- | The response's http status code.
createConfiguredTableAnalysisRuleResponse_httpStatus :: Lens.Lens' CreateConfiguredTableAnalysisRuleResponse Prelude.Int
createConfiguredTableAnalysisRuleResponse_httpStatus = Lens.lens (\CreateConfiguredTableAnalysisRuleResponse' {httpStatus} -> httpStatus) (\s@CreateConfiguredTableAnalysisRuleResponse' {} a -> s {httpStatus = a} :: CreateConfiguredTableAnalysisRuleResponse)

-- | The entire created analysis rule.
createConfiguredTableAnalysisRuleResponse_analysisRule :: Lens.Lens' CreateConfiguredTableAnalysisRuleResponse ConfiguredTableAnalysisRule
createConfiguredTableAnalysisRuleResponse_analysisRule = Lens.lens (\CreateConfiguredTableAnalysisRuleResponse' {analysisRule} -> analysisRule) (\s@CreateConfiguredTableAnalysisRuleResponse' {} a -> s {analysisRule = a} :: CreateConfiguredTableAnalysisRuleResponse)

instance
  Prelude.NFData
    CreateConfiguredTableAnalysisRuleResponse
  where
  rnf CreateConfiguredTableAnalysisRuleResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf analysisRule
