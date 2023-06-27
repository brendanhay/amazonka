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
-- Module      : Amazonka.CleanRooms.GetSchemaAnalysisRule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a schema analysis rule.
module Amazonka.CleanRooms.GetSchemaAnalysisRule
  ( -- * Creating a Request
    GetSchemaAnalysisRule (..),
    newGetSchemaAnalysisRule,

    -- * Request Lenses
    getSchemaAnalysisRule_collaborationIdentifier,
    getSchemaAnalysisRule_name,
    getSchemaAnalysisRule_type,

    -- * Destructuring the Response
    GetSchemaAnalysisRuleResponse (..),
    newGetSchemaAnalysisRuleResponse,

    -- * Response Lenses
    getSchemaAnalysisRuleResponse_httpStatus,
    getSchemaAnalysisRuleResponse_analysisRule,
  )
where

import Amazonka.CleanRooms.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetSchemaAnalysisRule' smart constructor.
data GetSchemaAnalysisRule = GetSchemaAnalysisRule'
  { -- | A unique identifier for the collaboration that the schema belongs to.
    -- Currently accepts a collaboration ID.
    collaborationIdentifier :: Prelude.Text,
    -- | The name of the schema to retrieve the analysis rule for.
    name :: Prelude.Text,
    -- | The type of the schema analysis rule to retrieve. Schema analysis rules
    -- are uniquely identified by a combination of the collaboration, the
    -- schema name, and their type.
    type' :: AnalysisRuleType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSchemaAnalysisRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'collaborationIdentifier', 'getSchemaAnalysisRule_collaborationIdentifier' - A unique identifier for the collaboration that the schema belongs to.
-- Currently accepts a collaboration ID.
--
-- 'name', 'getSchemaAnalysisRule_name' - The name of the schema to retrieve the analysis rule for.
--
-- 'type'', 'getSchemaAnalysisRule_type' - The type of the schema analysis rule to retrieve. Schema analysis rules
-- are uniquely identified by a combination of the collaboration, the
-- schema name, and their type.
newGetSchemaAnalysisRule ::
  -- | 'collaborationIdentifier'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'type''
  AnalysisRuleType ->
  GetSchemaAnalysisRule
newGetSchemaAnalysisRule
  pCollaborationIdentifier_
  pName_
  pType_ =
    GetSchemaAnalysisRule'
      { collaborationIdentifier =
          pCollaborationIdentifier_,
        name = pName_,
        type' = pType_
      }

-- | A unique identifier for the collaboration that the schema belongs to.
-- Currently accepts a collaboration ID.
getSchemaAnalysisRule_collaborationIdentifier :: Lens.Lens' GetSchemaAnalysisRule Prelude.Text
getSchemaAnalysisRule_collaborationIdentifier = Lens.lens (\GetSchemaAnalysisRule' {collaborationIdentifier} -> collaborationIdentifier) (\s@GetSchemaAnalysisRule' {} a -> s {collaborationIdentifier = a} :: GetSchemaAnalysisRule)

-- | The name of the schema to retrieve the analysis rule for.
getSchemaAnalysisRule_name :: Lens.Lens' GetSchemaAnalysisRule Prelude.Text
getSchemaAnalysisRule_name = Lens.lens (\GetSchemaAnalysisRule' {name} -> name) (\s@GetSchemaAnalysisRule' {} a -> s {name = a} :: GetSchemaAnalysisRule)

-- | The type of the schema analysis rule to retrieve. Schema analysis rules
-- are uniquely identified by a combination of the collaboration, the
-- schema name, and their type.
getSchemaAnalysisRule_type :: Lens.Lens' GetSchemaAnalysisRule AnalysisRuleType
getSchemaAnalysisRule_type = Lens.lens (\GetSchemaAnalysisRule' {type'} -> type') (\s@GetSchemaAnalysisRule' {} a -> s {type' = a} :: GetSchemaAnalysisRule)

instance Core.AWSRequest GetSchemaAnalysisRule where
  type
    AWSResponse GetSchemaAnalysisRule =
      GetSchemaAnalysisRuleResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSchemaAnalysisRuleResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "analysisRule")
      )

instance Prelude.Hashable GetSchemaAnalysisRule where
  hashWithSalt _salt GetSchemaAnalysisRule' {..} =
    _salt
      `Prelude.hashWithSalt` collaborationIdentifier
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'

instance Prelude.NFData GetSchemaAnalysisRule where
  rnf GetSchemaAnalysisRule' {..} =
    Prelude.rnf collaborationIdentifier
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders GetSchemaAnalysisRule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetSchemaAnalysisRule where
  toPath GetSchemaAnalysisRule' {..} =
    Prelude.mconcat
      [ "/collaborations/",
        Data.toBS collaborationIdentifier,
        "/schemas/",
        Data.toBS name,
        "/analysisRule/",
        Data.toBS type'
      ]

instance Data.ToQuery GetSchemaAnalysisRule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetSchemaAnalysisRuleResponse' smart constructor.
data GetSchemaAnalysisRuleResponse = GetSchemaAnalysisRuleResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A specification about how data from the configured table can be used.
    analysisRule :: AnalysisRule
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetSchemaAnalysisRuleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getSchemaAnalysisRuleResponse_httpStatus' - The response's http status code.
--
-- 'analysisRule', 'getSchemaAnalysisRuleResponse_analysisRule' - A specification about how data from the configured table can be used.
newGetSchemaAnalysisRuleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'analysisRule'
  AnalysisRule ->
  GetSchemaAnalysisRuleResponse
newGetSchemaAnalysisRuleResponse
  pHttpStatus_
  pAnalysisRule_ =
    GetSchemaAnalysisRuleResponse'
      { httpStatus =
          pHttpStatus_,
        analysisRule = pAnalysisRule_
      }

-- | The response's http status code.
getSchemaAnalysisRuleResponse_httpStatus :: Lens.Lens' GetSchemaAnalysisRuleResponse Prelude.Int
getSchemaAnalysisRuleResponse_httpStatus = Lens.lens (\GetSchemaAnalysisRuleResponse' {httpStatus} -> httpStatus) (\s@GetSchemaAnalysisRuleResponse' {} a -> s {httpStatus = a} :: GetSchemaAnalysisRuleResponse)

-- | A specification about how data from the configured table can be used.
getSchemaAnalysisRuleResponse_analysisRule :: Lens.Lens' GetSchemaAnalysisRuleResponse AnalysisRule
getSchemaAnalysisRuleResponse_analysisRule = Lens.lens (\GetSchemaAnalysisRuleResponse' {analysisRule} -> analysisRule) (\s@GetSchemaAnalysisRuleResponse' {} a -> s {analysisRule = a} :: GetSchemaAnalysisRuleResponse)

instance Prelude.NFData GetSchemaAnalysisRuleResponse where
  rnf GetSchemaAnalysisRuleResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf analysisRule
