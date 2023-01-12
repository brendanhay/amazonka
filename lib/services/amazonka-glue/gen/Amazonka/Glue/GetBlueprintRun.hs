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
-- Module      : Amazonka.Glue.GetBlueprintRun
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the details of a blueprint run.
module Amazonka.Glue.GetBlueprintRun
  ( -- * Creating a Request
    GetBlueprintRun (..),
    newGetBlueprintRun,

    -- * Request Lenses
    getBlueprintRun_blueprintName,
    getBlueprintRun_runId,

    -- * Destructuring the Response
    GetBlueprintRunResponse (..),
    newGetBlueprintRunResponse,

    -- * Response Lenses
    getBlueprintRunResponse_blueprintRun,
    getBlueprintRunResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBlueprintRun' smart constructor.
data GetBlueprintRun = GetBlueprintRun'
  { -- | The name of the blueprint.
    blueprintName :: Prelude.Text,
    -- | The run ID for the blueprint run you want to retrieve.
    runId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBlueprintRun' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueprintName', 'getBlueprintRun_blueprintName' - The name of the blueprint.
--
-- 'runId', 'getBlueprintRun_runId' - The run ID for the blueprint run you want to retrieve.
newGetBlueprintRun ::
  -- | 'blueprintName'
  Prelude.Text ->
  -- | 'runId'
  Prelude.Text ->
  GetBlueprintRun
newGetBlueprintRun pBlueprintName_ pRunId_ =
  GetBlueprintRun'
    { blueprintName = pBlueprintName_,
      runId = pRunId_
    }

-- | The name of the blueprint.
getBlueprintRun_blueprintName :: Lens.Lens' GetBlueprintRun Prelude.Text
getBlueprintRun_blueprintName = Lens.lens (\GetBlueprintRun' {blueprintName} -> blueprintName) (\s@GetBlueprintRun' {} a -> s {blueprintName = a} :: GetBlueprintRun)

-- | The run ID for the blueprint run you want to retrieve.
getBlueprintRun_runId :: Lens.Lens' GetBlueprintRun Prelude.Text
getBlueprintRun_runId = Lens.lens (\GetBlueprintRun' {runId} -> runId) (\s@GetBlueprintRun' {} a -> s {runId = a} :: GetBlueprintRun)

instance Core.AWSRequest GetBlueprintRun where
  type
    AWSResponse GetBlueprintRun =
      GetBlueprintRunResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBlueprintRunResponse'
            Prelude.<$> (x Data..?> "BlueprintRun")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBlueprintRun where
  hashWithSalt _salt GetBlueprintRun' {..} =
    _salt `Prelude.hashWithSalt` blueprintName
      `Prelude.hashWithSalt` runId

instance Prelude.NFData GetBlueprintRun where
  rnf GetBlueprintRun' {..} =
    Prelude.rnf blueprintName
      `Prelude.seq` Prelude.rnf runId

instance Data.ToHeaders GetBlueprintRun where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetBlueprintRun" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetBlueprintRun where
  toJSON GetBlueprintRun' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("BlueprintName" Data..= blueprintName),
            Prelude.Just ("RunId" Data..= runId)
          ]
      )

instance Data.ToPath GetBlueprintRun where
  toPath = Prelude.const "/"

instance Data.ToQuery GetBlueprintRun where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBlueprintRunResponse' smart constructor.
data GetBlueprintRunResponse = GetBlueprintRunResponse'
  { -- | Returns a @BlueprintRun@ object.
    blueprintRun :: Prelude.Maybe BlueprintRun,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBlueprintRunResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'blueprintRun', 'getBlueprintRunResponse_blueprintRun' - Returns a @BlueprintRun@ object.
--
-- 'httpStatus', 'getBlueprintRunResponse_httpStatus' - The response's http status code.
newGetBlueprintRunResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBlueprintRunResponse
newGetBlueprintRunResponse pHttpStatus_ =
  GetBlueprintRunResponse'
    { blueprintRun =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a @BlueprintRun@ object.
getBlueprintRunResponse_blueprintRun :: Lens.Lens' GetBlueprintRunResponse (Prelude.Maybe BlueprintRun)
getBlueprintRunResponse_blueprintRun = Lens.lens (\GetBlueprintRunResponse' {blueprintRun} -> blueprintRun) (\s@GetBlueprintRunResponse' {} a -> s {blueprintRun = a} :: GetBlueprintRunResponse)

-- | The response's http status code.
getBlueprintRunResponse_httpStatus :: Lens.Lens' GetBlueprintRunResponse Prelude.Int
getBlueprintRunResponse_httpStatus = Lens.lens (\GetBlueprintRunResponse' {httpStatus} -> httpStatus) (\s@GetBlueprintRunResponse' {} a -> s {httpStatus = a} :: GetBlueprintRunResponse)

instance Prelude.NFData GetBlueprintRunResponse where
  rnf GetBlueprintRunResponse' {..} =
    Prelude.rnf blueprintRun
      `Prelude.seq` Prelude.rnf httpStatus
