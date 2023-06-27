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
-- Module      : Amazonka.IVSRealtime.GetStage
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information for the specified stage.
module Amazonka.IVSRealtime.GetStage
  ( -- * Creating a Request
    GetStage (..),
    newGetStage,

    -- * Request Lenses
    getStage_arn,

    -- * Destructuring the Response
    GetStageResponse (..),
    newGetStageResponse,

    -- * Response Lenses
    getStageResponse_stage,
    getStageResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IVSRealtime.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetStage' smart constructor.
data GetStage = GetStage'
  { -- | ARN of the stage for which the information is to be retrieved.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStage' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getStage_arn' - ARN of the stage for which the information is to be retrieved.
newGetStage ::
  -- | 'arn'
  Prelude.Text ->
  GetStage
newGetStage pArn_ = GetStage' {arn = pArn_}

-- | ARN of the stage for which the information is to be retrieved.
getStage_arn :: Lens.Lens' GetStage Prelude.Text
getStage_arn = Lens.lens (\GetStage' {arn} -> arn) (\s@GetStage' {} a -> s {arn = a} :: GetStage)

instance Core.AWSRequest GetStage where
  type AWSResponse GetStage = GetStageResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStageResponse'
            Prelude.<$> (x Data..?> "stage")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetStage where
  hashWithSalt _salt GetStage' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData GetStage where
  rnf GetStage' {..} = Prelude.rnf arn

instance Data.ToHeaders GetStage where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetStage where
  toJSON GetStage' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("arn" Data..= arn)]
      )

instance Data.ToPath GetStage where
  toPath = Prelude.const "/GetStage"

instance Data.ToQuery GetStage where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetStageResponse' smart constructor.
data GetStageResponse = GetStageResponse'
  { -- | The stage that is returned.
    stage :: Prelude.Maybe Stage,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetStageResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'stage', 'getStageResponse_stage' - The stage that is returned.
--
-- 'httpStatus', 'getStageResponse_httpStatus' - The response's http status code.
newGetStageResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetStageResponse
newGetStageResponse pHttpStatus_ =
  GetStageResponse'
    { stage = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The stage that is returned.
getStageResponse_stage :: Lens.Lens' GetStageResponse (Prelude.Maybe Stage)
getStageResponse_stage = Lens.lens (\GetStageResponse' {stage} -> stage) (\s@GetStageResponse' {} a -> s {stage = a} :: GetStageResponse)

-- | The response's http status code.
getStageResponse_httpStatus :: Lens.Lens' GetStageResponse Prelude.Int
getStageResponse_httpStatus = Lens.lens (\GetStageResponse' {httpStatus} -> httpStatus) (\s@GetStageResponse' {} a -> s {httpStatus = a} :: GetStageResponse)

instance Prelude.NFData GetStageResponse where
  rnf GetStageResponse' {..} =
    Prelude.rnf stage
      `Prelude.seq` Prelude.rnf httpStatus
