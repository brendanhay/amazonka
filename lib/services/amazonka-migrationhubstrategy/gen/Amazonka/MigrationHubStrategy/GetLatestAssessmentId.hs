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
-- Module      : Amazonka.MigrationHubStrategy.GetLatestAssessmentId
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieve the latest ID of a specific assessment task.
module Amazonka.MigrationHubStrategy.GetLatestAssessmentId
  ( -- * Creating a Request
    GetLatestAssessmentId (..),
    newGetLatestAssessmentId,

    -- * Destructuring the Response
    GetLatestAssessmentIdResponse (..),
    newGetLatestAssessmentIdResponse,

    -- * Response Lenses
    getLatestAssessmentIdResponse_id,
    getLatestAssessmentIdResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetLatestAssessmentId' smart constructor.
data GetLatestAssessmentId = GetLatestAssessmentId'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLatestAssessmentId' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newGetLatestAssessmentId ::
  GetLatestAssessmentId
newGetLatestAssessmentId = GetLatestAssessmentId'

instance Core.AWSRequest GetLatestAssessmentId where
  type
    AWSResponse GetLatestAssessmentId =
      GetLatestAssessmentIdResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetLatestAssessmentIdResponse'
            Prelude.<$> (x Data..?> "id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetLatestAssessmentId where
  hashWithSalt _salt _ =
    _salt `Prelude.hashWithSalt` ()

instance Prelude.NFData GetLatestAssessmentId where
  rnf _ = ()

instance Data.ToHeaders GetLatestAssessmentId where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetLatestAssessmentId where
  toPath = Prelude.const "/get-latest-assessment-id"

instance Data.ToQuery GetLatestAssessmentId where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetLatestAssessmentIdResponse' smart constructor.
data GetLatestAssessmentIdResponse = GetLatestAssessmentIdResponse'
  { -- | The latest ID for the specific assessment task.
    id :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetLatestAssessmentIdResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getLatestAssessmentIdResponse_id' - The latest ID for the specific assessment task.
--
-- 'httpStatus', 'getLatestAssessmentIdResponse_httpStatus' - The response's http status code.
newGetLatestAssessmentIdResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetLatestAssessmentIdResponse
newGetLatestAssessmentIdResponse pHttpStatus_ =
  GetLatestAssessmentIdResponse'
    { id =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The latest ID for the specific assessment task.
getLatestAssessmentIdResponse_id :: Lens.Lens' GetLatestAssessmentIdResponse (Prelude.Maybe Prelude.Text)
getLatestAssessmentIdResponse_id = Lens.lens (\GetLatestAssessmentIdResponse' {id} -> id) (\s@GetLatestAssessmentIdResponse' {} a -> s {id = a} :: GetLatestAssessmentIdResponse)

-- | The response's http status code.
getLatestAssessmentIdResponse_httpStatus :: Lens.Lens' GetLatestAssessmentIdResponse Prelude.Int
getLatestAssessmentIdResponse_httpStatus = Lens.lens (\GetLatestAssessmentIdResponse' {httpStatus} -> httpStatus) (\s@GetLatestAssessmentIdResponse' {} a -> s {httpStatus = a} :: GetLatestAssessmentIdResponse)

instance Prelude.NFData GetLatestAssessmentIdResponse where
  rnf GetLatestAssessmentIdResponse' {..} =
    Prelude.rnf id `Prelude.seq` Prelude.rnf httpStatus
