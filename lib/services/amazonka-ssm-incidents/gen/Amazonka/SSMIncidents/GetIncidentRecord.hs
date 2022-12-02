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
-- Module      : Amazonka.SSMIncidents.GetIncidentRecord
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details for the specified incident record.
module Amazonka.SSMIncidents.GetIncidentRecord
  ( -- * Creating a Request
    GetIncidentRecord (..),
    newGetIncidentRecord,

    -- * Request Lenses
    getIncidentRecord_arn,

    -- * Destructuring the Response
    GetIncidentRecordResponse (..),
    newGetIncidentRecordResponse,

    -- * Response Lenses
    getIncidentRecordResponse_httpStatus,
    getIncidentRecordResponse_incidentRecord,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SSMIncidents.Types

-- | /See:/ 'newGetIncidentRecord' smart constructor.
data GetIncidentRecord = GetIncidentRecord'
  { -- | The Amazon Resource Name (ARN) of the incident record.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIncidentRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'getIncidentRecord_arn' - The Amazon Resource Name (ARN) of the incident record.
newGetIncidentRecord ::
  -- | 'arn'
  Prelude.Text ->
  GetIncidentRecord
newGetIncidentRecord pArn_ =
  GetIncidentRecord' {arn = pArn_}

-- | The Amazon Resource Name (ARN) of the incident record.
getIncidentRecord_arn :: Lens.Lens' GetIncidentRecord Prelude.Text
getIncidentRecord_arn = Lens.lens (\GetIncidentRecord' {arn} -> arn) (\s@GetIncidentRecord' {} a -> s {arn = a} :: GetIncidentRecord)

instance Core.AWSRequest GetIncidentRecord where
  type
    AWSResponse GetIncidentRecord =
      GetIncidentRecordResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetIncidentRecordResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "incidentRecord")
      )

instance Prelude.Hashable GetIncidentRecord where
  hashWithSalt _salt GetIncidentRecord' {..} =
    _salt `Prelude.hashWithSalt` arn

instance Prelude.NFData GetIncidentRecord where
  rnf GetIncidentRecord' {..} = Prelude.rnf arn

instance Data.ToHeaders GetIncidentRecord where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetIncidentRecord where
  toPath = Prelude.const "/getIncidentRecord"

instance Data.ToQuery GetIncidentRecord where
  toQuery GetIncidentRecord' {..} =
    Prelude.mconcat ["arn" Data.=: arn]

-- | /See:/ 'newGetIncidentRecordResponse' smart constructor.
data GetIncidentRecordResponse = GetIncidentRecordResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Details the structure of the incident record.
    incidentRecord :: IncidentRecord
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetIncidentRecordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getIncidentRecordResponse_httpStatus' - The response's http status code.
--
-- 'incidentRecord', 'getIncidentRecordResponse_incidentRecord' - Details the structure of the incident record.
newGetIncidentRecordResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'incidentRecord'
  IncidentRecord ->
  GetIncidentRecordResponse
newGetIncidentRecordResponse
  pHttpStatus_
  pIncidentRecord_ =
    GetIncidentRecordResponse'
      { httpStatus =
          pHttpStatus_,
        incidentRecord = pIncidentRecord_
      }

-- | The response's http status code.
getIncidentRecordResponse_httpStatus :: Lens.Lens' GetIncidentRecordResponse Prelude.Int
getIncidentRecordResponse_httpStatus = Lens.lens (\GetIncidentRecordResponse' {httpStatus} -> httpStatus) (\s@GetIncidentRecordResponse' {} a -> s {httpStatus = a} :: GetIncidentRecordResponse)

-- | Details the structure of the incident record.
getIncidentRecordResponse_incidentRecord :: Lens.Lens' GetIncidentRecordResponse IncidentRecord
getIncidentRecordResponse_incidentRecord = Lens.lens (\GetIncidentRecordResponse' {incidentRecord} -> incidentRecord) (\s@GetIncidentRecordResponse' {} a -> s {incidentRecord = a} :: GetIncidentRecordResponse)

instance Prelude.NFData GetIncidentRecordResponse where
  rnf GetIncidentRecordResponse' {..} =
    Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf incidentRecord
