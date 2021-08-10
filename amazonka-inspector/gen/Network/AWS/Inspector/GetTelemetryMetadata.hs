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
-- Module      : Network.AWS.Inspector.GetTelemetryMetadata
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about the data that is collected for the specified
-- assessment run.
module Network.AWS.Inspector.GetTelemetryMetadata
  ( -- * Creating a Request
    GetTelemetryMetadata (..),
    newGetTelemetryMetadata,

    -- * Request Lenses
    getTelemetryMetadata_assessmentRunArn,

    -- * Destructuring the Response
    GetTelemetryMetadataResponse (..),
    newGetTelemetryMetadataResponse,

    -- * Response Lenses
    getTelemetryMetadataResponse_httpStatus,
    getTelemetryMetadataResponse_telemetryMetadata,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Inspector.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetTelemetryMetadata' smart constructor.
data GetTelemetryMetadata = GetTelemetryMetadata'
  { -- | The ARN that specifies the assessment run that has the telemetry data
    -- that you want to obtain.
    assessmentRunArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTelemetryMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assessmentRunArn', 'getTelemetryMetadata_assessmentRunArn' - The ARN that specifies the assessment run that has the telemetry data
-- that you want to obtain.
newGetTelemetryMetadata ::
  -- | 'assessmentRunArn'
  Prelude.Text ->
  GetTelemetryMetadata
newGetTelemetryMetadata pAssessmentRunArn_ =
  GetTelemetryMetadata'
    { assessmentRunArn =
        pAssessmentRunArn_
    }

-- | The ARN that specifies the assessment run that has the telemetry data
-- that you want to obtain.
getTelemetryMetadata_assessmentRunArn :: Lens.Lens' GetTelemetryMetadata Prelude.Text
getTelemetryMetadata_assessmentRunArn = Lens.lens (\GetTelemetryMetadata' {assessmentRunArn} -> assessmentRunArn) (\s@GetTelemetryMetadata' {} a -> s {assessmentRunArn = a} :: GetTelemetryMetadata)

instance Core.AWSRequest GetTelemetryMetadata where
  type
    AWSResponse GetTelemetryMetadata =
      GetTelemetryMetadataResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTelemetryMetadataResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "telemetryMetadata"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable GetTelemetryMetadata

instance Prelude.NFData GetTelemetryMetadata

instance Core.ToHeaders GetTelemetryMetadata where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "InspectorService.GetTelemetryMetadata" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetTelemetryMetadata where
  toJSON GetTelemetryMetadata' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("assessmentRunArn" Core..= assessmentRunArn)
          ]
      )

instance Core.ToPath GetTelemetryMetadata where
  toPath = Prelude.const "/"

instance Core.ToQuery GetTelemetryMetadata where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTelemetryMetadataResponse' smart constructor.
data GetTelemetryMetadataResponse = GetTelemetryMetadataResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Telemetry details.
    telemetryMetadata :: [TelemetryMetadata]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTelemetryMetadataResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'getTelemetryMetadataResponse_httpStatus' - The response's http status code.
--
-- 'telemetryMetadata', 'getTelemetryMetadataResponse_telemetryMetadata' - Telemetry details.
newGetTelemetryMetadataResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTelemetryMetadataResponse
newGetTelemetryMetadataResponse pHttpStatus_ =
  GetTelemetryMetadataResponse'
    { httpStatus =
        pHttpStatus_,
      telemetryMetadata = Prelude.mempty
    }

-- | The response's http status code.
getTelemetryMetadataResponse_httpStatus :: Lens.Lens' GetTelemetryMetadataResponse Prelude.Int
getTelemetryMetadataResponse_httpStatus = Lens.lens (\GetTelemetryMetadataResponse' {httpStatus} -> httpStatus) (\s@GetTelemetryMetadataResponse' {} a -> s {httpStatus = a} :: GetTelemetryMetadataResponse)

-- | Telemetry details.
getTelemetryMetadataResponse_telemetryMetadata :: Lens.Lens' GetTelemetryMetadataResponse [TelemetryMetadata]
getTelemetryMetadataResponse_telemetryMetadata = Lens.lens (\GetTelemetryMetadataResponse' {telemetryMetadata} -> telemetryMetadata) (\s@GetTelemetryMetadataResponse' {} a -> s {telemetryMetadata = a} :: GetTelemetryMetadataResponse) Prelude.. Lens._Coerce

instance Prelude.NFData GetTelemetryMetadataResponse
