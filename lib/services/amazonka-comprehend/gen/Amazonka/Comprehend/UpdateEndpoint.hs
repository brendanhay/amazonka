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
-- Module      : Amazonka.Comprehend.UpdateEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates information about the specified endpoint. For information about
-- endpoints, see
-- <https://docs.aws.amazon.com/comprehend/latest/dg/manage-endpoints.html Managing endpoints>.
module Amazonka.Comprehend.UpdateEndpoint
  ( -- * Creating a Request
    UpdateEndpoint (..),
    newUpdateEndpoint,

    -- * Request Lenses
    updateEndpoint_desiredDataAccessRoleArn,
    updateEndpoint_desiredInferenceUnits,
    updateEndpoint_desiredModelArn,
    updateEndpoint_endpointArn,

    -- * Destructuring the Response
    UpdateEndpointResponse (..),
    newUpdateEndpointResponse,

    -- * Response Lenses
    updateEndpointResponse_httpStatus,
  )
where

import Amazonka.Comprehend.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateEndpoint' smart constructor.
data UpdateEndpoint = UpdateEndpoint'
  { -- | Data access role ARN to use in case the new model is encrypted with a
    -- customer CMK.
    desiredDataAccessRoleArn :: Prelude.Maybe Prelude.Text,
    -- | The desired number of inference units to be used by the model using this
    -- endpoint. Each inference unit represents of a throughput of 100
    -- characters per second.
    desiredInferenceUnits :: Prelude.Maybe Prelude.Natural,
    -- | The ARN of the new model to use when updating an existing endpoint.
    desiredModelArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Number (ARN) of the endpoint being updated.
    endpointArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'desiredDataAccessRoleArn', 'updateEndpoint_desiredDataAccessRoleArn' - Data access role ARN to use in case the new model is encrypted with a
-- customer CMK.
--
-- 'desiredInferenceUnits', 'updateEndpoint_desiredInferenceUnits' - The desired number of inference units to be used by the model using this
-- endpoint. Each inference unit represents of a throughput of 100
-- characters per second.
--
-- 'desiredModelArn', 'updateEndpoint_desiredModelArn' - The ARN of the new model to use when updating an existing endpoint.
--
-- 'endpointArn', 'updateEndpoint_endpointArn' - The Amazon Resource Number (ARN) of the endpoint being updated.
newUpdateEndpoint ::
  -- | 'endpointArn'
  Prelude.Text ->
  UpdateEndpoint
newUpdateEndpoint pEndpointArn_ =
  UpdateEndpoint'
    { desiredDataAccessRoleArn =
        Prelude.Nothing,
      desiredInferenceUnits = Prelude.Nothing,
      desiredModelArn = Prelude.Nothing,
      endpointArn = pEndpointArn_
    }

-- | Data access role ARN to use in case the new model is encrypted with a
-- customer CMK.
updateEndpoint_desiredDataAccessRoleArn :: Lens.Lens' UpdateEndpoint (Prelude.Maybe Prelude.Text)
updateEndpoint_desiredDataAccessRoleArn = Lens.lens (\UpdateEndpoint' {desiredDataAccessRoleArn} -> desiredDataAccessRoleArn) (\s@UpdateEndpoint' {} a -> s {desiredDataAccessRoleArn = a} :: UpdateEndpoint)

-- | The desired number of inference units to be used by the model using this
-- endpoint. Each inference unit represents of a throughput of 100
-- characters per second.
updateEndpoint_desiredInferenceUnits :: Lens.Lens' UpdateEndpoint (Prelude.Maybe Prelude.Natural)
updateEndpoint_desiredInferenceUnits = Lens.lens (\UpdateEndpoint' {desiredInferenceUnits} -> desiredInferenceUnits) (\s@UpdateEndpoint' {} a -> s {desiredInferenceUnits = a} :: UpdateEndpoint)

-- | The ARN of the new model to use when updating an existing endpoint.
updateEndpoint_desiredModelArn :: Lens.Lens' UpdateEndpoint (Prelude.Maybe Prelude.Text)
updateEndpoint_desiredModelArn = Lens.lens (\UpdateEndpoint' {desiredModelArn} -> desiredModelArn) (\s@UpdateEndpoint' {} a -> s {desiredModelArn = a} :: UpdateEndpoint)

-- | The Amazon Resource Number (ARN) of the endpoint being updated.
updateEndpoint_endpointArn :: Lens.Lens' UpdateEndpoint Prelude.Text
updateEndpoint_endpointArn = Lens.lens (\UpdateEndpoint' {endpointArn} -> endpointArn) (\s@UpdateEndpoint' {} a -> s {endpointArn = a} :: UpdateEndpoint)

instance Core.AWSRequest UpdateEndpoint where
  type
    AWSResponse UpdateEndpoint =
      UpdateEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          UpdateEndpointResponse'
            Prelude.<$> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateEndpoint where
  hashWithSalt _salt UpdateEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` desiredDataAccessRoleArn
      `Prelude.hashWithSalt` desiredInferenceUnits
      `Prelude.hashWithSalt` desiredModelArn
      `Prelude.hashWithSalt` endpointArn

instance Prelude.NFData UpdateEndpoint where
  rnf UpdateEndpoint' {..} =
    Prelude.rnf desiredDataAccessRoleArn `Prelude.seq`
      Prelude.rnf desiredInferenceUnits `Prelude.seq`
        Prelude.rnf desiredModelArn `Prelude.seq`
          Prelude.rnf endpointArn

instance Data.ToHeaders UpdateEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Comprehend_20171127.UpdateEndpoint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateEndpoint where
  toJSON UpdateEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DesiredDataAccessRoleArn" Data..=)
              Prelude.<$> desiredDataAccessRoleArn,
            ("DesiredInferenceUnits" Data..=)
              Prelude.<$> desiredInferenceUnits,
            ("DesiredModelArn" Data..=)
              Prelude.<$> desiredModelArn,
            Prelude.Just ("EndpointArn" Data..= endpointArn)
          ]
      )

instance Data.ToPath UpdateEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateEndpointResponse' smart constructor.
data UpdateEndpointResponse = UpdateEndpointResponse'
  { -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'httpStatus', 'updateEndpointResponse_httpStatus' - The response's http status code.
newUpdateEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateEndpointResponse
newUpdateEndpointResponse pHttpStatus_ =
  UpdateEndpointResponse' {httpStatus = pHttpStatus_}

-- | The response's http status code.
updateEndpointResponse_httpStatus :: Lens.Lens' UpdateEndpointResponse Prelude.Int
updateEndpointResponse_httpStatus = Lens.lens (\UpdateEndpointResponse' {httpStatus} -> httpStatus) (\s@UpdateEndpointResponse' {} a -> s {httpStatus = a} :: UpdateEndpointResponse)

instance Prelude.NFData UpdateEndpointResponse where
  rnf UpdateEndpointResponse' {..} =
    Prelude.rnf httpStatus
