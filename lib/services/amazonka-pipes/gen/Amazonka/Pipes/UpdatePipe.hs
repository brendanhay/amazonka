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
-- Module      : Amazonka.Pipes.UpdatePipe
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update an existing pipe. When you call @UpdatePipe@, only the fields
-- that are included in the request are changed, the rest are unchanged.
-- The exception to this is if you modify any Amazon Web Services-service
-- specific fields in the @SourceParameters@, @EnrichmentParameters@, or
-- @TargetParameters@ objects. The fields in these objects are updated
-- atomically as one and override existing values. This is by design and
-- means that if you don\'t specify an optional field in one of these
-- Parameters objects, that field will be set to its system-default value
-- after the update.
--
-- >  <p>For more information about pipes, see <a href="https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-pipes.html"> Amazon EventBridge Pipes</a> in the Amazon EventBridge User Guide.</p>
module Amazonka.Pipes.UpdatePipe
  ( -- * Creating a Request
    UpdatePipe (..),
    newUpdatePipe,

    -- * Request Lenses
    updatePipe_description,
    updatePipe_desiredState,
    updatePipe_enrichment,
    updatePipe_enrichmentParameters,
    updatePipe_sourceParameters,
    updatePipe_target,
    updatePipe_targetParameters,
    updatePipe_name,
    updatePipe_roleArn,

    -- * Destructuring the Response
    UpdatePipeResponse (..),
    newUpdatePipeResponse,

    -- * Response Lenses
    updatePipeResponse_arn,
    updatePipeResponse_creationTime,
    updatePipeResponse_currentState,
    updatePipeResponse_desiredState,
    updatePipeResponse_lastModifiedTime,
    updatePipeResponse_name,
    updatePipeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdatePipe' smart constructor.
data UpdatePipe = UpdatePipe'
  { -- | A description of the pipe.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The state the pipe should be in.
    desiredState :: Prelude.Maybe RequestedPipeState,
    -- | The ARN of the enrichment resource.
    enrichment :: Prelude.Maybe Prelude.Text,
    -- | The parameters required to set up enrichment on your pipe.
    enrichmentParameters :: Prelude.Maybe PipeEnrichmentParameters,
    -- | The parameters required to set up a source for your pipe.
    sourceParameters :: Prelude.Maybe UpdatePipeSourceParameters,
    -- | The ARN of the target resource.
    target :: Prelude.Maybe Prelude.Text,
    -- | The parameters required to set up a target for your pipe.
    targetParameters :: Prelude.Maybe PipeTargetParameters,
    -- | The name of the pipe.
    name :: Prelude.Text,
    -- | The ARN of the role that allows the pipe to send data to the target.
    roleArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'updatePipe_description' - A description of the pipe.
--
-- 'desiredState', 'updatePipe_desiredState' - The state the pipe should be in.
--
-- 'enrichment', 'updatePipe_enrichment' - The ARN of the enrichment resource.
--
-- 'enrichmentParameters', 'updatePipe_enrichmentParameters' - The parameters required to set up enrichment on your pipe.
--
-- 'sourceParameters', 'updatePipe_sourceParameters' - The parameters required to set up a source for your pipe.
--
-- 'target', 'updatePipe_target' - The ARN of the target resource.
--
-- 'targetParameters', 'updatePipe_targetParameters' - The parameters required to set up a target for your pipe.
--
-- 'name', 'updatePipe_name' - The name of the pipe.
--
-- 'roleArn', 'updatePipe_roleArn' - The ARN of the role that allows the pipe to send data to the target.
newUpdatePipe ::
  -- | 'name'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  UpdatePipe
newUpdatePipe pName_ pRoleArn_ =
  UpdatePipe'
    { description = Prelude.Nothing,
      desiredState = Prelude.Nothing,
      enrichment = Prelude.Nothing,
      enrichmentParameters = Prelude.Nothing,
      sourceParameters = Prelude.Nothing,
      target = Prelude.Nothing,
      targetParameters = Prelude.Nothing,
      name = pName_,
      roleArn = pRoleArn_
    }

-- | A description of the pipe.
updatePipe_description :: Lens.Lens' UpdatePipe (Prelude.Maybe Prelude.Text)
updatePipe_description = Lens.lens (\UpdatePipe' {description} -> description) (\s@UpdatePipe' {} a -> s {description = a} :: UpdatePipe) Prelude.. Lens.mapping Data._Sensitive

-- | The state the pipe should be in.
updatePipe_desiredState :: Lens.Lens' UpdatePipe (Prelude.Maybe RequestedPipeState)
updatePipe_desiredState = Lens.lens (\UpdatePipe' {desiredState} -> desiredState) (\s@UpdatePipe' {} a -> s {desiredState = a} :: UpdatePipe)

-- | The ARN of the enrichment resource.
updatePipe_enrichment :: Lens.Lens' UpdatePipe (Prelude.Maybe Prelude.Text)
updatePipe_enrichment = Lens.lens (\UpdatePipe' {enrichment} -> enrichment) (\s@UpdatePipe' {} a -> s {enrichment = a} :: UpdatePipe)

-- | The parameters required to set up enrichment on your pipe.
updatePipe_enrichmentParameters :: Lens.Lens' UpdatePipe (Prelude.Maybe PipeEnrichmentParameters)
updatePipe_enrichmentParameters = Lens.lens (\UpdatePipe' {enrichmentParameters} -> enrichmentParameters) (\s@UpdatePipe' {} a -> s {enrichmentParameters = a} :: UpdatePipe)

-- | The parameters required to set up a source for your pipe.
updatePipe_sourceParameters :: Lens.Lens' UpdatePipe (Prelude.Maybe UpdatePipeSourceParameters)
updatePipe_sourceParameters = Lens.lens (\UpdatePipe' {sourceParameters} -> sourceParameters) (\s@UpdatePipe' {} a -> s {sourceParameters = a} :: UpdatePipe)

-- | The ARN of the target resource.
updatePipe_target :: Lens.Lens' UpdatePipe (Prelude.Maybe Prelude.Text)
updatePipe_target = Lens.lens (\UpdatePipe' {target} -> target) (\s@UpdatePipe' {} a -> s {target = a} :: UpdatePipe)

-- | The parameters required to set up a target for your pipe.
updatePipe_targetParameters :: Lens.Lens' UpdatePipe (Prelude.Maybe PipeTargetParameters)
updatePipe_targetParameters = Lens.lens (\UpdatePipe' {targetParameters} -> targetParameters) (\s@UpdatePipe' {} a -> s {targetParameters = a} :: UpdatePipe)

-- | The name of the pipe.
updatePipe_name :: Lens.Lens' UpdatePipe Prelude.Text
updatePipe_name = Lens.lens (\UpdatePipe' {name} -> name) (\s@UpdatePipe' {} a -> s {name = a} :: UpdatePipe)

-- | The ARN of the role that allows the pipe to send data to the target.
updatePipe_roleArn :: Lens.Lens' UpdatePipe Prelude.Text
updatePipe_roleArn = Lens.lens (\UpdatePipe' {roleArn} -> roleArn) (\s@UpdatePipe' {} a -> s {roleArn = a} :: UpdatePipe)

instance Core.AWSRequest UpdatePipe where
  type AWSResponse UpdatePipe = UpdatePipeResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdatePipeResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "CurrentState")
            Prelude.<*> (x Data..?> "DesiredState")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdatePipe where
  hashWithSalt _salt UpdatePipe' {..} =
    _salt `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` desiredState
      `Prelude.hashWithSalt` enrichment
      `Prelude.hashWithSalt` enrichmentParameters
      `Prelude.hashWithSalt` sourceParameters
      `Prelude.hashWithSalt` target
      `Prelude.hashWithSalt` targetParameters
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` roleArn

instance Prelude.NFData UpdatePipe where
  rnf UpdatePipe' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf desiredState
      `Prelude.seq` Prelude.rnf enrichment
      `Prelude.seq` Prelude.rnf enrichmentParameters
      `Prelude.seq` Prelude.rnf sourceParameters
      `Prelude.seq` Prelude.rnf target
      `Prelude.seq` Prelude.rnf targetParameters
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn

instance Data.ToHeaders UpdatePipe where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdatePipe where
  toJSON UpdatePipe' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("DesiredState" Data..=) Prelude.<$> desiredState,
            ("Enrichment" Data..=) Prelude.<$> enrichment,
            ("EnrichmentParameters" Data..=)
              Prelude.<$> enrichmentParameters,
            ("SourceParameters" Data..=)
              Prelude.<$> sourceParameters,
            ("Target" Data..=) Prelude.<$> target,
            ("TargetParameters" Data..=)
              Prelude.<$> targetParameters,
            Prelude.Just ("RoleArn" Data..= roleArn)
          ]
      )

instance Data.ToPath UpdatePipe where
  toPath UpdatePipe' {..} =
    Prelude.mconcat ["/v1/pipes/", Data.toBS name]

instance Data.ToQuery UpdatePipe where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdatePipeResponse' smart constructor.
data UpdatePipeResponse = UpdatePipeResponse'
  { -- | The ARN of the pipe.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time the pipe was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The state the pipe is in.
    currentState :: Prelude.Maybe PipeState,
    -- | The state the pipe should be in.
    desiredState :: Prelude.Maybe RequestedPipeState,
    -- | When the pipe was last updated, in
    -- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
    -- (YYYY-MM-DDThh:mm:ss.sTZD).
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the pipe.
    name :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePipeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'updatePipeResponse_arn' - The ARN of the pipe.
--
-- 'creationTime', 'updatePipeResponse_creationTime' - The time the pipe was created.
--
-- 'currentState', 'updatePipeResponse_currentState' - The state the pipe is in.
--
-- 'desiredState', 'updatePipeResponse_desiredState' - The state the pipe should be in.
--
-- 'lastModifiedTime', 'updatePipeResponse_lastModifiedTime' - When the pipe was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- 'name', 'updatePipeResponse_name' - The name of the pipe.
--
-- 'httpStatus', 'updatePipeResponse_httpStatus' - The response's http status code.
newUpdatePipeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdatePipeResponse
newUpdatePipeResponse pHttpStatus_ =
  UpdatePipeResponse'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      currentState = Prelude.Nothing,
      desiredState = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the pipe.
updatePipeResponse_arn :: Lens.Lens' UpdatePipeResponse (Prelude.Maybe Prelude.Text)
updatePipeResponse_arn = Lens.lens (\UpdatePipeResponse' {arn} -> arn) (\s@UpdatePipeResponse' {} a -> s {arn = a} :: UpdatePipeResponse)

-- | The time the pipe was created.
updatePipeResponse_creationTime :: Lens.Lens' UpdatePipeResponse (Prelude.Maybe Prelude.UTCTime)
updatePipeResponse_creationTime = Lens.lens (\UpdatePipeResponse' {creationTime} -> creationTime) (\s@UpdatePipeResponse' {} a -> s {creationTime = a} :: UpdatePipeResponse) Prelude.. Lens.mapping Data._Time

-- | The state the pipe is in.
updatePipeResponse_currentState :: Lens.Lens' UpdatePipeResponse (Prelude.Maybe PipeState)
updatePipeResponse_currentState = Lens.lens (\UpdatePipeResponse' {currentState} -> currentState) (\s@UpdatePipeResponse' {} a -> s {currentState = a} :: UpdatePipeResponse)

-- | The state the pipe should be in.
updatePipeResponse_desiredState :: Lens.Lens' UpdatePipeResponse (Prelude.Maybe RequestedPipeState)
updatePipeResponse_desiredState = Lens.lens (\UpdatePipeResponse' {desiredState} -> desiredState) (\s@UpdatePipeResponse' {} a -> s {desiredState = a} :: UpdatePipeResponse)

-- | When the pipe was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
updatePipeResponse_lastModifiedTime :: Lens.Lens' UpdatePipeResponse (Prelude.Maybe Prelude.UTCTime)
updatePipeResponse_lastModifiedTime = Lens.lens (\UpdatePipeResponse' {lastModifiedTime} -> lastModifiedTime) (\s@UpdatePipeResponse' {} a -> s {lastModifiedTime = a} :: UpdatePipeResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the pipe.
updatePipeResponse_name :: Lens.Lens' UpdatePipeResponse (Prelude.Maybe Prelude.Text)
updatePipeResponse_name = Lens.lens (\UpdatePipeResponse' {name} -> name) (\s@UpdatePipeResponse' {} a -> s {name = a} :: UpdatePipeResponse)

-- | The response's http status code.
updatePipeResponse_httpStatus :: Lens.Lens' UpdatePipeResponse Prelude.Int
updatePipeResponse_httpStatus = Lens.lens (\UpdatePipeResponse' {httpStatus} -> httpStatus) (\s@UpdatePipeResponse' {} a -> s {httpStatus = a} :: UpdatePipeResponse)

instance Prelude.NFData UpdatePipeResponse where
  rnf UpdatePipeResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf currentState
      `Prelude.seq` Prelude.rnf desiredState
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
