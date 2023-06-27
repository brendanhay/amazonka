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
-- Module      : Amazonka.Pipes.CreatePipe
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Create a pipe. Amazon EventBridge Pipes connect event sources to targets
-- and reduces the need for specialized knowledge and integration code.
module Amazonka.Pipes.CreatePipe
  ( -- * Creating a Request
    CreatePipe (..),
    newCreatePipe,

    -- * Request Lenses
    createPipe_description,
    createPipe_desiredState,
    createPipe_enrichment,
    createPipe_enrichmentParameters,
    createPipe_sourceParameters,
    createPipe_tags,
    createPipe_targetParameters,
    createPipe_name,
    createPipe_roleArn,
    createPipe_source,
    createPipe_target,

    -- * Destructuring the Response
    CreatePipeResponse (..),
    newCreatePipeResponse,

    -- * Response Lenses
    createPipeResponse_arn,
    createPipeResponse_creationTime,
    createPipeResponse_currentState,
    createPipeResponse_desiredState,
    createPipeResponse_lastModifiedTime,
    createPipeResponse_name,
    createPipeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreatePipe' smart constructor.
data CreatePipe = CreatePipe'
  { -- | A description of the pipe.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The state the pipe should be in.
    desiredState :: Prelude.Maybe RequestedPipeState,
    -- | The ARN of the enrichment resource.
    enrichment :: Prelude.Maybe Prelude.Text,
    -- | The parameters required to set up enrichment on your pipe.
    enrichmentParameters :: Prelude.Maybe PipeEnrichmentParameters,
    -- | The parameters required to set up a source for your pipe.
    sourceParameters :: Prelude.Maybe PipeSourceParameters,
    -- | The list of key-value pairs to associate with the pipe.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text)),
    -- | The parameters required to set up a target for your pipe.
    targetParameters :: Prelude.Maybe PipeTargetParameters,
    -- | The name of the pipe.
    name :: Prelude.Text,
    -- | The ARN of the role that allows the pipe to send data to the target.
    roleArn :: Prelude.Text,
    -- | The ARN of the source resource.
    source :: Prelude.Text,
    -- | The ARN of the target resource.
    target :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreatePipe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'description', 'createPipe_description' - A description of the pipe.
--
-- 'desiredState', 'createPipe_desiredState' - The state the pipe should be in.
--
-- 'enrichment', 'createPipe_enrichment' - The ARN of the enrichment resource.
--
-- 'enrichmentParameters', 'createPipe_enrichmentParameters' - The parameters required to set up enrichment on your pipe.
--
-- 'sourceParameters', 'createPipe_sourceParameters' - The parameters required to set up a source for your pipe.
--
-- 'tags', 'createPipe_tags' - The list of key-value pairs to associate with the pipe.
--
-- 'targetParameters', 'createPipe_targetParameters' - The parameters required to set up a target for your pipe.
--
-- 'name', 'createPipe_name' - The name of the pipe.
--
-- 'roleArn', 'createPipe_roleArn' - The ARN of the role that allows the pipe to send data to the target.
--
-- 'source', 'createPipe_source' - The ARN of the source resource.
--
-- 'target', 'createPipe_target' - The ARN of the target resource.
newCreatePipe ::
  -- | 'name'
  Prelude.Text ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'source'
  Prelude.Text ->
  -- | 'target'
  Prelude.Text ->
  CreatePipe
newCreatePipe pName_ pRoleArn_ pSource_ pTarget_ =
  CreatePipe'
    { description = Prelude.Nothing,
      desiredState = Prelude.Nothing,
      enrichment = Prelude.Nothing,
      enrichmentParameters = Prelude.Nothing,
      sourceParameters = Prelude.Nothing,
      tags = Prelude.Nothing,
      targetParameters = Prelude.Nothing,
      name = pName_,
      roleArn = pRoleArn_,
      source = pSource_,
      target = pTarget_
    }

-- | A description of the pipe.
createPipe_description :: Lens.Lens' CreatePipe (Prelude.Maybe Prelude.Text)
createPipe_description = Lens.lens (\CreatePipe' {description} -> description) (\s@CreatePipe' {} a -> s {description = a} :: CreatePipe) Prelude.. Lens.mapping Data._Sensitive

-- | The state the pipe should be in.
createPipe_desiredState :: Lens.Lens' CreatePipe (Prelude.Maybe RequestedPipeState)
createPipe_desiredState = Lens.lens (\CreatePipe' {desiredState} -> desiredState) (\s@CreatePipe' {} a -> s {desiredState = a} :: CreatePipe)

-- | The ARN of the enrichment resource.
createPipe_enrichment :: Lens.Lens' CreatePipe (Prelude.Maybe Prelude.Text)
createPipe_enrichment = Lens.lens (\CreatePipe' {enrichment} -> enrichment) (\s@CreatePipe' {} a -> s {enrichment = a} :: CreatePipe)

-- | The parameters required to set up enrichment on your pipe.
createPipe_enrichmentParameters :: Lens.Lens' CreatePipe (Prelude.Maybe PipeEnrichmentParameters)
createPipe_enrichmentParameters = Lens.lens (\CreatePipe' {enrichmentParameters} -> enrichmentParameters) (\s@CreatePipe' {} a -> s {enrichmentParameters = a} :: CreatePipe)

-- | The parameters required to set up a source for your pipe.
createPipe_sourceParameters :: Lens.Lens' CreatePipe (Prelude.Maybe PipeSourceParameters)
createPipe_sourceParameters = Lens.lens (\CreatePipe' {sourceParameters} -> sourceParameters) (\s@CreatePipe' {} a -> s {sourceParameters = a} :: CreatePipe)

-- | The list of key-value pairs to associate with the pipe.
createPipe_tags :: Lens.Lens' CreatePipe (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
createPipe_tags = Lens.lens (\CreatePipe' {tags} -> tags) (\s@CreatePipe' {} a -> s {tags = a} :: CreatePipe) Prelude.. Lens.mapping Lens.coerced

-- | The parameters required to set up a target for your pipe.
createPipe_targetParameters :: Lens.Lens' CreatePipe (Prelude.Maybe PipeTargetParameters)
createPipe_targetParameters = Lens.lens (\CreatePipe' {targetParameters} -> targetParameters) (\s@CreatePipe' {} a -> s {targetParameters = a} :: CreatePipe)

-- | The name of the pipe.
createPipe_name :: Lens.Lens' CreatePipe Prelude.Text
createPipe_name = Lens.lens (\CreatePipe' {name} -> name) (\s@CreatePipe' {} a -> s {name = a} :: CreatePipe)

-- | The ARN of the role that allows the pipe to send data to the target.
createPipe_roleArn :: Lens.Lens' CreatePipe Prelude.Text
createPipe_roleArn = Lens.lens (\CreatePipe' {roleArn} -> roleArn) (\s@CreatePipe' {} a -> s {roleArn = a} :: CreatePipe)

-- | The ARN of the source resource.
createPipe_source :: Lens.Lens' CreatePipe Prelude.Text
createPipe_source = Lens.lens (\CreatePipe' {source} -> source) (\s@CreatePipe' {} a -> s {source = a} :: CreatePipe)

-- | The ARN of the target resource.
createPipe_target :: Lens.Lens' CreatePipe Prelude.Text
createPipe_target = Lens.lens (\CreatePipe' {target} -> target) (\s@CreatePipe' {} a -> s {target = a} :: CreatePipe)

instance Core.AWSRequest CreatePipe where
  type AWSResponse CreatePipe = CreatePipeResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreatePipeResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "CurrentState")
            Prelude.<*> (x Data..?> "DesiredState")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreatePipe where
  hashWithSalt _salt CreatePipe' {..} =
    _salt
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` desiredState
      `Prelude.hashWithSalt` enrichment
      `Prelude.hashWithSalt` enrichmentParameters
      `Prelude.hashWithSalt` sourceParameters
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` targetParameters
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` target

instance Prelude.NFData CreatePipe where
  rnf CreatePipe' {..} =
    Prelude.rnf description
      `Prelude.seq` Prelude.rnf desiredState
      `Prelude.seq` Prelude.rnf enrichment
      `Prelude.seq` Prelude.rnf enrichmentParameters
      `Prelude.seq` Prelude.rnf sourceParameters
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf targetParameters
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf target

instance Data.ToHeaders CreatePipe where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreatePipe where
  toJSON CreatePipe' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Description" Data..=) Prelude.<$> description,
            ("DesiredState" Data..=) Prelude.<$> desiredState,
            ("Enrichment" Data..=) Prelude.<$> enrichment,
            ("EnrichmentParameters" Data..=)
              Prelude.<$> enrichmentParameters,
            ("SourceParameters" Data..=)
              Prelude.<$> sourceParameters,
            ("Tags" Data..=) Prelude.<$> tags,
            ("TargetParameters" Data..=)
              Prelude.<$> targetParameters,
            Prelude.Just ("RoleArn" Data..= roleArn),
            Prelude.Just ("Source" Data..= source),
            Prelude.Just ("Target" Data..= target)
          ]
      )

instance Data.ToPath CreatePipe where
  toPath CreatePipe' {..} =
    Prelude.mconcat ["/v1/pipes/", Data.toBS name]

instance Data.ToQuery CreatePipe where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreatePipeResponse' smart constructor.
data CreatePipeResponse = CreatePipeResponse'
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
-- Create a value of 'CreatePipeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'createPipeResponse_arn' - The ARN of the pipe.
--
-- 'creationTime', 'createPipeResponse_creationTime' - The time the pipe was created.
--
-- 'currentState', 'createPipeResponse_currentState' - The state the pipe is in.
--
-- 'desiredState', 'createPipeResponse_desiredState' - The state the pipe should be in.
--
-- 'lastModifiedTime', 'createPipeResponse_lastModifiedTime' - When the pipe was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- 'name', 'createPipeResponse_name' - The name of the pipe.
--
-- 'httpStatus', 'createPipeResponse_httpStatus' - The response's http status code.
newCreatePipeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreatePipeResponse
newCreatePipeResponse pHttpStatus_ =
  CreatePipeResponse'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      currentState = Prelude.Nothing,
      desiredState = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the pipe.
createPipeResponse_arn :: Lens.Lens' CreatePipeResponse (Prelude.Maybe Prelude.Text)
createPipeResponse_arn = Lens.lens (\CreatePipeResponse' {arn} -> arn) (\s@CreatePipeResponse' {} a -> s {arn = a} :: CreatePipeResponse)

-- | The time the pipe was created.
createPipeResponse_creationTime :: Lens.Lens' CreatePipeResponse (Prelude.Maybe Prelude.UTCTime)
createPipeResponse_creationTime = Lens.lens (\CreatePipeResponse' {creationTime} -> creationTime) (\s@CreatePipeResponse' {} a -> s {creationTime = a} :: CreatePipeResponse) Prelude.. Lens.mapping Data._Time

-- | The state the pipe is in.
createPipeResponse_currentState :: Lens.Lens' CreatePipeResponse (Prelude.Maybe PipeState)
createPipeResponse_currentState = Lens.lens (\CreatePipeResponse' {currentState} -> currentState) (\s@CreatePipeResponse' {} a -> s {currentState = a} :: CreatePipeResponse)

-- | The state the pipe should be in.
createPipeResponse_desiredState :: Lens.Lens' CreatePipeResponse (Prelude.Maybe RequestedPipeState)
createPipeResponse_desiredState = Lens.lens (\CreatePipeResponse' {desiredState} -> desiredState) (\s@CreatePipeResponse' {} a -> s {desiredState = a} :: CreatePipeResponse)

-- | When the pipe was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
createPipeResponse_lastModifiedTime :: Lens.Lens' CreatePipeResponse (Prelude.Maybe Prelude.UTCTime)
createPipeResponse_lastModifiedTime = Lens.lens (\CreatePipeResponse' {lastModifiedTime} -> lastModifiedTime) (\s@CreatePipeResponse' {} a -> s {lastModifiedTime = a} :: CreatePipeResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the pipe.
createPipeResponse_name :: Lens.Lens' CreatePipeResponse (Prelude.Maybe Prelude.Text)
createPipeResponse_name = Lens.lens (\CreatePipeResponse' {name} -> name) (\s@CreatePipeResponse' {} a -> s {name = a} :: CreatePipeResponse)

-- | The response's http status code.
createPipeResponse_httpStatus :: Lens.Lens' CreatePipeResponse Prelude.Int
createPipeResponse_httpStatus = Lens.lens (\CreatePipeResponse' {httpStatus} -> httpStatus) (\s@CreatePipeResponse' {} a -> s {httpStatus = a} :: CreatePipeResponse)

instance Prelude.NFData CreatePipeResponse where
  rnf CreatePipeResponse' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf currentState
      `Prelude.seq` Prelude.rnf desiredState
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf httpStatus
