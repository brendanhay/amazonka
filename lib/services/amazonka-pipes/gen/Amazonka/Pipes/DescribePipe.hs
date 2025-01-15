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
-- Module      : Amazonka.Pipes.DescribePipe
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get the information about an existing pipe. For more information about
-- pipes, see
-- <https://docs.aws.amazon.com/eventbridge/latest/userguide/eb-pipes.html Amazon EventBridge Pipes>
-- in the Amazon EventBridge User Guide.
module Amazonka.Pipes.DescribePipe
  ( -- * Creating a Request
    DescribePipe (..),
    newDescribePipe,

    -- * Request Lenses
    describePipe_name,

    -- * Destructuring the Response
    DescribePipeResponse (..),
    newDescribePipeResponse,

    -- * Response Lenses
    describePipeResponse_arn,
    describePipeResponse_creationTime,
    describePipeResponse_currentState,
    describePipeResponse_description,
    describePipeResponse_desiredState,
    describePipeResponse_enrichment,
    describePipeResponse_enrichmentParameters,
    describePipeResponse_lastModifiedTime,
    describePipeResponse_name,
    describePipeResponse_roleArn,
    describePipeResponse_source,
    describePipeResponse_sourceParameters,
    describePipeResponse_stateReason,
    describePipeResponse_tags,
    describePipeResponse_target,
    describePipeResponse_targetParameters,
    describePipeResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDescribePipe' smart constructor.
data DescribePipe = DescribePipe'
  { -- | The name of the pipe.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePipe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'describePipe_name' - The name of the pipe.
newDescribePipe ::
  -- | 'name'
  Prelude.Text ->
  DescribePipe
newDescribePipe pName_ = DescribePipe' {name = pName_}

-- | The name of the pipe.
describePipe_name :: Lens.Lens' DescribePipe Prelude.Text
describePipe_name = Lens.lens (\DescribePipe' {name} -> name) (\s@DescribePipe' {} a -> s {name = a} :: DescribePipe)

instance Core.AWSRequest DescribePipe where
  type AWSResponse DescribePipe = DescribePipeResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribePipeResponse'
            Prelude.<$> (x Data..?> "Arn")
            Prelude.<*> (x Data..?> "CreationTime")
            Prelude.<*> (x Data..?> "CurrentState")
            Prelude.<*> (x Data..?> "Description")
            Prelude.<*> (x Data..?> "DesiredState")
            Prelude.<*> (x Data..?> "Enrichment")
            Prelude.<*> (x Data..?> "EnrichmentParameters")
            Prelude.<*> (x Data..?> "LastModifiedTime")
            Prelude.<*> (x Data..?> "Name")
            Prelude.<*> (x Data..?> "RoleArn")
            Prelude.<*> (x Data..?> "Source")
            Prelude.<*> (x Data..?> "SourceParameters")
            Prelude.<*> (x Data..?> "StateReason")
            Prelude.<*> (x Data..?> "Tags" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "Target")
            Prelude.<*> (x Data..?> "TargetParameters")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribePipe where
  hashWithSalt _salt DescribePipe' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData DescribePipe where
  rnf DescribePipe' {..} = Prelude.rnf name

instance Data.ToHeaders DescribePipe where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath DescribePipe where
  toPath DescribePipe' {..} =
    Prelude.mconcat ["/v1/pipes/", Data.toBS name]

instance Data.ToQuery DescribePipe where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribePipeResponse' smart constructor.
data DescribePipeResponse = DescribePipeResponse'
  { -- | The ARN of the pipe.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time the pipe was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The state the pipe is in.
    currentState :: Prelude.Maybe PipeState,
    -- | A description of the pipe.
    description :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The state the pipe should be in.
    desiredState :: Prelude.Maybe RequestedPipeStateDescribeResponse,
    -- | The ARN of the enrichment resource.
    enrichment :: Prelude.Maybe Prelude.Text,
    -- | The parameters required to set up enrichment on your pipe.
    enrichmentParameters :: Prelude.Maybe PipeEnrichmentParameters,
    -- | When the pipe was last updated, in
    -- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
    -- (YYYY-MM-DDThh:mm:ss.sTZD).
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the pipe.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the role that allows the pipe to send data to the target.
    roleArn :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the source resource.
    source :: Prelude.Maybe Prelude.Text,
    -- | The parameters required to set up a source for your pipe.
    sourceParameters :: Prelude.Maybe PipeSourceParameters,
    -- | The reason the pipe is in its current state.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | The list of key-value pairs to associate with the pipe.
    tags :: Prelude.Maybe (Prelude.HashMap Prelude.Text (Data.Sensitive Prelude.Text)),
    -- | The ARN of the target resource.
    target :: Prelude.Maybe Prelude.Text,
    -- | The parameters required to set up a target for your pipe.
    targetParameters :: Prelude.Maybe PipeTargetParameters,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribePipeResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'describePipeResponse_arn' - The ARN of the pipe.
--
-- 'creationTime', 'describePipeResponse_creationTime' - The time the pipe was created.
--
-- 'currentState', 'describePipeResponse_currentState' - The state the pipe is in.
--
-- 'description', 'describePipeResponse_description' - A description of the pipe.
--
-- 'desiredState', 'describePipeResponse_desiredState' - The state the pipe should be in.
--
-- 'enrichment', 'describePipeResponse_enrichment' - The ARN of the enrichment resource.
--
-- 'enrichmentParameters', 'describePipeResponse_enrichmentParameters' - The parameters required to set up enrichment on your pipe.
--
-- 'lastModifiedTime', 'describePipeResponse_lastModifiedTime' - When the pipe was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- 'name', 'describePipeResponse_name' - The name of the pipe.
--
-- 'roleArn', 'describePipeResponse_roleArn' - The ARN of the role that allows the pipe to send data to the target.
--
-- 'source', 'describePipeResponse_source' - The ARN of the source resource.
--
-- 'sourceParameters', 'describePipeResponse_sourceParameters' - The parameters required to set up a source for your pipe.
--
-- 'stateReason', 'describePipeResponse_stateReason' - The reason the pipe is in its current state.
--
-- 'tags', 'describePipeResponse_tags' - The list of key-value pairs to associate with the pipe.
--
-- 'target', 'describePipeResponse_target' - The ARN of the target resource.
--
-- 'targetParameters', 'describePipeResponse_targetParameters' - The parameters required to set up a target for your pipe.
--
-- 'httpStatus', 'describePipeResponse_httpStatus' - The response's http status code.
newDescribePipeResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribePipeResponse
newDescribePipeResponse pHttpStatus_ =
  DescribePipeResponse'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      currentState = Prelude.Nothing,
      description = Prelude.Nothing,
      desiredState = Prelude.Nothing,
      enrichment = Prelude.Nothing,
      enrichmentParameters = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      roleArn = Prelude.Nothing,
      source = Prelude.Nothing,
      sourceParameters = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      tags = Prelude.Nothing,
      target = Prelude.Nothing,
      targetParameters = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ARN of the pipe.
describePipeResponse_arn :: Lens.Lens' DescribePipeResponse (Prelude.Maybe Prelude.Text)
describePipeResponse_arn = Lens.lens (\DescribePipeResponse' {arn} -> arn) (\s@DescribePipeResponse' {} a -> s {arn = a} :: DescribePipeResponse)

-- | The time the pipe was created.
describePipeResponse_creationTime :: Lens.Lens' DescribePipeResponse (Prelude.Maybe Prelude.UTCTime)
describePipeResponse_creationTime = Lens.lens (\DescribePipeResponse' {creationTime} -> creationTime) (\s@DescribePipeResponse' {} a -> s {creationTime = a} :: DescribePipeResponse) Prelude.. Lens.mapping Data._Time

-- | The state the pipe is in.
describePipeResponse_currentState :: Lens.Lens' DescribePipeResponse (Prelude.Maybe PipeState)
describePipeResponse_currentState = Lens.lens (\DescribePipeResponse' {currentState} -> currentState) (\s@DescribePipeResponse' {} a -> s {currentState = a} :: DescribePipeResponse)

-- | A description of the pipe.
describePipeResponse_description :: Lens.Lens' DescribePipeResponse (Prelude.Maybe Prelude.Text)
describePipeResponse_description = Lens.lens (\DescribePipeResponse' {description} -> description) (\s@DescribePipeResponse' {} a -> s {description = a} :: DescribePipeResponse) Prelude.. Lens.mapping Data._Sensitive

-- | The state the pipe should be in.
describePipeResponse_desiredState :: Lens.Lens' DescribePipeResponse (Prelude.Maybe RequestedPipeStateDescribeResponse)
describePipeResponse_desiredState = Lens.lens (\DescribePipeResponse' {desiredState} -> desiredState) (\s@DescribePipeResponse' {} a -> s {desiredState = a} :: DescribePipeResponse)

-- | The ARN of the enrichment resource.
describePipeResponse_enrichment :: Lens.Lens' DescribePipeResponse (Prelude.Maybe Prelude.Text)
describePipeResponse_enrichment = Lens.lens (\DescribePipeResponse' {enrichment} -> enrichment) (\s@DescribePipeResponse' {} a -> s {enrichment = a} :: DescribePipeResponse)

-- | The parameters required to set up enrichment on your pipe.
describePipeResponse_enrichmentParameters :: Lens.Lens' DescribePipeResponse (Prelude.Maybe PipeEnrichmentParameters)
describePipeResponse_enrichmentParameters = Lens.lens (\DescribePipeResponse' {enrichmentParameters} -> enrichmentParameters) (\s@DescribePipeResponse' {} a -> s {enrichmentParameters = a} :: DescribePipeResponse)

-- | When the pipe was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
describePipeResponse_lastModifiedTime :: Lens.Lens' DescribePipeResponse (Prelude.Maybe Prelude.UTCTime)
describePipeResponse_lastModifiedTime = Lens.lens (\DescribePipeResponse' {lastModifiedTime} -> lastModifiedTime) (\s@DescribePipeResponse' {} a -> s {lastModifiedTime = a} :: DescribePipeResponse) Prelude.. Lens.mapping Data._Time

-- | The name of the pipe.
describePipeResponse_name :: Lens.Lens' DescribePipeResponse (Prelude.Maybe Prelude.Text)
describePipeResponse_name = Lens.lens (\DescribePipeResponse' {name} -> name) (\s@DescribePipeResponse' {} a -> s {name = a} :: DescribePipeResponse)

-- | The ARN of the role that allows the pipe to send data to the target.
describePipeResponse_roleArn :: Lens.Lens' DescribePipeResponse (Prelude.Maybe Prelude.Text)
describePipeResponse_roleArn = Lens.lens (\DescribePipeResponse' {roleArn} -> roleArn) (\s@DescribePipeResponse' {} a -> s {roleArn = a} :: DescribePipeResponse)

-- | The ARN of the source resource.
describePipeResponse_source :: Lens.Lens' DescribePipeResponse (Prelude.Maybe Prelude.Text)
describePipeResponse_source = Lens.lens (\DescribePipeResponse' {source} -> source) (\s@DescribePipeResponse' {} a -> s {source = a} :: DescribePipeResponse)

-- | The parameters required to set up a source for your pipe.
describePipeResponse_sourceParameters :: Lens.Lens' DescribePipeResponse (Prelude.Maybe PipeSourceParameters)
describePipeResponse_sourceParameters = Lens.lens (\DescribePipeResponse' {sourceParameters} -> sourceParameters) (\s@DescribePipeResponse' {} a -> s {sourceParameters = a} :: DescribePipeResponse)

-- | The reason the pipe is in its current state.
describePipeResponse_stateReason :: Lens.Lens' DescribePipeResponse (Prelude.Maybe Prelude.Text)
describePipeResponse_stateReason = Lens.lens (\DescribePipeResponse' {stateReason} -> stateReason) (\s@DescribePipeResponse' {} a -> s {stateReason = a} :: DescribePipeResponse)

-- | The list of key-value pairs to associate with the pipe.
describePipeResponse_tags :: Lens.Lens' DescribePipeResponse (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
describePipeResponse_tags = Lens.lens (\DescribePipeResponse' {tags} -> tags) (\s@DescribePipeResponse' {} a -> s {tags = a} :: DescribePipeResponse) Prelude.. Lens.mapping Lens.coerced

-- | The ARN of the target resource.
describePipeResponse_target :: Lens.Lens' DescribePipeResponse (Prelude.Maybe Prelude.Text)
describePipeResponse_target = Lens.lens (\DescribePipeResponse' {target} -> target) (\s@DescribePipeResponse' {} a -> s {target = a} :: DescribePipeResponse)

-- | The parameters required to set up a target for your pipe.
describePipeResponse_targetParameters :: Lens.Lens' DescribePipeResponse (Prelude.Maybe PipeTargetParameters)
describePipeResponse_targetParameters = Lens.lens (\DescribePipeResponse' {targetParameters} -> targetParameters) (\s@DescribePipeResponse' {} a -> s {targetParameters = a} :: DescribePipeResponse)

-- | The response's http status code.
describePipeResponse_httpStatus :: Lens.Lens' DescribePipeResponse Prelude.Int
describePipeResponse_httpStatus = Lens.lens (\DescribePipeResponse' {httpStatus} -> httpStatus) (\s@DescribePipeResponse' {} a -> s {httpStatus = a} :: DescribePipeResponse)

instance Prelude.NFData DescribePipeResponse where
  rnf DescribePipeResponse' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf creationTime `Prelude.seq`
        Prelude.rnf currentState `Prelude.seq`
          Prelude.rnf description `Prelude.seq`
            Prelude.rnf desiredState `Prelude.seq`
              Prelude.rnf enrichment `Prelude.seq`
                Prelude.rnf enrichmentParameters `Prelude.seq`
                  Prelude.rnf lastModifiedTime `Prelude.seq`
                    Prelude.rnf name `Prelude.seq`
                      Prelude.rnf roleArn `Prelude.seq`
                        Prelude.rnf source `Prelude.seq`
                          Prelude.rnf sourceParameters `Prelude.seq`
                            Prelude.rnf stateReason `Prelude.seq`
                              Prelude.rnf tags `Prelude.seq`
                                Prelude.rnf target `Prelude.seq`
                                  Prelude.rnf targetParameters `Prelude.seq`
                                    Prelude.rnf httpStatus
