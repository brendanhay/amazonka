{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.Pipes.Types.Pipe
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pipes.Types.Pipe where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Pipes.Types.PipeState
import Amazonka.Pipes.Types.RequestedPipeState
import qualified Amazonka.Prelude as Prelude

-- | An object that represents a pipe. Amazon EventBridgePipes connect event
-- sources to targets and reduces the need for specialized knowledge and
-- integration code.
--
-- /See:/ 'newPipe' smart constructor.
data Pipe = Pipe'
  { -- | The ARN of the pipe.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time the pipe was created.
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | The state the pipe is in.
    currentState :: Prelude.Maybe PipeState,
    -- | The state the pipe should be in.
    desiredState :: Prelude.Maybe RequestedPipeState,
    -- | The ARN of the enrichment resource.
    enrichment :: Prelude.Maybe Prelude.Text,
    -- | When the pipe was last updated, in
    -- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
    -- (YYYY-MM-DDThh:mm:ss.sTZD).
    lastModifiedTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the pipe.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the source resource.
    source :: Prelude.Maybe Prelude.Text,
    -- | The reason the pipe is in its current state.
    stateReason :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the target resource.
    target :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Pipe' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'pipe_arn' - The ARN of the pipe.
--
-- 'creationTime', 'pipe_creationTime' - The time the pipe was created.
--
-- 'currentState', 'pipe_currentState' - The state the pipe is in.
--
-- 'desiredState', 'pipe_desiredState' - The state the pipe should be in.
--
-- 'enrichment', 'pipe_enrichment' - The ARN of the enrichment resource.
--
-- 'lastModifiedTime', 'pipe_lastModifiedTime' - When the pipe was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
--
-- 'name', 'pipe_name' - The name of the pipe.
--
-- 'source', 'pipe_source' - The ARN of the source resource.
--
-- 'stateReason', 'pipe_stateReason' - The reason the pipe is in its current state.
--
-- 'target', 'pipe_target' - The ARN of the target resource.
newPipe ::
  Pipe
newPipe =
  Pipe'
    { arn = Prelude.Nothing,
      creationTime = Prelude.Nothing,
      currentState = Prelude.Nothing,
      desiredState = Prelude.Nothing,
      enrichment = Prelude.Nothing,
      lastModifiedTime = Prelude.Nothing,
      name = Prelude.Nothing,
      source = Prelude.Nothing,
      stateReason = Prelude.Nothing,
      target = Prelude.Nothing
    }

-- | The ARN of the pipe.
pipe_arn :: Lens.Lens' Pipe (Prelude.Maybe Prelude.Text)
pipe_arn = Lens.lens (\Pipe' {arn} -> arn) (\s@Pipe' {} a -> s {arn = a} :: Pipe)

-- | The time the pipe was created.
pipe_creationTime :: Lens.Lens' Pipe (Prelude.Maybe Prelude.UTCTime)
pipe_creationTime = Lens.lens (\Pipe' {creationTime} -> creationTime) (\s@Pipe' {} a -> s {creationTime = a} :: Pipe) Prelude.. Lens.mapping Data._Time

-- | The state the pipe is in.
pipe_currentState :: Lens.Lens' Pipe (Prelude.Maybe PipeState)
pipe_currentState = Lens.lens (\Pipe' {currentState} -> currentState) (\s@Pipe' {} a -> s {currentState = a} :: Pipe)

-- | The state the pipe should be in.
pipe_desiredState :: Lens.Lens' Pipe (Prelude.Maybe RequestedPipeState)
pipe_desiredState = Lens.lens (\Pipe' {desiredState} -> desiredState) (\s@Pipe' {} a -> s {desiredState = a} :: Pipe)

-- | The ARN of the enrichment resource.
pipe_enrichment :: Lens.Lens' Pipe (Prelude.Maybe Prelude.Text)
pipe_enrichment = Lens.lens (\Pipe' {enrichment} -> enrichment) (\s@Pipe' {} a -> s {enrichment = a} :: Pipe)

-- | When the pipe was last updated, in
-- <https://www.w3.org/TR/NOTE-datetime ISO-8601 format>
-- (YYYY-MM-DDThh:mm:ss.sTZD).
pipe_lastModifiedTime :: Lens.Lens' Pipe (Prelude.Maybe Prelude.UTCTime)
pipe_lastModifiedTime = Lens.lens (\Pipe' {lastModifiedTime} -> lastModifiedTime) (\s@Pipe' {} a -> s {lastModifiedTime = a} :: Pipe) Prelude.. Lens.mapping Data._Time

-- | The name of the pipe.
pipe_name :: Lens.Lens' Pipe (Prelude.Maybe Prelude.Text)
pipe_name = Lens.lens (\Pipe' {name} -> name) (\s@Pipe' {} a -> s {name = a} :: Pipe)

-- | The ARN of the source resource.
pipe_source :: Lens.Lens' Pipe (Prelude.Maybe Prelude.Text)
pipe_source = Lens.lens (\Pipe' {source} -> source) (\s@Pipe' {} a -> s {source = a} :: Pipe)

-- | The reason the pipe is in its current state.
pipe_stateReason :: Lens.Lens' Pipe (Prelude.Maybe Prelude.Text)
pipe_stateReason = Lens.lens (\Pipe' {stateReason} -> stateReason) (\s@Pipe' {} a -> s {stateReason = a} :: Pipe)

-- | The ARN of the target resource.
pipe_target :: Lens.Lens' Pipe (Prelude.Maybe Prelude.Text)
pipe_target = Lens.lens (\Pipe' {target} -> target) (\s@Pipe' {} a -> s {target = a} :: Pipe)

instance Data.FromJSON Pipe where
  parseJSON =
    Data.withObject
      "Pipe"
      ( \x ->
          Pipe'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "CurrentState")
            Prelude.<*> (x Data..:? "DesiredState")
            Prelude.<*> (x Data..:? "Enrichment")
            Prelude.<*> (x Data..:? "LastModifiedTime")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Source")
            Prelude.<*> (x Data..:? "StateReason")
            Prelude.<*> (x Data..:? "Target")
      )

instance Prelude.Hashable Pipe where
  hashWithSalt _salt Pipe' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` currentState
      `Prelude.hashWithSalt` desiredState
      `Prelude.hashWithSalt` enrichment
      `Prelude.hashWithSalt` lastModifiedTime
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` source
      `Prelude.hashWithSalt` stateReason
      `Prelude.hashWithSalt` target

instance Prelude.NFData Pipe where
  rnf Pipe' {..} =
    Prelude.rnf arn
      `Prelude.seq` Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf currentState
      `Prelude.seq` Prelude.rnf desiredState
      `Prelude.seq` Prelude.rnf enrichment
      `Prelude.seq` Prelude.rnf lastModifiedTime
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf source
      `Prelude.seq` Prelude.rnf stateReason
      `Prelude.seq` Prelude.rnf target
