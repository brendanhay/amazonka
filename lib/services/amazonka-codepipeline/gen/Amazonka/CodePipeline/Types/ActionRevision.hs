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
-- Module      : Amazonka.CodePipeline.Types.ActionRevision
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.ActionRevision where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about the version (or revision) of an action.
--
-- /See:/ 'newActionRevision' smart constructor.
data ActionRevision = ActionRevision'
  { -- | The system-generated unique ID that identifies the revision number of
    -- the action.
    revisionId :: Prelude.Text,
    -- | The unique identifier of the change that set the state to this revision
    -- (for example, a deployment ID or timestamp).
    revisionChangeId :: Prelude.Text,
    -- | The date and time when the most recent version of the action was
    -- created, in timestamp format.
    created :: Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ActionRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionId', 'actionRevision_revisionId' - The system-generated unique ID that identifies the revision number of
-- the action.
--
-- 'revisionChangeId', 'actionRevision_revisionChangeId' - The unique identifier of the change that set the state to this revision
-- (for example, a deployment ID or timestamp).
--
-- 'created', 'actionRevision_created' - The date and time when the most recent version of the action was
-- created, in timestamp format.
newActionRevision ::
  -- | 'revisionId'
  Prelude.Text ->
  -- | 'revisionChangeId'
  Prelude.Text ->
  -- | 'created'
  Prelude.UTCTime ->
  ActionRevision
newActionRevision
  pRevisionId_
  pRevisionChangeId_
  pCreated_ =
    ActionRevision'
      { revisionId = pRevisionId_,
        revisionChangeId = pRevisionChangeId_,
        created = Data._Time Lens.# pCreated_
      }

-- | The system-generated unique ID that identifies the revision number of
-- the action.
actionRevision_revisionId :: Lens.Lens' ActionRevision Prelude.Text
actionRevision_revisionId = Lens.lens (\ActionRevision' {revisionId} -> revisionId) (\s@ActionRevision' {} a -> s {revisionId = a} :: ActionRevision)

-- | The unique identifier of the change that set the state to this revision
-- (for example, a deployment ID or timestamp).
actionRevision_revisionChangeId :: Lens.Lens' ActionRevision Prelude.Text
actionRevision_revisionChangeId = Lens.lens (\ActionRevision' {revisionChangeId} -> revisionChangeId) (\s@ActionRevision' {} a -> s {revisionChangeId = a} :: ActionRevision)

-- | The date and time when the most recent version of the action was
-- created, in timestamp format.
actionRevision_created :: Lens.Lens' ActionRevision Prelude.UTCTime
actionRevision_created = Lens.lens (\ActionRevision' {created} -> created) (\s@ActionRevision' {} a -> s {created = a} :: ActionRevision) Prelude.. Data._Time

instance Data.FromJSON ActionRevision where
  parseJSON =
    Data.withObject
      "ActionRevision"
      ( \x ->
          ActionRevision'
            Prelude.<$> (x Data..: "revisionId")
            Prelude.<*> (x Data..: "revisionChangeId")
            Prelude.<*> (x Data..: "created")
      )

instance Prelude.Hashable ActionRevision where
  hashWithSalt _salt ActionRevision' {..} =
    _salt
      `Prelude.hashWithSalt` revisionId
      `Prelude.hashWithSalt` revisionChangeId
      `Prelude.hashWithSalt` created

instance Prelude.NFData ActionRevision where
  rnf ActionRevision' {..} =
    Prelude.rnf revisionId `Prelude.seq`
      Prelude.rnf revisionChangeId `Prelude.seq`
        Prelude.rnf created

instance Data.ToJSON ActionRevision where
  toJSON ActionRevision' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("revisionId" Data..= revisionId),
            Prelude.Just
              ("revisionChangeId" Data..= revisionChangeId),
            Prelude.Just ("created" Data..= created)
          ]
      )
