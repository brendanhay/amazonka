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
-- Module      : Network.AWS.CodePipeline.Types.ActionRevision
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.ActionRevision where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

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
    created :: Core.POSIX
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
        created = Core._Time Lens.# pCreated_
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
actionRevision_created = Lens.lens (\ActionRevision' {created} -> created) (\s@ActionRevision' {} a -> s {created = a} :: ActionRevision) Prelude.. Core._Time

instance Core.FromJSON ActionRevision where
  parseJSON =
    Core.withObject
      "ActionRevision"
      ( \x ->
          ActionRevision'
            Prelude.<$> (x Core..: "revisionId")
            Prelude.<*> (x Core..: "revisionChangeId")
            Prelude.<*> (x Core..: "created")
      )

instance Prelude.Hashable ActionRevision

instance Prelude.NFData ActionRevision

instance Core.ToJSON ActionRevision where
  toJSON ActionRevision' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("revisionId" Core..= revisionId),
            Prelude.Just
              ("revisionChangeId" Core..= revisionChangeId),
            Prelude.Just ("created" Core..= created)
          ]
      )
