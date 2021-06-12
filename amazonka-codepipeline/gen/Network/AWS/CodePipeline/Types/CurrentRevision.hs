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
-- Module      : Network.AWS.CodePipeline.Types.CurrentRevision
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CodePipeline.Types.CurrentRevision where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents information about a current revision.
--
-- /See:/ 'newCurrentRevision' smart constructor.
data CurrentRevision = CurrentRevision'
  { -- | The summary of the most recent revision of the artifact.
    revisionSummary :: Core.Maybe Core.Text,
    -- | The date and time when the most recent revision of the artifact was
    -- created, in timestamp format.
    created :: Core.Maybe Core.POSIX,
    -- | The revision ID of the current version of an artifact.
    revision :: Core.Text,
    -- | The change identifier for the current revision.
    changeIdentifier :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'CurrentRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'revisionSummary', 'currentRevision_revisionSummary' - The summary of the most recent revision of the artifact.
--
-- 'created', 'currentRevision_created' - The date and time when the most recent revision of the artifact was
-- created, in timestamp format.
--
-- 'revision', 'currentRevision_revision' - The revision ID of the current version of an artifact.
--
-- 'changeIdentifier', 'currentRevision_changeIdentifier' - The change identifier for the current revision.
newCurrentRevision ::
  -- | 'revision'
  Core.Text ->
  -- | 'changeIdentifier'
  Core.Text ->
  CurrentRevision
newCurrentRevision pRevision_ pChangeIdentifier_ =
  CurrentRevision'
    { revisionSummary = Core.Nothing,
      created = Core.Nothing,
      revision = pRevision_,
      changeIdentifier = pChangeIdentifier_
    }

-- | The summary of the most recent revision of the artifact.
currentRevision_revisionSummary :: Lens.Lens' CurrentRevision (Core.Maybe Core.Text)
currentRevision_revisionSummary = Lens.lens (\CurrentRevision' {revisionSummary} -> revisionSummary) (\s@CurrentRevision' {} a -> s {revisionSummary = a} :: CurrentRevision)

-- | The date and time when the most recent revision of the artifact was
-- created, in timestamp format.
currentRevision_created :: Lens.Lens' CurrentRevision (Core.Maybe Core.UTCTime)
currentRevision_created = Lens.lens (\CurrentRevision' {created} -> created) (\s@CurrentRevision' {} a -> s {created = a} :: CurrentRevision) Core.. Lens.mapping Core._Time

-- | The revision ID of the current version of an artifact.
currentRevision_revision :: Lens.Lens' CurrentRevision Core.Text
currentRevision_revision = Lens.lens (\CurrentRevision' {revision} -> revision) (\s@CurrentRevision' {} a -> s {revision = a} :: CurrentRevision)

-- | The change identifier for the current revision.
currentRevision_changeIdentifier :: Lens.Lens' CurrentRevision Core.Text
currentRevision_changeIdentifier = Lens.lens (\CurrentRevision' {changeIdentifier} -> changeIdentifier) (\s@CurrentRevision' {} a -> s {changeIdentifier = a} :: CurrentRevision)

instance Core.Hashable CurrentRevision

instance Core.NFData CurrentRevision

instance Core.ToJSON CurrentRevision where
  toJSON CurrentRevision' {..} =
    Core.object
      ( Core.catMaybes
          [ ("revisionSummary" Core..=)
              Core.<$> revisionSummary,
            ("created" Core..=) Core.<$> created,
            Core.Just ("revision" Core..= revision),
            Core.Just
              ("changeIdentifier" Core..= changeIdentifier)
          ]
      )
