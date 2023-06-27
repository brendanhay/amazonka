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
-- Module      : Amazonka.CodePipeline.Types.CurrentRevision
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CodePipeline.Types.CurrentRevision where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents information about a current revision.
--
-- /See:/ 'newCurrentRevision' smart constructor.
data CurrentRevision = CurrentRevision'
  { -- | The date and time when the most recent revision of the artifact was
    -- created, in timestamp format.
    created :: Prelude.Maybe Data.POSIX,
    -- | The summary of the most recent revision of the artifact.
    revisionSummary :: Prelude.Maybe Prelude.Text,
    -- | The revision ID of the current version of an artifact.
    revision :: Prelude.Text,
    -- | The change identifier for the current revision.
    changeIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CurrentRevision' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'created', 'currentRevision_created' - The date and time when the most recent revision of the artifact was
-- created, in timestamp format.
--
-- 'revisionSummary', 'currentRevision_revisionSummary' - The summary of the most recent revision of the artifact.
--
-- 'revision', 'currentRevision_revision' - The revision ID of the current version of an artifact.
--
-- 'changeIdentifier', 'currentRevision_changeIdentifier' - The change identifier for the current revision.
newCurrentRevision ::
  -- | 'revision'
  Prelude.Text ->
  -- | 'changeIdentifier'
  Prelude.Text ->
  CurrentRevision
newCurrentRevision pRevision_ pChangeIdentifier_ =
  CurrentRevision'
    { created = Prelude.Nothing,
      revisionSummary = Prelude.Nothing,
      revision = pRevision_,
      changeIdentifier = pChangeIdentifier_
    }

-- | The date and time when the most recent revision of the artifact was
-- created, in timestamp format.
currentRevision_created :: Lens.Lens' CurrentRevision (Prelude.Maybe Prelude.UTCTime)
currentRevision_created = Lens.lens (\CurrentRevision' {created} -> created) (\s@CurrentRevision' {} a -> s {created = a} :: CurrentRevision) Prelude.. Lens.mapping Data._Time

-- | The summary of the most recent revision of the artifact.
currentRevision_revisionSummary :: Lens.Lens' CurrentRevision (Prelude.Maybe Prelude.Text)
currentRevision_revisionSummary = Lens.lens (\CurrentRevision' {revisionSummary} -> revisionSummary) (\s@CurrentRevision' {} a -> s {revisionSummary = a} :: CurrentRevision)

-- | The revision ID of the current version of an artifact.
currentRevision_revision :: Lens.Lens' CurrentRevision Prelude.Text
currentRevision_revision = Lens.lens (\CurrentRevision' {revision} -> revision) (\s@CurrentRevision' {} a -> s {revision = a} :: CurrentRevision)

-- | The change identifier for the current revision.
currentRevision_changeIdentifier :: Lens.Lens' CurrentRevision Prelude.Text
currentRevision_changeIdentifier = Lens.lens (\CurrentRevision' {changeIdentifier} -> changeIdentifier) (\s@CurrentRevision' {} a -> s {changeIdentifier = a} :: CurrentRevision)

instance Prelude.Hashable CurrentRevision where
  hashWithSalt _salt CurrentRevision' {..} =
    _salt
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` revisionSummary
      `Prelude.hashWithSalt` revision
      `Prelude.hashWithSalt` changeIdentifier

instance Prelude.NFData CurrentRevision where
  rnf CurrentRevision' {..} =
    Prelude.rnf created
      `Prelude.seq` Prelude.rnf revisionSummary
      `Prelude.seq` Prelude.rnf revision
      `Prelude.seq` Prelude.rnf changeIdentifier

instance Data.ToJSON CurrentRevision where
  toJSON CurrentRevision' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("created" Data..=) Prelude.<$> created,
            ("revisionSummary" Data..=)
              Prelude.<$> revisionSummary,
            Prelude.Just ("revision" Data..= revision),
            Prelude.Just
              ("changeIdentifier" Data..= changeIdentifier)
          ]
      )
