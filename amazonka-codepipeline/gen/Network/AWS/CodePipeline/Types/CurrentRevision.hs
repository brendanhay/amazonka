{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents information about a current revision.
--
-- /See:/ 'newCurrentRevision' smart constructor.
data CurrentRevision = CurrentRevision'
  { -- | The summary of the most recent revision of the artifact.
    revisionSummary :: Prelude.Maybe Prelude.Text,
    -- | The date and time when the most recent revision of the artifact was
    -- created, in timestamp format.
    created :: Prelude.Maybe Prelude.POSIX,
    -- | The revision ID of the current version of an artifact.
    revision :: Prelude.Text,
    -- | The change identifier for the current revision.
    changeIdentifier :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'changeIdentifier'
  Prelude.Text ->
  CurrentRevision
newCurrentRevision pRevision_ pChangeIdentifier_ =
  CurrentRevision'
    { revisionSummary = Prelude.Nothing,
      created = Prelude.Nothing,
      revision = pRevision_,
      changeIdentifier = pChangeIdentifier_
    }

-- | The summary of the most recent revision of the artifact.
currentRevision_revisionSummary :: Lens.Lens' CurrentRevision (Prelude.Maybe Prelude.Text)
currentRevision_revisionSummary = Lens.lens (\CurrentRevision' {revisionSummary} -> revisionSummary) (\s@CurrentRevision' {} a -> s {revisionSummary = a} :: CurrentRevision)

-- | The date and time when the most recent revision of the artifact was
-- created, in timestamp format.
currentRevision_created :: Lens.Lens' CurrentRevision (Prelude.Maybe Prelude.UTCTime)
currentRevision_created = Lens.lens (\CurrentRevision' {created} -> created) (\s@CurrentRevision' {} a -> s {created = a} :: CurrentRevision) Prelude.. Lens.mapping Prelude._Time

-- | The revision ID of the current version of an artifact.
currentRevision_revision :: Lens.Lens' CurrentRevision Prelude.Text
currentRevision_revision = Lens.lens (\CurrentRevision' {revision} -> revision) (\s@CurrentRevision' {} a -> s {revision = a} :: CurrentRevision)

-- | The change identifier for the current revision.
currentRevision_changeIdentifier :: Lens.Lens' CurrentRevision Prelude.Text
currentRevision_changeIdentifier = Lens.lens (\CurrentRevision' {changeIdentifier} -> changeIdentifier) (\s@CurrentRevision' {} a -> s {changeIdentifier = a} :: CurrentRevision)

instance Prelude.Hashable CurrentRevision

instance Prelude.NFData CurrentRevision

instance Prelude.ToJSON CurrentRevision where
  toJSON CurrentRevision' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("revisionSummary" Prelude..=)
              Prelude.<$> revisionSummary,
            ("created" Prelude..=) Prelude.<$> created,
            Prelude.Just ("revision" Prelude..= revision),
            Prelude.Just
              ("changeIdentifier" Prelude..= changeIdentifier)
          ]
      )
