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
-- Module      : Amazonka.SSMContacts.Types.PreviewOverride
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSMContacts.Types.PreviewOverride where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about contacts and times that an on-call override replaces.
--
-- /See:/ 'newPreviewOverride' smart constructor.
data PreviewOverride = PreviewOverride'
  { -- | Information about the time a rotation override would end.
    endTime :: Prelude.Maybe Data.POSIX,
    -- | Information about contacts to add to an on-call rotation override.
    newMembers' :: Prelude.Maybe [Prelude.Text],
    -- | Information about the time a rotation override would begin.
    startTime :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PreviewOverride' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'endTime', 'previewOverride_endTime' - Information about the time a rotation override would end.
--
-- 'newMembers'', 'previewOverride_newMembers' - Information about contacts to add to an on-call rotation override.
--
-- 'startTime', 'previewOverride_startTime' - Information about the time a rotation override would begin.
newPreviewOverride ::
  PreviewOverride
newPreviewOverride =
  PreviewOverride'
    { endTime = Prelude.Nothing,
      newMembers' = Prelude.Nothing,
      startTime = Prelude.Nothing
    }

-- | Information about the time a rotation override would end.
previewOverride_endTime :: Lens.Lens' PreviewOverride (Prelude.Maybe Prelude.UTCTime)
previewOverride_endTime = Lens.lens (\PreviewOverride' {endTime} -> endTime) (\s@PreviewOverride' {} a -> s {endTime = a} :: PreviewOverride) Prelude.. Lens.mapping Data._Time

-- | Information about contacts to add to an on-call rotation override.
previewOverride_newMembers :: Lens.Lens' PreviewOverride (Prelude.Maybe [Prelude.Text])
previewOverride_newMembers = Lens.lens (\PreviewOverride' {newMembers'} -> newMembers') (\s@PreviewOverride' {} a -> s {newMembers' = a} :: PreviewOverride) Prelude.. Lens.mapping Lens.coerced

-- | Information about the time a rotation override would begin.
previewOverride_startTime :: Lens.Lens' PreviewOverride (Prelude.Maybe Prelude.UTCTime)
previewOverride_startTime = Lens.lens (\PreviewOverride' {startTime} -> startTime) (\s@PreviewOverride' {} a -> s {startTime = a} :: PreviewOverride) Prelude.. Lens.mapping Data._Time

instance Prelude.Hashable PreviewOverride where
  hashWithSalt _salt PreviewOverride' {..} =
    _salt
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` newMembers'
      `Prelude.hashWithSalt` startTime

instance Prelude.NFData PreviewOverride where
  rnf PreviewOverride' {..} =
    Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf newMembers'
      `Prelude.seq` Prelude.rnf startTime

instance Data.ToJSON PreviewOverride where
  toJSON PreviewOverride' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("EndTime" Data..=) Prelude.<$> endTime,
            ("NewMembers" Data..=) Prelude.<$> newMembers',
            ("StartTime" Data..=) Prelude.<$> startTime
          ]
      )
