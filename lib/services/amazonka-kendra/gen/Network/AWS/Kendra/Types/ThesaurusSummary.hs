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
-- Module      : Network.AWS.Kendra.Types.ThesaurusSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Kendra.Types.ThesaurusSummary where

import qualified Network.AWS.Core as Core
import Network.AWS.Kendra.Types.ThesaurusStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | An array of summary information for a thesaurus or multiple thesauri.
--
-- /See:/ 'newThesaurusSummary' smart constructor.
data ThesaurusSummary = ThesaurusSummary'
  { -- | The status of the thesaurus.
    status :: Prelude.Maybe ThesaurusStatus,
    -- | The Unix datetime that the thesaurus was created.
    createdAt :: Prelude.Maybe Core.POSIX,
    -- | The name of the thesaurus.
    name :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the thesaurus.
    id :: Prelude.Maybe Prelude.Text,
    -- | The Unix datetime that the thesaurus was last updated.
    updatedAt :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ThesaurusSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'thesaurusSummary_status' - The status of the thesaurus.
--
-- 'createdAt', 'thesaurusSummary_createdAt' - The Unix datetime that the thesaurus was created.
--
-- 'name', 'thesaurusSummary_name' - The name of the thesaurus.
--
-- 'id', 'thesaurusSummary_id' - The identifier of the thesaurus.
--
-- 'updatedAt', 'thesaurusSummary_updatedAt' - The Unix datetime that the thesaurus was last updated.
newThesaurusSummary ::
  ThesaurusSummary
newThesaurusSummary =
  ThesaurusSummary'
    { status = Prelude.Nothing,
      createdAt = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The status of the thesaurus.
thesaurusSummary_status :: Lens.Lens' ThesaurusSummary (Prelude.Maybe ThesaurusStatus)
thesaurusSummary_status = Lens.lens (\ThesaurusSummary' {status} -> status) (\s@ThesaurusSummary' {} a -> s {status = a} :: ThesaurusSummary)

-- | The Unix datetime that the thesaurus was created.
thesaurusSummary_createdAt :: Lens.Lens' ThesaurusSummary (Prelude.Maybe Prelude.UTCTime)
thesaurusSummary_createdAt = Lens.lens (\ThesaurusSummary' {createdAt} -> createdAt) (\s@ThesaurusSummary' {} a -> s {createdAt = a} :: ThesaurusSummary) Prelude.. Lens.mapping Core._Time

-- | The name of the thesaurus.
thesaurusSummary_name :: Lens.Lens' ThesaurusSummary (Prelude.Maybe Prelude.Text)
thesaurusSummary_name = Lens.lens (\ThesaurusSummary' {name} -> name) (\s@ThesaurusSummary' {} a -> s {name = a} :: ThesaurusSummary)

-- | The identifier of the thesaurus.
thesaurusSummary_id :: Lens.Lens' ThesaurusSummary (Prelude.Maybe Prelude.Text)
thesaurusSummary_id = Lens.lens (\ThesaurusSummary' {id} -> id) (\s@ThesaurusSummary' {} a -> s {id = a} :: ThesaurusSummary)

-- | The Unix datetime that the thesaurus was last updated.
thesaurusSummary_updatedAt :: Lens.Lens' ThesaurusSummary (Prelude.Maybe Prelude.UTCTime)
thesaurusSummary_updatedAt = Lens.lens (\ThesaurusSummary' {updatedAt} -> updatedAt) (\s@ThesaurusSummary' {} a -> s {updatedAt = a} :: ThesaurusSummary) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON ThesaurusSummary where
  parseJSON =
    Core.withObject
      "ThesaurusSummary"
      ( \x ->
          ThesaurusSummary'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "CreatedAt")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "Id")
            Prelude.<*> (x Core..:? "UpdatedAt")
      )

instance Prelude.Hashable ThesaurusSummary

instance Prelude.NFData ThesaurusSummary
