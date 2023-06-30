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
-- Module      : Amazonka.Kendra.Types.ThesaurusSummary
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Kendra.Types.ThesaurusSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kendra.Types.ThesaurusStatus
import qualified Amazonka.Prelude as Prelude

-- | An array of summary information for a thesaurus or multiple thesauri.
--
-- /See:/ 'newThesaurusSummary' smart constructor.
data ThesaurusSummary = ThesaurusSummary'
  { -- | The Unix datetime that the thesaurus was created.
    createdAt :: Prelude.Maybe Data.POSIX,
    -- | The identifier of the thesaurus.
    id :: Prelude.Maybe Prelude.Text,
    -- | The name of the thesaurus.
    name :: Prelude.Maybe Prelude.Text,
    -- | The status of the thesaurus.
    status :: Prelude.Maybe ThesaurusStatus,
    -- | The Unix datetime that the thesaurus was last updated.
    updatedAt :: Prelude.Maybe Data.POSIX
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
-- 'createdAt', 'thesaurusSummary_createdAt' - The Unix datetime that the thesaurus was created.
--
-- 'id', 'thesaurusSummary_id' - The identifier of the thesaurus.
--
-- 'name', 'thesaurusSummary_name' - The name of the thesaurus.
--
-- 'status', 'thesaurusSummary_status' - The status of the thesaurus.
--
-- 'updatedAt', 'thesaurusSummary_updatedAt' - The Unix datetime that the thesaurus was last updated.
newThesaurusSummary ::
  ThesaurusSummary
newThesaurusSummary =
  ThesaurusSummary'
    { createdAt = Prelude.Nothing,
      id = Prelude.Nothing,
      name = Prelude.Nothing,
      status = Prelude.Nothing,
      updatedAt = Prelude.Nothing
    }

-- | The Unix datetime that the thesaurus was created.
thesaurusSummary_createdAt :: Lens.Lens' ThesaurusSummary (Prelude.Maybe Prelude.UTCTime)
thesaurusSummary_createdAt = Lens.lens (\ThesaurusSummary' {createdAt} -> createdAt) (\s@ThesaurusSummary' {} a -> s {createdAt = a} :: ThesaurusSummary) Prelude.. Lens.mapping Data._Time

-- | The identifier of the thesaurus.
thesaurusSummary_id :: Lens.Lens' ThesaurusSummary (Prelude.Maybe Prelude.Text)
thesaurusSummary_id = Lens.lens (\ThesaurusSummary' {id} -> id) (\s@ThesaurusSummary' {} a -> s {id = a} :: ThesaurusSummary)

-- | The name of the thesaurus.
thesaurusSummary_name :: Lens.Lens' ThesaurusSummary (Prelude.Maybe Prelude.Text)
thesaurusSummary_name = Lens.lens (\ThesaurusSummary' {name} -> name) (\s@ThesaurusSummary' {} a -> s {name = a} :: ThesaurusSummary)

-- | The status of the thesaurus.
thesaurusSummary_status :: Lens.Lens' ThesaurusSummary (Prelude.Maybe ThesaurusStatus)
thesaurusSummary_status = Lens.lens (\ThesaurusSummary' {status} -> status) (\s@ThesaurusSummary' {} a -> s {status = a} :: ThesaurusSummary)

-- | The Unix datetime that the thesaurus was last updated.
thesaurusSummary_updatedAt :: Lens.Lens' ThesaurusSummary (Prelude.Maybe Prelude.UTCTime)
thesaurusSummary_updatedAt = Lens.lens (\ThesaurusSummary' {updatedAt} -> updatedAt) (\s@ThesaurusSummary' {} a -> s {updatedAt = a} :: ThesaurusSummary) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ThesaurusSummary where
  parseJSON =
    Data.withObject
      "ThesaurusSummary"
      ( \x ->
          ThesaurusSummary'
            Prelude.<$> (x Data..:? "CreatedAt")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "UpdatedAt")
      )

instance Prelude.Hashable ThesaurusSummary where
  hashWithSalt _salt ThesaurusSummary' {..} =
    _salt
      `Prelude.hashWithSalt` createdAt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData ThesaurusSummary where
  rnf ThesaurusSummary' {..} =
    Prelude.rnf createdAt
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf updatedAt
