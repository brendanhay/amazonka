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
-- Module      : Amazonka.CloudSearchDomains.Types.Hit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CloudSearchDomains.Types.Hit where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a document that matches the search request.
--
-- /See:/ 'newHit' smart constructor.
data Hit = Hit'
  { -- | The expressions returned from a document that matches the search
    -- request.
    exprs :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The fields returned from a document that matches the search request.
    fields :: Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]),
    -- | The highlights returned from a document that matches the search request.
    highlights :: Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text),
    -- | The document ID of a document that matches the search request.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Hit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exprs', 'hit_exprs' - The expressions returned from a document that matches the search
-- request.
--
-- 'fields', 'hit_fields' - The fields returned from a document that matches the search request.
--
-- 'highlights', 'hit_highlights' - The highlights returned from a document that matches the search request.
--
-- 'id', 'hit_id' - The document ID of a document that matches the search request.
newHit ::
  Hit
newHit =
  Hit'
    { exprs = Prelude.Nothing,
      fields = Prelude.Nothing,
      highlights = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | The expressions returned from a document that matches the search
-- request.
hit_exprs :: Lens.Lens' Hit (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
hit_exprs = Lens.lens (\Hit' {exprs} -> exprs) (\s@Hit' {} a -> s {exprs = a} :: Hit) Prelude.. Lens.mapping Lens.coerced

-- | The fields returned from a document that matches the search request.
hit_fields :: Lens.Lens' Hit (Prelude.Maybe (Prelude.HashMap Prelude.Text [Prelude.Text]))
hit_fields = Lens.lens (\Hit' {fields} -> fields) (\s@Hit' {} a -> s {fields = a} :: Hit) Prelude.. Lens.mapping Lens.coerced

-- | The highlights returned from a document that matches the search request.
hit_highlights :: Lens.Lens' Hit (Prelude.Maybe (Prelude.HashMap Prelude.Text Prelude.Text))
hit_highlights = Lens.lens (\Hit' {highlights} -> highlights) (\s@Hit' {} a -> s {highlights = a} :: Hit) Prelude.. Lens.mapping Lens.coerced

-- | The document ID of a document that matches the search request.
hit_id :: Lens.Lens' Hit (Prelude.Maybe Prelude.Text)
hit_id = Lens.lens (\Hit' {id} -> id) (\s@Hit' {} a -> s {id = a} :: Hit)

instance Data.FromJSON Hit where
  parseJSON =
    Data.withObject
      "Hit"
      ( \x ->
          Hit'
            Prelude.<$> (x Data..:? "exprs" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "fields" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "highlights" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "id")
      )

instance Prelude.Hashable Hit where
  hashWithSalt _salt Hit' {..} =
    _salt `Prelude.hashWithSalt` exprs
      `Prelude.hashWithSalt` fields
      `Prelude.hashWithSalt` highlights
      `Prelude.hashWithSalt` id

instance Prelude.NFData Hit where
  rnf Hit' {..} =
    Prelude.rnf exprs
      `Prelude.seq` Prelude.rnf fields
      `Prelude.seq` Prelude.rnf highlights
      `Prelude.seq` Prelude.rnf id
