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
-- Module      : Network.AWS.CloudSearchDomains.Types.Hit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CloudSearchDomains.Types.Hit where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Information about a document that matches the search request.
--
-- /See:/ 'newHit' smart constructor.
data Hit = Hit'
  { -- | The document ID of a document that matches the search request.
    id :: Core.Maybe Core.Text,
    -- | The expressions returned from a document that matches the search
    -- request.
    exprs :: Core.Maybe (Core.HashMap Core.Text Core.Text),
    -- | The fields returned from a document that matches the search request.
    fields :: Core.Maybe (Core.HashMap Core.Text [Core.Text]),
    -- | The highlights returned from a document that matches the search request.
    highlights :: Core.Maybe (Core.HashMap Core.Text Core.Text)
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Hit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'hit_id' - The document ID of a document that matches the search request.
--
-- 'exprs', 'hit_exprs' - The expressions returned from a document that matches the search
-- request.
--
-- 'fields', 'hit_fields' - The fields returned from a document that matches the search request.
--
-- 'highlights', 'hit_highlights' - The highlights returned from a document that matches the search request.
newHit ::
  Hit
newHit =
  Hit'
    { id = Core.Nothing,
      exprs = Core.Nothing,
      fields = Core.Nothing,
      highlights = Core.Nothing
    }

-- | The document ID of a document that matches the search request.
hit_id :: Lens.Lens' Hit (Core.Maybe Core.Text)
hit_id = Lens.lens (\Hit' {id} -> id) (\s@Hit' {} a -> s {id = a} :: Hit)

-- | The expressions returned from a document that matches the search
-- request.
hit_exprs :: Lens.Lens' Hit (Core.Maybe (Core.HashMap Core.Text Core.Text))
hit_exprs = Lens.lens (\Hit' {exprs} -> exprs) (\s@Hit' {} a -> s {exprs = a} :: Hit) Core.. Lens.mapping Lens._Coerce

-- | The fields returned from a document that matches the search request.
hit_fields :: Lens.Lens' Hit (Core.Maybe (Core.HashMap Core.Text [Core.Text]))
hit_fields = Lens.lens (\Hit' {fields} -> fields) (\s@Hit' {} a -> s {fields = a} :: Hit) Core.. Lens.mapping Lens._Coerce

-- | The highlights returned from a document that matches the search request.
hit_highlights :: Lens.Lens' Hit (Core.Maybe (Core.HashMap Core.Text Core.Text))
hit_highlights = Lens.lens (\Hit' {highlights} -> highlights) (\s@Hit' {} a -> s {highlights = a} :: Hit) Core.. Lens.mapping Lens._Coerce

instance Core.FromJSON Hit where
  parseJSON =
    Core.withObject
      "Hit"
      ( \x ->
          Hit'
            Core.<$> (x Core..:? "id")
            Core.<*> (x Core..:? "exprs" Core..!= Core.mempty)
            Core.<*> (x Core..:? "fields" Core..!= Core.mempty)
            Core.<*> (x Core..:? "highlights" Core..!= Core.mempty)
      )

instance Core.Hashable Hit

instance Core.NFData Hit
