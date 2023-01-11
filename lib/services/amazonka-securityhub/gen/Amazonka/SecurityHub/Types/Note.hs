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
-- Module      : Amazonka.SecurityHub.Types.Note
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.Note where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A user-defined note added to a finding.
--
-- /See:/ 'newNote' smart constructor.
data Note = Note'
  { -- | The text of a note.
    text :: Prelude.Text,
    -- | The principal that created a note.
    updatedBy :: Prelude.Text,
    -- | The timestamp of when the note was updated.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    updatedAt :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Note' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'text', 'note_text' - The text of a note.
--
-- 'updatedBy', 'note_updatedBy' - The principal that created a note.
--
-- 'updatedAt', 'note_updatedAt' - The timestamp of when the note was updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
newNote ::
  -- | 'text'
  Prelude.Text ->
  -- | 'updatedBy'
  Prelude.Text ->
  -- | 'updatedAt'
  Prelude.Text ->
  Note
newNote pText_ pUpdatedBy_ pUpdatedAt_ =
  Note'
    { text = pText_,
      updatedBy = pUpdatedBy_,
      updatedAt = pUpdatedAt_
    }

-- | The text of a note.
note_text :: Lens.Lens' Note Prelude.Text
note_text = Lens.lens (\Note' {text} -> text) (\s@Note' {} a -> s {text = a} :: Note)

-- | The principal that created a note.
note_updatedBy :: Lens.Lens' Note Prelude.Text
note_updatedBy = Lens.lens (\Note' {updatedBy} -> updatedBy) (\s@Note' {} a -> s {updatedBy = a} :: Note)

-- | The timestamp of when the note was updated.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
note_updatedAt :: Lens.Lens' Note Prelude.Text
note_updatedAt = Lens.lens (\Note' {updatedAt} -> updatedAt) (\s@Note' {} a -> s {updatedAt = a} :: Note)

instance Data.FromJSON Note where
  parseJSON =
    Data.withObject
      "Note"
      ( \x ->
          Note'
            Prelude.<$> (x Data..: "Text")
            Prelude.<*> (x Data..: "UpdatedBy")
            Prelude.<*> (x Data..: "UpdatedAt")
      )

instance Prelude.Hashable Note where
  hashWithSalt _salt Note' {..} =
    _salt `Prelude.hashWithSalt` text
      `Prelude.hashWithSalt` updatedBy
      `Prelude.hashWithSalt` updatedAt

instance Prelude.NFData Note where
  rnf Note' {..} =
    Prelude.rnf text
      `Prelude.seq` Prelude.rnf updatedBy
      `Prelude.seq` Prelude.rnf updatedAt

instance Data.ToJSON Note where
  toJSON Note' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("Text" Data..= text),
            Prelude.Just ("UpdatedBy" Data..= updatedBy),
            Prelude.Just ("UpdatedAt" Data..= updatedAt)
          ]
      )
