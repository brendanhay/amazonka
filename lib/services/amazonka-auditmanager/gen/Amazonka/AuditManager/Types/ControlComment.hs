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
-- Module      : Amazonka.AuditManager.Types.ControlComment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ControlComment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A comment posted by a user on a control. This includes the author\'s
-- name, the comment text, and a timestamp.
--
-- /See:/ 'newControlComment' smart constructor.
data ControlComment = ControlComment'
  { -- | The name of the user who authored the comment.
    authorName :: Prelude.Maybe Prelude.Text,
    -- | The time when the comment was posted.
    postedDate :: Prelude.Maybe Core.POSIX,
    -- | The body text of a control comment.
    commentBody :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ControlComment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'authorName', 'controlComment_authorName' - The name of the user who authored the comment.
--
-- 'postedDate', 'controlComment_postedDate' - The time when the comment was posted.
--
-- 'commentBody', 'controlComment_commentBody' - The body text of a control comment.
newControlComment ::
  ControlComment
newControlComment =
  ControlComment'
    { authorName = Prelude.Nothing,
      postedDate = Prelude.Nothing,
      commentBody = Prelude.Nothing
    }

-- | The name of the user who authored the comment.
controlComment_authorName :: Lens.Lens' ControlComment (Prelude.Maybe Prelude.Text)
controlComment_authorName = Lens.lens (\ControlComment' {authorName} -> authorName) (\s@ControlComment' {} a -> s {authorName = a} :: ControlComment)

-- | The time when the comment was posted.
controlComment_postedDate :: Lens.Lens' ControlComment (Prelude.Maybe Prelude.UTCTime)
controlComment_postedDate = Lens.lens (\ControlComment' {postedDate} -> postedDate) (\s@ControlComment' {} a -> s {postedDate = a} :: ControlComment) Prelude.. Lens.mapping Core._Time

-- | The body text of a control comment.
controlComment_commentBody :: Lens.Lens' ControlComment (Prelude.Maybe Prelude.Text)
controlComment_commentBody = Lens.lens (\ControlComment' {commentBody} -> commentBody) (\s@ControlComment' {} a -> s {commentBody = a} :: ControlComment)

instance Core.FromJSON ControlComment where
  parseJSON =
    Core.withObject
      "ControlComment"
      ( \x ->
          ControlComment'
            Prelude.<$> (x Core..:? "authorName")
            Prelude.<*> (x Core..:? "postedDate")
            Prelude.<*> (x Core..:? "commentBody")
      )

instance Prelude.Hashable ControlComment where
  hashWithSalt _salt ControlComment' {..} =
    _salt `Prelude.hashWithSalt` authorName
      `Prelude.hashWithSalt` postedDate
      `Prelude.hashWithSalt` commentBody

instance Prelude.NFData ControlComment where
  rnf ControlComment' {..} =
    Prelude.rnf authorName
      `Prelude.seq` Prelude.rnf postedDate
      `Prelude.seq` Prelude.rnf commentBody
