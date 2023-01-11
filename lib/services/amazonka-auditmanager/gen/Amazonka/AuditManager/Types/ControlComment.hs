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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.AuditManager.Types.ControlComment where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | A comment that\'s posted by a user on a control. This includes the
-- author\'s name, the comment text, and a timestamp.
--
-- /See:/ 'newControlComment' smart constructor.
data ControlComment = ControlComment'
  { -- | The name of the user who authored the comment.
    authorName :: Prelude.Maybe Prelude.Text,
    -- | The body text of a control comment.
    commentBody :: Prelude.Maybe Prelude.Text,
    -- | The time when the comment was posted.
    postedDate :: Prelude.Maybe Data.POSIX
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
-- 'commentBody', 'controlComment_commentBody' - The body text of a control comment.
--
-- 'postedDate', 'controlComment_postedDate' - The time when the comment was posted.
newControlComment ::
  ControlComment
newControlComment =
  ControlComment'
    { authorName = Prelude.Nothing,
      commentBody = Prelude.Nothing,
      postedDate = Prelude.Nothing
    }

-- | The name of the user who authored the comment.
controlComment_authorName :: Lens.Lens' ControlComment (Prelude.Maybe Prelude.Text)
controlComment_authorName = Lens.lens (\ControlComment' {authorName} -> authorName) (\s@ControlComment' {} a -> s {authorName = a} :: ControlComment)

-- | The body text of a control comment.
controlComment_commentBody :: Lens.Lens' ControlComment (Prelude.Maybe Prelude.Text)
controlComment_commentBody = Lens.lens (\ControlComment' {commentBody} -> commentBody) (\s@ControlComment' {} a -> s {commentBody = a} :: ControlComment)

-- | The time when the comment was posted.
controlComment_postedDate :: Lens.Lens' ControlComment (Prelude.Maybe Prelude.UTCTime)
controlComment_postedDate = Lens.lens (\ControlComment' {postedDate} -> postedDate) (\s@ControlComment' {} a -> s {postedDate = a} :: ControlComment) Prelude.. Lens.mapping Data._Time

instance Data.FromJSON ControlComment where
  parseJSON =
    Data.withObject
      "ControlComment"
      ( \x ->
          ControlComment'
            Prelude.<$> (x Data..:? "authorName")
            Prelude.<*> (x Data..:? "commentBody")
            Prelude.<*> (x Data..:? "postedDate")
      )

instance Prelude.Hashable ControlComment where
  hashWithSalt _salt ControlComment' {..} =
    _salt `Prelude.hashWithSalt` authorName
      `Prelude.hashWithSalt` commentBody
      `Prelude.hashWithSalt` postedDate

instance Prelude.NFData ControlComment where
  rnf ControlComment' {..} =
    Prelude.rnf authorName
      `Prelude.seq` Prelude.rnf commentBody
      `Prelude.seq` Prelude.rnf postedDate
