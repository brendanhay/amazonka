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
-- Module      : Amazonka.ConnectCases.Types.CommentContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.CommentContent where

import Amazonka.ConnectCases.Types.CommentBodyTextType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the content of a @Comment@ to be returned to agents.
--
-- /See:/ 'newCommentContent' smart constructor.
data CommentContent = CommentContent'
  { -- | Text in the body of a @Comment@ on a case.
    body :: Prelude.Text,
    -- | Type of the text in the box of a @Comment@ on a case.
    contentType :: CommentBodyTextType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CommentContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'body', 'commentContent_body' - Text in the body of a @Comment@ on a case.
--
-- 'contentType', 'commentContent_contentType' - Type of the text in the box of a @Comment@ on a case.
newCommentContent ::
  -- | 'body'
  Prelude.Text ->
  -- | 'contentType'
  CommentBodyTextType ->
  CommentContent
newCommentContent pBody_ pContentType_ =
  CommentContent'
    { body = pBody_,
      contentType = pContentType_
    }

-- | Text in the body of a @Comment@ on a case.
commentContent_body :: Lens.Lens' CommentContent Prelude.Text
commentContent_body = Lens.lens (\CommentContent' {body} -> body) (\s@CommentContent' {} a -> s {body = a} :: CommentContent)

-- | Type of the text in the box of a @Comment@ on a case.
commentContent_contentType :: Lens.Lens' CommentContent CommentBodyTextType
commentContent_contentType = Lens.lens (\CommentContent' {contentType} -> contentType) (\s@CommentContent' {} a -> s {contentType = a} :: CommentContent)

instance Data.FromJSON CommentContent where
  parseJSON =
    Data.withObject
      "CommentContent"
      ( \x ->
          CommentContent'
            Prelude.<$> (x Data..: "body")
            Prelude.<*> (x Data..: "contentType")
      )

instance Prelude.Hashable CommentContent where
  hashWithSalt _salt CommentContent' {..} =
    _salt
      `Prelude.hashWithSalt` body
      `Prelude.hashWithSalt` contentType

instance Prelude.NFData CommentContent where
  rnf CommentContent' {..} =
    Prelude.rnf body
      `Prelude.seq` Prelude.rnf contentType

instance Data.ToJSON CommentContent where
  toJSON CommentContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("body" Data..= body),
            Prelude.Just ("contentType" Data..= contentType)
          ]
      )
