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
-- Module      : Amazonka.ConnectCases.Types.RelatedItemInputContent
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.RelatedItemInputContent where

import Amazonka.ConnectCases.Types.CommentContent
import Amazonka.ConnectCases.Types.Contact
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents the content of a related item to be created.
--
-- /See:/ 'newRelatedItemInputContent' smart constructor.
data RelatedItemInputContent = RelatedItemInputContent'
  { -- | Represents the content of a comment to be returned to agents.
    comment :: Prelude.Maybe CommentContent,
    -- | Object representing a contact in Amazon Connect as an API request field.
    contact :: Prelude.Maybe Contact
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelatedItemInputContent' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'relatedItemInputContent_comment' - Represents the content of a comment to be returned to agents.
--
-- 'contact', 'relatedItemInputContent_contact' - Object representing a contact in Amazon Connect as an API request field.
newRelatedItemInputContent ::
  RelatedItemInputContent
newRelatedItemInputContent =
  RelatedItemInputContent'
    { comment = Prelude.Nothing,
      contact = Prelude.Nothing
    }

-- | Represents the content of a comment to be returned to agents.
relatedItemInputContent_comment :: Lens.Lens' RelatedItemInputContent (Prelude.Maybe CommentContent)
relatedItemInputContent_comment = Lens.lens (\RelatedItemInputContent' {comment} -> comment) (\s@RelatedItemInputContent' {} a -> s {comment = a} :: RelatedItemInputContent)

-- | Object representing a contact in Amazon Connect as an API request field.
relatedItemInputContent_contact :: Lens.Lens' RelatedItemInputContent (Prelude.Maybe Contact)
relatedItemInputContent_contact = Lens.lens (\RelatedItemInputContent' {contact} -> contact) (\s@RelatedItemInputContent' {} a -> s {contact = a} :: RelatedItemInputContent)

instance Prelude.Hashable RelatedItemInputContent where
  hashWithSalt _salt RelatedItemInputContent' {..} =
    _salt
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` contact

instance Prelude.NFData RelatedItemInputContent where
  rnf RelatedItemInputContent' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf contact

instance Data.ToJSON RelatedItemInputContent where
  toJSON RelatedItemInputContent' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("comment" Data..=) Prelude.<$> comment,
            ("contact" Data..=) Prelude.<$> contact
          ]
      )
