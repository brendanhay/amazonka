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
-- Module      : Amazonka.ConnectCases.Types.RelatedItemTypeFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.RelatedItemTypeFilter where

import Amazonka.ConnectCases.Types.CommentFilter
import Amazonka.ConnectCases.Types.ContactFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | The list of types of related items and their parameters to use for
-- filtering.
--
-- /See:/ 'newRelatedItemTypeFilter' smart constructor.
data RelatedItemTypeFilter = RelatedItemTypeFilter'
  { -- | A filter for related items of type @Contact@.
    contact :: Prelude.Maybe ContactFilter,
    -- | A filter for related items of type @Comment@.
    comment :: Prelude.Maybe CommentFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RelatedItemTypeFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contact', 'relatedItemTypeFilter_contact' - A filter for related items of type @Contact@.
--
-- 'comment', 'relatedItemTypeFilter_comment' - A filter for related items of type @Comment@.
newRelatedItemTypeFilter ::
  RelatedItemTypeFilter
newRelatedItemTypeFilter =
  RelatedItemTypeFilter'
    { contact = Prelude.Nothing,
      comment = Prelude.Nothing
    }

-- | A filter for related items of type @Contact@.
relatedItemTypeFilter_contact :: Lens.Lens' RelatedItemTypeFilter (Prelude.Maybe ContactFilter)
relatedItemTypeFilter_contact = Lens.lens (\RelatedItemTypeFilter' {contact} -> contact) (\s@RelatedItemTypeFilter' {} a -> s {contact = a} :: RelatedItemTypeFilter)

-- | A filter for related items of type @Comment@.
relatedItemTypeFilter_comment :: Lens.Lens' RelatedItemTypeFilter (Prelude.Maybe CommentFilter)
relatedItemTypeFilter_comment = Lens.lens (\RelatedItemTypeFilter' {comment} -> comment) (\s@RelatedItemTypeFilter' {} a -> s {comment = a} :: RelatedItemTypeFilter)

instance Prelude.Hashable RelatedItemTypeFilter where
  hashWithSalt _salt RelatedItemTypeFilter' {..} =
    _salt `Prelude.hashWithSalt` contact
      `Prelude.hashWithSalt` comment

instance Prelude.NFData RelatedItemTypeFilter where
  rnf RelatedItemTypeFilter' {..} =
    Prelude.rnf contact
      `Prelude.seq` Prelude.rnf comment

instance Core.ToJSON RelatedItemTypeFilter where
  toJSON RelatedItemTypeFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("contact" Core..=) Prelude.<$> contact,
            ("comment" Core..=) Prelude.<$> comment
          ]
      )
