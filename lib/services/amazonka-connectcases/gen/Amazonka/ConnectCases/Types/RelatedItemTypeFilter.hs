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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ConnectCases.Types.RelatedItemTypeFilter where

import Amazonka.ConnectCases.Types.CommentFilter
import Amazonka.ConnectCases.Types.ContactFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The list of types of related items and their parameters to use for
-- filtering.
--
-- /See:/ 'newRelatedItemTypeFilter' smart constructor.
data RelatedItemTypeFilter = RelatedItemTypeFilter'
  { -- | A filter for related items of type @Comment@.
    comment :: Prelude.Maybe CommentFilter,
    -- | A filter for related items of type @Contact@.
    contact :: Prelude.Maybe ContactFilter
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
-- 'comment', 'relatedItemTypeFilter_comment' - A filter for related items of type @Comment@.
--
-- 'contact', 'relatedItemTypeFilter_contact' - A filter for related items of type @Contact@.
newRelatedItemTypeFilter ::
  RelatedItemTypeFilter
newRelatedItemTypeFilter =
  RelatedItemTypeFilter'
    { comment = Prelude.Nothing,
      contact = Prelude.Nothing
    }

-- | A filter for related items of type @Comment@.
relatedItemTypeFilter_comment :: Lens.Lens' RelatedItemTypeFilter (Prelude.Maybe CommentFilter)
relatedItemTypeFilter_comment = Lens.lens (\RelatedItemTypeFilter' {comment} -> comment) (\s@RelatedItemTypeFilter' {} a -> s {comment = a} :: RelatedItemTypeFilter)

-- | A filter for related items of type @Contact@.
relatedItemTypeFilter_contact :: Lens.Lens' RelatedItemTypeFilter (Prelude.Maybe ContactFilter)
relatedItemTypeFilter_contact = Lens.lens (\RelatedItemTypeFilter' {contact} -> contact) (\s@RelatedItemTypeFilter' {} a -> s {contact = a} :: RelatedItemTypeFilter)

instance Prelude.Hashable RelatedItemTypeFilter where
  hashWithSalt _salt RelatedItemTypeFilter' {..} =
    _salt
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` contact

instance Prelude.NFData RelatedItemTypeFilter where
  rnf RelatedItemTypeFilter' {..} =
    Prelude.rnf comment
      `Prelude.seq` Prelude.rnf contact

instance Data.ToJSON RelatedItemTypeFilter where
  toJSON RelatedItemTypeFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("comment" Data..=) Prelude.<$> comment,
            ("contact" Data..=) Prelude.<$> contact
          ]
      )
