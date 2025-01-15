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
-- Module      : Amazonka.Route53.Types.ChangeBatch
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Route53.Types.ChangeBatch where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Route53.Internal
import Amazonka.Route53.Types.Change

-- | The information for a change request.
--
-- /See:/ 'newChangeBatch' smart constructor.
data ChangeBatch = ChangeBatch'
  { -- | /Optional:/ Any comments you want to include about a change batch
    -- request.
    comment :: Prelude.Maybe Prelude.Text,
    -- | Information about the changes to make to the record sets.
    changes :: Prelude.NonEmpty Change
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangeBatch' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'comment', 'changeBatch_comment' - /Optional:/ Any comments you want to include about a change batch
-- request.
--
-- 'changes', 'changeBatch_changes' - Information about the changes to make to the record sets.
newChangeBatch ::
  -- | 'changes'
  Prelude.NonEmpty Change ->
  ChangeBatch
newChangeBatch pChanges_ =
  ChangeBatch'
    { comment = Prelude.Nothing,
      changes = Lens.coerced Lens.# pChanges_
    }

-- | /Optional:/ Any comments you want to include about a change batch
-- request.
changeBatch_comment :: Lens.Lens' ChangeBatch (Prelude.Maybe Prelude.Text)
changeBatch_comment = Lens.lens (\ChangeBatch' {comment} -> comment) (\s@ChangeBatch' {} a -> s {comment = a} :: ChangeBatch)

-- | Information about the changes to make to the record sets.
changeBatch_changes :: Lens.Lens' ChangeBatch (Prelude.NonEmpty Change)
changeBatch_changes = Lens.lens (\ChangeBatch' {changes} -> changes) (\s@ChangeBatch' {} a -> s {changes = a} :: ChangeBatch) Prelude.. Lens.coerced

instance Prelude.Hashable ChangeBatch where
  hashWithSalt _salt ChangeBatch' {..} =
    _salt
      `Prelude.hashWithSalt` comment
      `Prelude.hashWithSalt` changes

instance Prelude.NFData ChangeBatch where
  rnf ChangeBatch' {..} =
    Prelude.rnf comment `Prelude.seq`
      Prelude.rnf changes

instance Data.ToXML ChangeBatch where
  toXML ChangeBatch' {..} =
    Prelude.mconcat
      [ "Comment" Data.@= comment,
        "Changes" Data.@= Data.toXMLList "Change" changes
      ]
