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
-- Module      : Network.AWS.Route53.Types.ChangeBatch
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Route53.Types.ChangeBatch where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Route53.Internal
import Network.AWS.Route53.Types.Change

-- | The information for a change request.
--
-- /See:/ 'newChangeBatch' smart constructor.
data ChangeBatch = ChangeBatch'
  { -- | /Optional:/ Any comments you want to include about a change batch
    -- request.
    comment :: Core.Maybe Core.Text,
    -- | Information about the changes to make to the record sets.
    changes :: Core.NonEmpty Change
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.NonEmpty Change ->
  ChangeBatch
newChangeBatch pChanges_ =
  ChangeBatch'
    { comment = Core.Nothing,
      changes = Lens._Coerce Lens.# pChanges_
    }

-- | /Optional:/ Any comments you want to include about a change batch
-- request.
changeBatch_comment :: Lens.Lens' ChangeBatch (Core.Maybe Core.Text)
changeBatch_comment = Lens.lens (\ChangeBatch' {comment} -> comment) (\s@ChangeBatch' {} a -> s {comment = a} :: ChangeBatch)

-- | Information about the changes to make to the record sets.
changeBatch_changes :: Lens.Lens' ChangeBatch (Core.NonEmpty Change)
changeBatch_changes = Lens.lens (\ChangeBatch' {changes} -> changes) (\s@ChangeBatch' {} a -> s {changes = a} :: ChangeBatch) Core.. Lens._Coerce

instance Core.Hashable ChangeBatch

instance Core.NFData ChangeBatch

instance Core.ToXML ChangeBatch where
  toXML ChangeBatch' {..} =
    Core.mconcat
      [ "Comment" Core.@= comment,
        "Changes" Core.@= Core.toXMLList "Change" changes
      ]
