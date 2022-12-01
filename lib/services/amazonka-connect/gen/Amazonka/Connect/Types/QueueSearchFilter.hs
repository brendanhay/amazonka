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
-- Module      : Amazonka.Connect.Types.QueueSearchFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.QueueSearchFilter where

import Amazonka.Connect.Types.ControlPlaneTagFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | Filters to be applied to search results.
--
-- /See:/ 'newQueueSearchFilter' smart constructor.
data QueueSearchFilter = QueueSearchFilter'
  { tagFilter :: Prelude.Maybe ControlPlaneTagFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'QueueSearchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tagFilter', 'queueSearchFilter_tagFilter' - Undocumented member.
newQueueSearchFilter ::
  QueueSearchFilter
newQueueSearchFilter =
  QueueSearchFilter' {tagFilter = Prelude.Nothing}

-- | Undocumented member.
queueSearchFilter_tagFilter :: Lens.Lens' QueueSearchFilter (Prelude.Maybe ControlPlaneTagFilter)
queueSearchFilter_tagFilter = Lens.lens (\QueueSearchFilter' {tagFilter} -> tagFilter) (\s@QueueSearchFilter' {} a -> s {tagFilter = a} :: QueueSearchFilter)

instance Prelude.Hashable QueueSearchFilter where
  hashWithSalt _salt QueueSearchFilter' {..} =
    _salt `Prelude.hashWithSalt` tagFilter

instance Prelude.NFData QueueSearchFilter where
  rnf QueueSearchFilter' {..} = Prelude.rnf tagFilter

instance Core.ToJSON QueueSearchFilter where
  toJSON QueueSearchFilter' {..} =
    Core.object
      ( Prelude.catMaybes
          [("TagFilter" Core..=) Prelude.<$> tagFilter]
      )
