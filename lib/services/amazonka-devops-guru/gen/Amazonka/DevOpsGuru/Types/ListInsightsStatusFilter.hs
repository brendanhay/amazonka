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
-- Module      : Amazonka.DevOpsGuru.Types.ListInsightsStatusFilter
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.DevOpsGuru.Types.ListInsightsStatusFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.DevOpsGuru.Types.ListInsightsAnyStatusFilter
import Amazonka.DevOpsGuru.Types.ListInsightsClosedStatusFilter
import Amazonka.DevOpsGuru.Types.ListInsightsOngoingStatusFilter
import qualified Amazonka.Prelude as Prelude

-- | A filter used by @ListInsights@ to specify which insights to return.
--
-- /See:/ 'newListInsightsStatusFilter' smart constructor.
data ListInsightsStatusFilter = ListInsightsStatusFilter'
  { -- | A @ListInsightsAnyStatusFilter@ that specifies insights of any status
    -- that are either @REACTIVE@ or @PROACTIVE@.
    any :: Prelude.Maybe ListInsightsAnyStatusFilter,
    -- | A @ListInsightsClosedStatusFilter@ that specifies closed insights that
    -- are either @REACTIVE@ or @PROACTIVE@.
    closed :: Prelude.Maybe ListInsightsClosedStatusFilter,
    -- | A @ListInsightsAnyStatusFilter@ that specifies ongoing insights that are
    -- either @REACTIVE@ or @PROACTIVE@.
    ongoing :: Prelude.Maybe ListInsightsOngoingStatusFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInsightsStatusFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'any', 'listInsightsStatusFilter_any' - A @ListInsightsAnyStatusFilter@ that specifies insights of any status
-- that are either @REACTIVE@ or @PROACTIVE@.
--
-- 'closed', 'listInsightsStatusFilter_closed' - A @ListInsightsClosedStatusFilter@ that specifies closed insights that
-- are either @REACTIVE@ or @PROACTIVE@.
--
-- 'ongoing', 'listInsightsStatusFilter_ongoing' - A @ListInsightsAnyStatusFilter@ that specifies ongoing insights that are
-- either @REACTIVE@ or @PROACTIVE@.
newListInsightsStatusFilter ::
  ListInsightsStatusFilter
newListInsightsStatusFilter =
  ListInsightsStatusFilter'
    { any = Prelude.Nothing,
      closed = Prelude.Nothing,
      ongoing = Prelude.Nothing
    }

-- | A @ListInsightsAnyStatusFilter@ that specifies insights of any status
-- that are either @REACTIVE@ or @PROACTIVE@.
listInsightsStatusFilter_any :: Lens.Lens' ListInsightsStatusFilter (Prelude.Maybe ListInsightsAnyStatusFilter)
listInsightsStatusFilter_any = Lens.lens (\ListInsightsStatusFilter' {any} -> any) (\s@ListInsightsStatusFilter' {} a -> s {any = a} :: ListInsightsStatusFilter)

-- | A @ListInsightsClosedStatusFilter@ that specifies closed insights that
-- are either @REACTIVE@ or @PROACTIVE@.
listInsightsStatusFilter_closed :: Lens.Lens' ListInsightsStatusFilter (Prelude.Maybe ListInsightsClosedStatusFilter)
listInsightsStatusFilter_closed = Lens.lens (\ListInsightsStatusFilter' {closed} -> closed) (\s@ListInsightsStatusFilter' {} a -> s {closed = a} :: ListInsightsStatusFilter)

-- | A @ListInsightsAnyStatusFilter@ that specifies ongoing insights that are
-- either @REACTIVE@ or @PROACTIVE@.
listInsightsStatusFilter_ongoing :: Lens.Lens' ListInsightsStatusFilter (Prelude.Maybe ListInsightsOngoingStatusFilter)
listInsightsStatusFilter_ongoing = Lens.lens (\ListInsightsStatusFilter' {ongoing} -> ongoing) (\s@ListInsightsStatusFilter' {} a -> s {ongoing = a} :: ListInsightsStatusFilter)

instance Prelude.Hashable ListInsightsStatusFilter where
  hashWithSalt _salt ListInsightsStatusFilter' {..} =
    _salt
      `Prelude.hashWithSalt` any
      `Prelude.hashWithSalt` closed
      `Prelude.hashWithSalt` ongoing

instance Prelude.NFData ListInsightsStatusFilter where
  rnf ListInsightsStatusFilter' {..} =
    Prelude.rnf any
      `Prelude.seq` Prelude.rnf closed
      `Prelude.seq` Prelude.rnf ongoing

instance Data.ToJSON ListInsightsStatusFilter where
  toJSON ListInsightsStatusFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Any" Data..=) Prelude.<$> any,
            ("Closed" Data..=) Prelude.<$> closed,
            ("Ongoing" Data..=) Prelude.<$> ongoing
          ]
      )
