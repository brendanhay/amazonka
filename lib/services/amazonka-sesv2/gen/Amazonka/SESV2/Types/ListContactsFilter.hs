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
-- Module      : Amazonka.SESV2.Types.ListContactsFilter
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SESV2.Types.ListContactsFilter where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SESV2.Types.SubscriptionStatus
import Amazonka.SESV2.Types.TopicFilter

-- | A filter that can be applied to a list of contacts.
--
-- /See:/ 'newListContactsFilter' smart constructor.
data ListContactsFilter = ListContactsFilter'
  { -- | The status by which you are filtering: @OPT_IN@ or @OPT_OUT@.
    filteredStatus :: Prelude.Maybe SubscriptionStatus,
    -- | Used for filtering by a specific topic preference.
    topicFilter :: Prelude.Maybe TopicFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListContactsFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filteredStatus', 'listContactsFilter_filteredStatus' - The status by which you are filtering: @OPT_IN@ or @OPT_OUT@.
--
-- 'topicFilter', 'listContactsFilter_topicFilter' - Used for filtering by a specific topic preference.
newListContactsFilter ::
  ListContactsFilter
newListContactsFilter =
  ListContactsFilter'
    { filteredStatus =
        Prelude.Nothing,
      topicFilter = Prelude.Nothing
    }

-- | The status by which you are filtering: @OPT_IN@ or @OPT_OUT@.
listContactsFilter_filteredStatus :: Lens.Lens' ListContactsFilter (Prelude.Maybe SubscriptionStatus)
listContactsFilter_filteredStatus = Lens.lens (\ListContactsFilter' {filteredStatus} -> filteredStatus) (\s@ListContactsFilter' {} a -> s {filteredStatus = a} :: ListContactsFilter)

-- | Used for filtering by a specific topic preference.
listContactsFilter_topicFilter :: Lens.Lens' ListContactsFilter (Prelude.Maybe TopicFilter)
listContactsFilter_topicFilter = Lens.lens (\ListContactsFilter' {topicFilter} -> topicFilter) (\s@ListContactsFilter' {} a -> s {topicFilter = a} :: ListContactsFilter)

instance Prelude.Hashable ListContactsFilter where
  hashWithSalt _salt ListContactsFilter' {..} =
    _salt `Prelude.hashWithSalt` filteredStatus
      `Prelude.hashWithSalt` topicFilter

instance Prelude.NFData ListContactsFilter where
  rnf ListContactsFilter' {..} =
    Prelude.rnf filteredStatus
      `Prelude.seq` Prelude.rnf topicFilter

instance Data.ToJSON ListContactsFilter where
  toJSON ListContactsFilter' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("FilteredStatus" Data..=)
              Prelude.<$> filteredStatus,
            ("TopicFilter" Data..=) Prelude.<$> topicFilter
          ]
      )
