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
-- Module      : Amazonka.Connect.Types.UserDataFilters
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Connect.Types.UserDataFilters where

import Amazonka.Connect.Types.ContactFilter
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude

-- | A filter for the user data.
--
-- /See:/ 'newUserDataFilters' smart constructor.
data UserDataFilters = UserDataFilters'
  { -- | A filter for the user data based on the contact information that is
    -- associated to the user. It contains a list of contact states.
    contactFilter :: Prelude.Maybe ContactFilter,
    -- | Contains information about a queue resource for which metrics are
    -- returned.
    queues :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text)
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserDataFilters' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contactFilter', 'userDataFilters_contactFilter' - A filter for the user data based on the contact information that is
-- associated to the user. It contains a list of contact states.
--
-- 'queues', 'userDataFilters_queues' - Contains information about a queue resource for which metrics are
-- returned.
newUserDataFilters ::
  UserDataFilters
newUserDataFilters =
  UserDataFilters'
    { contactFilter = Prelude.Nothing,
      queues = Prelude.Nothing
    }

-- | A filter for the user data based on the contact information that is
-- associated to the user. It contains a list of contact states.
userDataFilters_contactFilter :: Lens.Lens' UserDataFilters (Prelude.Maybe ContactFilter)
userDataFilters_contactFilter = Lens.lens (\UserDataFilters' {contactFilter} -> contactFilter) (\s@UserDataFilters' {} a -> s {contactFilter = a} :: UserDataFilters)

-- | Contains information about a queue resource for which metrics are
-- returned.
userDataFilters_queues :: Lens.Lens' UserDataFilters (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
userDataFilters_queues = Lens.lens (\UserDataFilters' {queues} -> queues) (\s@UserDataFilters' {} a -> s {queues = a} :: UserDataFilters) Prelude.. Lens.mapping Lens.coerced

instance Prelude.Hashable UserDataFilters where
  hashWithSalt _salt UserDataFilters' {..} =
    _salt `Prelude.hashWithSalt` contactFilter
      `Prelude.hashWithSalt` queues

instance Prelude.NFData UserDataFilters where
  rnf UserDataFilters' {..} =
    Prelude.rnf contactFilter
      `Prelude.seq` Prelude.rnf queues

instance Core.ToJSON UserDataFilters where
  toJSON UserDataFilters' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ContactFilter" Core..=) Prelude.<$> contactFilter,
            ("Queues" Core..=) Prelude.<$> queues
          ]
      )
