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
-- Module      : Network.AWS.ServiceCatalog.Types.AccessLevelFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.AccessLevelFilter where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.ServiceCatalog.Types.AccessLevelFilterKey

-- | The access level to use to filter results.
--
-- /See:/ 'newAccessLevelFilter' smart constructor.
data AccessLevelFilter = AccessLevelFilter'
  { -- | The access level.
    --
    -- -   @Account@ - Filter results based on the account.
    --
    -- -   @Role@ - Filter results based on the federated role of the specified
    --     user.
    --
    -- -   @User@ - Filter results based on the specified user.
    key :: Core.Maybe AccessLevelFilterKey,
    -- | The user to which the access level applies. The only supported value is
    -- @Self@.
    value :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AccessLevelFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'accessLevelFilter_key' - The access level.
--
-- -   @Account@ - Filter results based on the account.
--
-- -   @Role@ - Filter results based on the federated role of the specified
--     user.
--
-- -   @User@ - Filter results based on the specified user.
--
-- 'value', 'accessLevelFilter_value' - The user to which the access level applies. The only supported value is
-- @Self@.
newAccessLevelFilter ::
  AccessLevelFilter
newAccessLevelFilter =
  AccessLevelFilter'
    { key = Core.Nothing,
      value = Core.Nothing
    }

-- | The access level.
--
-- -   @Account@ - Filter results based on the account.
--
-- -   @Role@ - Filter results based on the federated role of the specified
--     user.
--
-- -   @User@ - Filter results based on the specified user.
accessLevelFilter_key :: Lens.Lens' AccessLevelFilter (Core.Maybe AccessLevelFilterKey)
accessLevelFilter_key = Lens.lens (\AccessLevelFilter' {key} -> key) (\s@AccessLevelFilter' {} a -> s {key = a} :: AccessLevelFilter)

-- | The user to which the access level applies. The only supported value is
-- @Self@.
accessLevelFilter_value :: Lens.Lens' AccessLevelFilter (Core.Maybe Core.Text)
accessLevelFilter_value = Lens.lens (\AccessLevelFilter' {value} -> value) (\s@AccessLevelFilter' {} a -> s {value = a} :: AccessLevelFilter)

instance Core.Hashable AccessLevelFilter

instance Core.NFData AccessLevelFilter

instance Core.ToJSON AccessLevelFilter where
  toJSON AccessLevelFilter' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Key" Core..=) Core.<$> key,
            ("Value" Core..=) Core.<$> value
          ]
      )
