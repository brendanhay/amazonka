{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
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
    key :: Prelude.Maybe AccessLevelFilterKey,
    -- | The user to which the access level applies. The only supported value is
    -- @Self@.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The access level.
--
-- -   @Account@ - Filter results based on the account.
--
-- -   @Role@ - Filter results based on the federated role of the specified
--     user.
--
-- -   @User@ - Filter results based on the specified user.
accessLevelFilter_key :: Lens.Lens' AccessLevelFilter (Prelude.Maybe AccessLevelFilterKey)
accessLevelFilter_key = Lens.lens (\AccessLevelFilter' {key} -> key) (\s@AccessLevelFilter' {} a -> s {key = a} :: AccessLevelFilter)

-- | The user to which the access level applies. The only supported value is
-- @Self@.
accessLevelFilter_value :: Lens.Lens' AccessLevelFilter (Prelude.Maybe Prelude.Text)
accessLevelFilter_value = Lens.lens (\AccessLevelFilter' {value} -> value) (\s@AccessLevelFilter' {} a -> s {value = a} :: AccessLevelFilter)

instance Prelude.Hashable AccessLevelFilter

instance Prelude.NFData AccessLevelFilter

instance Prelude.ToJSON AccessLevelFilter where
  toJSON AccessLevelFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Key" Prelude..=) Prelude.<$> key,
            ("Value" Prelude..=) Prelude.<$> value
          ]
      )
