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
-- Module      : Network.AWS.ServiceCatalog.Types.ListRecordHistorySearchFilter
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.ListRecordHistorySearchFilter where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The search filter to use when listing history records.
--
-- /See:/ 'newListRecordHistorySearchFilter' smart constructor.
data ListRecordHistorySearchFilter = ListRecordHistorySearchFilter'
  { -- | The filter key.
    --
    -- -   @product@ - Filter results based on the specified product
    --     identifier.
    --
    -- -   @provisionedproduct@ - Filter results based on the provisioned
    --     product identifier.
    key :: Prelude.Maybe Prelude.Text,
    -- | The filter value.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ListRecordHistorySearchFilter' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'listRecordHistorySearchFilter_key' - The filter key.
--
-- -   @product@ - Filter results based on the specified product
--     identifier.
--
-- -   @provisionedproduct@ - Filter results based on the provisioned
--     product identifier.
--
-- 'value', 'listRecordHistorySearchFilter_value' - The filter value.
newListRecordHistorySearchFilter ::
  ListRecordHistorySearchFilter
newListRecordHistorySearchFilter =
  ListRecordHistorySearchFilter'
    { key =
        Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The filter key.
--
-- -   @product@ - Filter results based on the specified product
--     identifier.
--
-- -   @provisionedproduct@ - Filter results based on the provisioned
--     product identifier.
listRecordHistorySearchFilter_key :: Lens.Lens' ListRecordHistorySearchFilter (Prelude.Maybe Prelude.Text)
listRecordHistorySearchFilter_key = Lens.lens (\ListRecordHistorySearchFilter' {key} -> key) (\s@ListRecordHistorySearchFilter' {} a -> s {key = a} :: ListRecordHistorySearchFilter)

-- | The filter value.
listRecordHistorySearchFilter_value :: Lens.Lens' ListRecordHistorySearchFilter (Prelude.Maybe Prelude.Text)
listRecordHistorySearchFilter_value = Lens.lens (\ListRecordHistorySearchFilter' {value} -> value) (\s@ListRecordHistorySearchFilter' {} a -> s {value = a} :: ListRecordHistorySearchFilter)

instance
  Prelude.Hashable
    ListRecordHistorySearchFilter

instance Prelude.NFData ListRecordHistorySearchFilter

instance Prelude.ToJSON ListRecordHistorySearchFilter where
  toJSON ListRecordHistorySearchFilter' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("Key" Prelude..=) Prelude.<$> key,
            ("Value" Prelude..=) Prelude.<$> value
          ]
      )
