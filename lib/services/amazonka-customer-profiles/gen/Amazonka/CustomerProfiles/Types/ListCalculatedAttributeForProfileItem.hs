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
-- Module      : Amazonka.CustomerProfiles.Types.ListCalculatedAttributeForProfileItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.ListCalculatedAttributeForProfileItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of a single calculated attribute for a profile.
--
-- /See:/ 'newListCalculatedAttributeForProfileItem' smart constructor.
data ListCalculatedAttributeForProfileItem = ListCalculatedAttributeForProfileItem'
  { -- | The unique name of the calculated attribute.
    calculatedAttributeName :: Prelude.Maybe Prelude.Text,
    -- | The display name of the calculated attribute.
    displayName :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the calculated attribute’s value is based on partial
    -- data. If data is partial, it is set to true.
    isDataPartial :: Prelude.Maybe Prelude.Text,
    -- | The value of the calculated attribute.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCalculatedAttributeForProfileItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'calculatedAttributeName', 'listCalculatedAttributeForProfileItem_calculatedAttributeName' - The unique name of the calculated attribute.
--
-- 'displayName', 'listCalculatedAttributeForProfileItem_displayName' - The display name of the calculated attribute.
--
-- 'isDataPartial', 'listCalculatedAttributeForProfileItem_isDataPartial' - Indicates whether the calculated attribute’s value is based on partial
-- data. If data is partial, it is set to true.
--
-- 'value', 'listCalculatedAttributeForProfileItem_value' - The value of the calculated attribute.
newListCalculatedAttributeForProfileItem ::
  ListCalculatedAttributeForProfileItem
newListCalculatedAttributeForProfileItem =
  ListCalculatedAttributeForProfileItem'
    { calculatedAttributeName =
        Prelude.Nothing,
      displayName = Prelude.Nothing,
      isDataPartial = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The unique name of the calculated attribute.
listCalculatedAttributeForProfileItem_calculatedAttributeName :: Lens.Lens' ListCalculatedAttributeForProfileItem (Prelude.Maybe Prelude.Text)
listCalculatedAttributeForProfileItem_calculatedAttributeName = Lens.lens (\ListCalculatedAttributeForProfileItem' {calculatedAttributeName} -> calculatedAttributeName) (\s@ListCalculatedAttributeForProfileItem' {} a -> s {calculatedAttributeName = a} :: ListCalculatedAttributeForProfileItem)

-- | The display name of the calculated attribute.
listCalculatedAttributeForProfileItem_displayName :: Lens.Lens' ListCalculatedAttributeForProfileItem (Prelude.Maybe Prelude.Text)
listCalculatedAttributeForProfileItem_displayName = Lens.lens (\ListCalculatedAttributeForProfileItem' {displayName} -> displayName) (\s@ListCalculatedAttributeForProfileItem' {} a -> s {displayName = a} :: ListCalculatedAttributeForProfileItem)

-- | Indicates whether the calculated attribute’s value is based on partial
-- data. If data is partial, it is set to true.
listCalculatedAttributeForProfileItem_isDataPartial :: Lens.Lens' ListCalculatedAttributeForProfileItem (Prelude.Maybe Prelude.Text)
listCalculatedAttributeForProfileItem_isDataPartial = Lens.lens (\ListCalculatedAttributeForProfileItem' {isDataPartial} -> isDataPartial) (\s@ListCalculatedAttributeForProfileItem' {} a -> s {isDataPartial = a} :: ListCalculatedAttributeForProfileItem)

-- | The value of the calculated attribute.
listCalculatedAttributeForProfileItem_value :: Lens.Lens' ListCalculatedAttributeForProfileItem (Prelude.Maybe Prelude.Text)
listCalculatedAttributeForProfileItem_value = Lens.lens (\ListCalculatedAttributeForProfileItem' {value} -> value) (\s@ListCalculatedAttributeForProfileItem' {} a -> s {value = a} :: ListCalculatedAttributeForProfileItem)

instance
  Data.FromJSON
    ListCalculatedAttributeForProfileItem
  where
  parseJSON =
    Data.withObject
      "ListCalculatedAttributeForProfileItem"
      ( \x ->
          ListCalculatedAttributeForProfileItem'
            Prelude.<$> (x Data..:? "CalculatedAttributeName")
            Prelude.<*> (x Data..:? "DisplayName")
            Prelude.<*> (x Data..:? "IsDataPartial")
            Prelude.<*> (x Data..:? "Value")
      )

instance
  Prelude.Hashable
    ListCalculatedAttributeForProfileItem
  where
  hashWithSalt
    _salt
    ListCalculatedAttributeForProfileItem' {..} =
      _salt
        `Prelude.hashWithSalt` calculatedAttributeName
        `Prelude.hashWithSalt` displayName
        `Prelude.hashWithSalt` isDataPartial
        `Prelude.hashWithSalt` value

instance
  Prelude.NFData
    ListCalculatedAttributeForProfileItem
  where
  rnf ListCalculatedAttributeForProfileItem' {..} =
    Prelude.rnf calculatedAttributeName
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf isDataPartial
      `Prelude.seq` Prelude.rnf value
