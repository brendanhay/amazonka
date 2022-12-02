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
-- Module      : Amazonka.Inspector.Types.ResourceGroupTag
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Inspector.Types.ResourceGroupTag where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | This data type is used as one of the elements of the ResourceGroup data
-- type.
--
-- /See:/ 'newResourceGroupTag' smart constructor.
data ResourceGroupTag = ResourceGroupTag'
  { -- | The value assigned to a tag key.
    value :: Prelude.Maybe Prelude.Text,
    -- | A tag key.
    key :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceGroupTag' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'resourceGroupTag_value' - The value assigned to a tag key.
--
-- 'key', 'resourceGroupTag_key' - A tag key.
newResourceGroupTag ::
  -- | 'key'
  Prelude.Text ->
  ResourceGroupTag
newResourceGroupTag pKey_ =
  ResourceGroupTag'
    { value = Prelude.Nothing,
      key = pKey_
    }

-- | The value assigned to a tag key.
resourceGroupTag_value :: Lens.Lens' ResourceGroupTag (Prelude.Maybe Prelude.Text)
resourceGroupTag_value = Lens.lens (\ResourceGroupTag' {value} -> value) (\s@ResourceGroupTag' {} a -> s {value = a} :: ResourceGroupTag)

-- | A tag key.
resourceGroupTag_key :: Lens.Lens' ResourceGroupTag Prelude.Text
resourceGroupTag_key = Lens.lens (\ResourceGroupTag' {key} -> key) (\s@ResourceGroupTag' {} a -> s {key = a} :: ResourceGroupTag)

instance Data.FromJSON ResourceGroupTag where
  parseJSON =
    Data.withObject
      "ResourceGroupTag"
      ( \x ->
          ResourceGroupTag'
            Prelude.<$> (x Data..:? "value") Prelude.<*> (x Data..: "key")
      )

instance Prelude.Hashable ResourceGroupTag where
  hashWithSalt _salt ResourceGroupTag' {..} =
    _salt `Prelude.hashWithSalt` value
      `Prelude.hashWithSalt` key

instance Prelude.NFData ResourceGroupTag where
  rnf ResourceGroupTag' {..} =
    Prelude.rnf value `Prelude.seq` Prelude.rnf key

instance Data.ToJSON ResourceGroupTag where
  toJSON ResourceGroupTag' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("value" Data..=) Prelude.<$> value,
            Prelude.Just ("key" Data..= key)
          ]
      )
