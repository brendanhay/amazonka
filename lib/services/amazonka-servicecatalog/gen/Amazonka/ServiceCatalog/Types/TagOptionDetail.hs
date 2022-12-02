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
-- Module      : Amazonka.ServiceCatalog.Types.TagOptionDetail
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.TagOptionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Information about a TagOption.
--
-- /See:/ 'newTagOptionDetail' smart constructor.
data TagOptionDetail = TagOptionDetail'
  { -- | The TagOption key.
    key :: Prelude.Maybe Prelude.Text,
    -- | The TagOption active state.
    active :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Web Services account Id of the owner account that created the
    -- TagOption.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The TagOption identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The TagOption value.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TagOptionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'tagOptionDetail_key' - The TagOption key.
--
-- 'active', 'tagOptionDetail_active' - The TagOption active state.
--
-- 'owner', 'tagOptionDetail_owner' - The Amazon Web Services account Id of the owner account that created the
-- TagOption.
--
-- 'id', 'tagOptionDetail_id' - The TagOption identifier.
--
-- 'value', 'tagOptionDetail_value' - The TagOption value.
newTagOptionDetail ::
  TagOptionDetail
newTagOptionDetail =
  TagOptionDetail'
    { key = Prelude.Nothing,
      active = Prelude.Nothing,
      owner = Prelude.Nothing,
      id = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The TagOption key.
tagOptionDetail_key :: Lens.Lens' TagOptionDetail (Prelude.Maybe Prelude.Text)
tagOptionDetail_key = Lens.lens (\TagOptionDetail' {key} -> key) (\s@TagOptionDetail' {} a -> s {key = a} :: TagOptionDetail)

-- | The TagOption active state.
tagOptionDetail_active :: Lens.Lens' TagOptionDetail (Prelude.Maybe Prelude.Bool)
tagOptionDetail_active = Lens.lens (\TagOptionDetail' {active} -> active) (\s@TagOptionDetail' {} a -> s {active = a} :: TagOptionDetail)

-- | The Amazon Web Services account Id of the owner account that created the
-- TagOption.
tagOptionDetail_owner :: Lens.Lens' TagOptionDetail (Prelude.Maybe Prelude.Text)
tagOptionDetail_owner = Lens.lens (\TagOptionDetail' {owner} -> owner) (\s@TagOptionDetail' {} a -> s {owner = a} :: TagOptionDetail)

-- | The TagOption identifier.
tagOptionDetail_id :: Lens.Lens' TagOptionDetail (Prelude.Maybe Prelude.Text)
tagOptionDetail_id = Lens.lens (\TagOptionDetail' {id} -> id) (\s@TagOptionDetail' {} a -> s {id = a} :: TagOptionDetail)

-- | The TagOption value.
tagOptionDetail_value :: Lens.Lens' TagOptionDetail (Prelude.Maybe Prelude.Text)
tagOptionDetail_value = Lens.lens (\TagOptionDetail' {value} -> value) (\s@TagOptionDetail' {} a -> s {value = a} :: TagOptionDetail)

instance Data.FromJSON TagOptionDetail where
  parseJSON =
    Data.withObject
      "TagOptionDetail"
      ( \x ->
          TagOptionDetail'
            Prelude.<$> (x Data..:? "Key")
            Prelude.<*> (x Data..:? "Active")
            Prelude.<*> (x Data..:? "Owner")
            Prelude.<*> (x Data..:? "Id")
            Prelude.<*> (x Data..:? "Value")
      )

instance Prelude.Hashable TagOptionDetail where
  hashWithSalt _salt TagOptionDetail' {..} =
    _salt `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` active
      `Prelude.hashWithSalt` owner
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` value

instance Prelude.NFData TagOptionDetail where
  rnf TagOptionDetail' {..} =
    Prelude.rnf key
      `Prelude.seq` Prelude.rnf active
      `Prelude.seq` Prelude.rnf owner
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf value
