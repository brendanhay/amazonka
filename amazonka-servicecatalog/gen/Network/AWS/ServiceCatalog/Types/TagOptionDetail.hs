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
-- Module      : Network.AWS.ServiceCatalog.Types.TagOptionDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ServiceCatalog.Types.TagOptionDetail where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a TagOption.
--
-- /See:/ 'newTagOptionDetail' smart constructor.
data TagOptionDetail = TagOptionDetail'
  { -- | The TagOption key.
    key :: Prelude.Maybe Prelude.Text,
    -- | The TagOption identifier.
    id :: Prelude.Maybe Prelude.Text,
    -- | The TagOption active state.
    active :: Prelude.Maybe Prelude.Bool,
    -- | The AWS account Id of the owner account that created the TagOption.
    owner :: Prelude.Maybe Prelude.Text,
    -- | The TagOption value.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'id', 'tagOptionDetail_id' - The TagOption identifier.
--
-- 'active', 'tagOptionDetail_active' - The TagOption active state.
--
-- 'owner', 'tagOptionDetail_owner' - The AWS account Id of the owner account that created the TagOption.
--
-- 'value', 'tagOptionDetail_value' - The TagOption value.
newTagOptionDetail ::
  TagOptionDetail
newTagOptionDetail =
  TagOptionDetail'
    { key = Prelude.Nothing,
      id = Prelude.Nothing,
      active = Prelude.Nothing,
      owner = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The TagOption key.
tagOptionDetail_key :: Lens.Lens' TagOptionDetail (Prelude.Maybe Prelude.Text)
tagOptionDetail_key = Lens.lens (\TagOptionDetail' {key} -> key) (\s@TagOptionDetail' {} a -> s {key = a} :: TagOptionDetail)

-- | The TagOption identifier.
tagOptionDetail_id :: Lens.Lens' TagOptionDetail (Prelude.Maybe Prelude.Text)
tagOptionDetail_id = Lens.lens (\TagOptionDetail' {id} -> id) (\s@TagOptionDetail' {} a -> s {id = a} :: TagOptionDetail)

-- | The TagOption active state.
tagOptionDetail_active :: Lens.Lens' TagOptionDetail (Prelude.Maybe Prelude.Bool)
tagOptionDetail_active = Lens.lens (\TagOptionDetail' {active} -> active) (\s@TagOptionDetail' {} a -> s {active = a} :: TagOptionDetail)

-- | The AWS account Id of the owner account that created the TagOption.
tagOptionDetail_owner :: Lens.Lens' TagOptionDetail (Prelude.Maybe Prelude.Text)
tagOptionDetail_owner = Lens.lens (\TagOptionDetail' {owner} -> owner) (\s@TagOptionDetail' {} a -> s {owner = a} :: TagOptionDetail)

-- | The TagOption value.
tagOptionDetail_value :: Lens.Lens' TagOptionDetail (Prelude.Maybe Prelude.Text)
tagOptionDetail_value = Lens.lens (\TagOptionDetail' {value} -> value) (\s@TagOptionDetail' {} a -> s {value = a} :: TagOptionDetail)

instance Prelude.FromJSON TagOptionDetail where
  parseJSON =
    Prelude.withObject
      "TagOptionDetail"
      ( \x ->
          TagOptionDetail'
            Prelude.<$> (x Prelude..:? "Key")
            Prelude.<*> (x Prelude..:? "Id")
            Prelude.<*> (x Prelude..:? "Active")
            Prelude.<*> (x Prelude..:? "Owner")
            Prelude.<*> (x Prelude..:? "Value")
      )

instance Prelude.Hashable TagOptionDetail

instance Prelude.NFData TagOptionDetail
