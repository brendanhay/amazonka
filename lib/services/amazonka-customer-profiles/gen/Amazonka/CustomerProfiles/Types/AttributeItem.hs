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
-- Module      : Amazonka.CustomerProfiles.Types.AttributeItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.CustomerProfiles.Types.AttributeItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details of a single attribute item specified in the mathematical
-- expression.
--
-- /See:/ 'newAttributeItem' smart constructor.
data AttributeItem = AttributeItem'
  { -- | The name of an attribute defined in a profile object type.
    name :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'attributeItem_name' - The name of an attribute defined in a profile object type.
newAttributeItem ::
  -- | 'name'
  Prelude.Text ->
  AttributeItem
newAttributeItem pName_ =
  AttributeItem' {name = pName_}

-- | The name of an attribute defined in a profile object type.
attributeItem_name :: Lens.Lens' AttributeItem Prelude.Text
attributeItem_name = Lens.lens (\AttributeItem' {name} -> name) (\s@AttributeItem' {} a -> s {name = a} :: AttributeItem)

instance Data.FromJSON AttributeItem where
  parseJSON =
    Data.withObject
      "AttributeItem"
      ( \x ->
          AttributeItem' Prelude.<$> (x Data..: "Name")
      )

instance Prelude.Hashable AttributeItem where
  hashWithSalt _salt AttributeItem' {..} =
    _salt `Prelude.hashWithSalt` name

instance Prelude.NFData AttributeItem where
  rnf AttributeItem' {..} = Prelude.rnf name

instance Data.ToJSON AttributeItem where
  toJSON AttributeItem' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("Name" Data..= name)]
      )
