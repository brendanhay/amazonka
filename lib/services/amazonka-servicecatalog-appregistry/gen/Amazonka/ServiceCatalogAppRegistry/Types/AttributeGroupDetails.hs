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
-- Module      : Amazonka.ServiceCatalogAppRegistry.Types.AttributeGroupDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalogAppRegistry.Types.AttributeGroupDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The details related to a specific AttributeGroup.
--
-- /See:/ 'newAttributeGroupDetails' smart constructor.
data AttributeGroupDetails = AttributeGroupDetails'
  { -- | This field is no longer supported. We recommend you don\'t use the field
    -- when using @ListAttributeGroupsForApplication@.
    --
    -- The name of the attribute group.
    name :: Prelude.Maybe Prelude.Text,
    -- | The Amazon resource name (ARN) that specifies the attribute group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier of the attribute group.
    id :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeGroupDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'attributeGroupDetails_name' - This field is no longer supported. We recommend you don\'t use the field
-- when using @ListAttributeGroupsForApplication@.
--
-- The name of the attribute group.
--
-- 'arn', 'attributeGroupDetails_arn' - The Amazon resource name (ARN) that specifies the attribute group.
--
-- 'id', 'attributeGroupDetails_id' - The unique identifier of the attribute group.
newAttributeGroupDetails ::
  AttributeGroupDetails
newAttributeGroupDetails =
  AttributeGroupDetails'
    { name = Prelude.Nothing,
      arn = Prelude.Nothing,
      id = Prelude.Nothing
    }

-- | This field is no longer supported. We recommend you don\'t use the field
-- when using @ListAttributeGroupsForApplication@.
--
-- The name of the attribute group.
attributeGroupDetails_name :: Lens.Lens' AttributeGroupDetails (Prelude.Maybe Prelude.Text)
attributeGroupDetails_name = Lens.lens (\AttributeGroupDetails' {name} -> name) (\s@AttributeGroupDetails' {} a -> s {name = a} :: AttributeGroupDetails)

-- | The Amazon resource name (ARN) that specifies the attribute group.
attributeGroupDetails_arn :: Lens.Lens' AttributeGroupDetails (Prelude.Maybe Prelude.Text)
attributeGroupDetails_arn = Lens.lens (\AttributeGroupDetails' {arn} -> arn) (\s@AttributeGroupDetails' {} a -> s {arn = a} :: AttributeGroupDetails)

-- | The unique identifier of the attribute group.
attributeGroupDetails_id :: Lens.Lens' AttributeGroupDetails (Prelude.Maybe Prelude.Text)
attributeGroupDetails_id = Lens.lens (\AttributeGroupDetails' {id} -> id) (\s@AttributeGroupDetails' {} a -> s {id = a} :: AttributeGroupDetails)

instance Data.FromJSON AttributeGroupDetails where
  parseJSON =
    Data.withObject
      "AttributeGroupDetails"
      ( \x ->
          AttributeGroupDetails'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "arn")
            Prelude.<*> (x Data..:? "id")
      )

instance Prelude.Hashable AttributeGroupDetails where
  hashWithSalt _salt AttributeGroupDetails' {..} =
    _salt `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` id

instance Prelude.NFData AttributeGroupDetails where
  rnf AttributeGroupDetails' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf id
