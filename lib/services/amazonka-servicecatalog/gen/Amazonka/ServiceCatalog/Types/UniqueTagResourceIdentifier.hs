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
-- Module      : Amazonka.ServiceCatalog.Types.UniqueTagResourceIdentifier
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ServiceCatalog.Types.UniqueTagResourceIdentifier where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The unique key-value pair for a tag that identifies provisioned product
-- resources.
--
-- /See:/ 'newUniqueTagResourceIdentifier' smart constructor.
data UniqueTagResourceIdentifier = UniqueTagResourceIdentifier'
  { -- | A unique key that\'s attached to a resource.
    key :: Prelude.Maybe Prelude.Text,
    -- | A unique value that\'s attached to a resource.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UniqueTagResourceIdentifier' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'key', 'uniqueTagResourceIdentifier_key' - A unique key that\'s attached to a resource.
--
-- 'value', 'uniqueTagResourceIdentifier_value' - A unique value that\'s attached to a resource.
newUniqueTagResourceIdentifier ::
  UniqueTagResourceIdentifier
newUniqueTagResourceIdentifier =
  UniqueTagResourceIdentifier'
    { key = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | A unique key that\'s attached to a resource.
uniqueTagResourceIdentifier_key :: Lens.Lens' UniqueTagResourceIdentifier (Prelude.Maybe Prelude.Text)
uniqueTagResourceIdentifier_key = Lens.lens (\UniqueTagResourceIdentifier' {key} -> key) (\s@UniqueTagResourceIdentifier' {} a -> s {key = a} :: UniqueTagResourceIdentifier)

-- | A unique value that\'s attached to a resource.
uniqueTagResourceIdentifier_value :: Lens.Lens' UniqueTagResourceIdentifier (Prelude.Maybe Prelude.Text)
uniqueTagResourceIdentifier_value = Lens.lens (\UniqueTagResourceIdentifier' {value} -> value) (\s@UniqueTagResourceIdentifier' {} a -> s {value = a} :: UniqueTagResourceIdentifier)

instance Prelude.Hashable UniqueTagResourceIdentifier where
  hashWithSalt _salt UniqueTagResourceIdentifier' {..} =
    _salt
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` value

instance Prelude.NFData UniqueTagResourceIdentifier where
  rnf UniqueTagResourceIdentifier' {..} =
    Prelude.rnf key `Prelude.seq` Prelude.rnf value

instance Data.ToJSON UniqueTagResourceIdentifier where
  toJSON UniqueTagResourceIdentifier' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Key" Data..=) Prelude.<$> key,
            ("Value" Data..=) Prelude.<$> value
          ]
      )
