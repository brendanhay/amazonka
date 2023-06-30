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
-- Module      : Amazonka.Lightsail.Types.ResourceRecord
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Lightsail.Types.ResourceRecord where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes the domain name system (DNS) records to add to your domain\'s
-- DNS to validate it for an Amazon Lightsail certificate.
--
-- /See:/ 'newResourceRecord' smart constructor.
data ResourceRecord = ResourceRecord'
  { -- | The name of the record.
    name :: Prelude.Maybe Prelude.Text,
    -- | The DNS record type.
    type' :: Prelude.Maybe Prelude.Text,
    -- | The value for the DNS record.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ResourceRecord' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'resourceRecord_name' - The name of the record.
--
-- 'type'', 'resourceRecord_type' - The DNS record type.
--
-- 'value', 'resourceRecord_value' - The value for the DNS record.
newResourceRecord ::
  ResourceRecord
newResourceRecord =
  ResourceRecord'
    { name = Prelude.Nothing,
      type' = Prelude.Nothing,
      value = Prelude.Nothing
    }

-- | The name of the record.
resourceRecord_name :: Lens.Lens' ResourceRecord (Prelude.Maybe Prelude.Text)
resourceRecord_name = Lens.lens (\ResourceRecord' {name} -> name) (\s@ResourceRecord' {} a -> s {name = a} :: ResourceRecord)

-- | The DNS record type.
resourceRecord_type :: Lens.Lens' ResourceRecord (Prelude.Maybe Prelude.Text)
resourceRecord_type = Lens.lens (\ResourceRecord' {type'} -> type') (\s@ResourceRecord' {} a -> s {type' = a} :: ResourceRecord)

-- | The value for the DNS record.
resourceRecord_value :: Lens.Lens' ResourceRecord (Prelude.Maybe Prelude.Text)
resourceRecord_value = Lens.lens (\ResourceRecord' {value} -> value) (\s@ResourceRecord' {} a -> s {value = a} :: ResourceRecord)

instance Data.FromJSON ResourceRecord where
  parseJSON =
    Data.withObject
      "ResourceRecord"
      ( \x ->
          ResourceRecord'
            Prelude.<$> (x Data..:? "name")
            Prelude.<*> (x Data..:? "type")
            Prelude.<*> (x Data..:? "value")
      )

instance Prelude.Hashable ResourceRecord where
  hashWithSalt _salt ResourceRecord' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` value

instance Prelude.NFData ResourceRecord where
  rnf ResourceRecord' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf value
