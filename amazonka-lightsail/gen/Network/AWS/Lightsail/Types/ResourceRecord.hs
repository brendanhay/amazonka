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
-- Module      : Network.AWS.Lightsail.Types.ResourceRecord
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.ResourceRecord where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes the domain name system (DNS) records to add to your domain\'s
-- DNS to validate it for an Amazon Lightsail certificate.
--
-- /See:/ 'newResourceRecord' smart constructor.
data ResourceRecord = ResourceRecord'
  { -- | The name of the record.
    name :: Prelude.Maybe Prelude.Text,
    -- | The value for the DNS record.
    value :: Prelude.Maybe Prelude.Text,
    -- | The DNS record type.
    type' :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
-- 'value', 'resourceRecord_value' - The value for the DNS record.
--
-- 'type'', 'resourceRecord_type' - The DNS record type.
newResourceRecord ::
  ResourceRecord
newResourceRecord =
  ResourceRecord'
    { name = Prelude.Nothing,
      value = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | The name of the record.
resourceRecord_name :: Lens.Lens' ResourceRecord (Prelude.Maybe Prelude.Text)
resourceRecord_name = Lens.lens (\ResourceRecord' {name} -> name) (\s@ResourceRecord' {} a -> s {name = a} :: ResourceRecord)

-- | The value for the DNS record.
resourceRecord_value :: Lens.Lens' ResourceRecord (Prelude.Maybe Prelude.Text)
resourceRecord_value = Lens.lens (\ResourceRecord' {value} -> value) (\s@ResourceRecord' {} a -> s {value = a} :: ResourceRecord)

-- | The DNS record type.
resourceRecord_type :: Lens.Lens' ResourceRecord (Prelude.Maybe Prelude.Text)
resourceRecord_type = Lens.lens (\ResourceRecord' {type'} -> type') (\s@ResourceRecord' {} a -> s {type' = a} :: ResourceRecord)

instance Prelude.FromJSON ResourceRecord where
  parseJSON =
    Prelude.withObject
      "ResourceRecord"
      ( \x ->
          ResourceRecord'
            Prelude.<$> (x Prelude..:? "name")
            Prelude.<*> (x Prelude..:? "value")
            Prelude.<*> (x Prelude..:? "type")
      )

instance Prelude.Hashable ResourceRecord

instance Prelude.NFData ResourceRecord
