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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes the domain name system (DNS) records to add to your domain\'s
-- DNS to validate it for an Amazon Lightsail certificate.
--
-- /See:/ 'newResourceRecord' smart constructor.
data ResourceRecord = ResourceRecord'
  { -- | The name of the record.
    name :: Core.Maybe Core.Text,
    -- | The value for the DNS record.
    value :: Core.Maybe Core.Text,
    -- | The DNS record type.
    type' :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
    { name = Core.Nothing,
      value = Core.Nothing,
      type' = Core.Nothing
    }

-- | The name of the record.
resourceRecord_name :: Lens.Lens' ResourceRecord (Core.Maybe Core.Text)
resourceRecord_name = Lens.lens (\ResourceRecord' {name} -> name) (\s@ResourceRecord' {} a -> s {name = a} :: ResourceRecord)

-- | The value for the DNS record.
resourceRecord_value :: Lens.Lens' ResourceRecord (Core.Maybe Core.Text)
resourceRecord_value = Lens.lens (\ResourceRecord' {value} -> value) (\s@ResourceRecord' {} a -> s {value = a} :: ResourceRecord)

-- | The DNS record type.
resourceRecord_type :: Lens.Lens' ResourceRecord (Core.Maybe Core.Text)
resourceRecord_type = Lens.lens (\ResourceRecord' {type'} -> type') (\s@ResourceRecord' {} a -> s {type' = a} :: ResourceRecord)

instance Core.FromJSON ResourceRecord where
  parseJSON =
    Core.withObject
      "ResourceRecord"
      ( \x ->
          ResourceRecord'
            Core.<$> (x Core..:? "name")
            Core.<*> (x Core..:? "value")
            Core.<*> (x Core..:? "type")
      )

instance Core.Hashable ResourceRecord

instance Core.NFData ResourceRecord
