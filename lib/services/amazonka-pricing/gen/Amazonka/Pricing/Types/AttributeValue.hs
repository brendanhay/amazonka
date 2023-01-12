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
-- Module      : Amazonka.Pricing.Types.AttributeValue
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Pricing.Types.AttributeValue where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The values of a given attribute, such as @Throughput Optimized HDD@ or
-- @Provisioned IOPS@ for the @Amazon EC2@ @volumeType@ attribute.
--
-- /See:/ 'newAttributeValue' smart constructor.
data AttributeValue = AttributeValue'
  { -- | The specific value of an @attributeName@.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'attributeValue_value' - The specific value of an @attributeName@.
newAttributeValue ::
  AttributeValue
newAttributeValue =
  AttributeValue' {value = Prelude.Nothing}

-- | The specific value of an @attributeName@.
attributeValue_value :: Lens.Lens' AttributeValue (Prelude.Maybe Prelude.Text)
attributeValue_value = Lens.lens (\AttributeValue' {value} -> value) (\s@AttributeValue' {} a -> s {value = a} :: AttributeValue)

instance Data.FromJSON AttributeValue where
  parseJSON =
    Data.withObject
      "AttributeValue"
      ( \x ->
          AttributeValue' Prelude.<$> (x Data..:? "Value")
      )

instance Prelude.Hashable AttributeValue where
  hashWithSalt _salt AttributeValue' {..} =
    _salt `Prelude.hashWithSalt` value

instance Prelude.NFData AttributeValue where
  rnf AttributeValue' {..} = Prelude.rnf value
