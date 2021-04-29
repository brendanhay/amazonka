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
-- Module      : Network.AWS.Pricing.Types.AttributeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Pricing.Types.AttributeValue where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The values of a given attribute, such as @Throughput Optimized HDD@ or
-- @Provisioned IOPS@ for the @Amazon EC2@ @volumeType@ attribute.
--
-- /See:/ 'newAttributeValue' smart constructor.
data AttributeValue = AttributeValue'
  { -- | The specific value of an @attributeName@.
    value :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.FromJSON AttributeValue where
  parseJSON =
    Prelude.withObject
      "AttributeValue"
      ( \x ->
          AttributeValue' Prelude.<$> (x Prelude..:? "Value")
      )

instance Prelude.Hashable AttributeValue

instance Prelude.NFData AttributeValue
