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
-- Module      : Network.AWS.EC2.Types.AttributeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AttributeValue where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a value for a resource attribute that is a String.
--
-- /See:/ 'newAttributeValue' smart constructor.
data AttributeValue = AttributeValue'
  { -- | The attribute value. The value is case-sensitive.
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
-- 'value', 'attributeValue_value' - The attribute value. The value is case-sensitive.
newAttributeValue ::
  AttributeValue
newAttributeValue =
  AttributeValue' {value = Prelude.Nothing}

-- | The attribute value. The value is case-sensitive.
attributeValue_value :: Lens.Lens' AttributeValue (Prelude.Maybe Prelude.Text)
attributeValue_value = Lens.lens (\AttributeValue' {value} -> value) (\s@AttributeValue' {} a -> s {value = a} :: AttributeValue)

instance Prelude.FromXML AttributeValue where
  parseXML x =
    AttributeValue' Prelude.<$> (x Prelude..@? "value")

instance Prelude.Hashable AttributeValue

instance Prelude.NFData AttributeValue

instance Prelude.ToQuery AttributeValue where
  toQuery AttributeValue' {..} =
    Prelude.mconcat ["Value" Prelude.=: value]
