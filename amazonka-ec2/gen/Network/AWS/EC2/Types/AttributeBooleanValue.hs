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
-- Module      : Network.AWS.EC2.Types.AttributeBooleanValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AttributeBooleanValue where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a value for a resource attribute that is a Boolean value.
--
-- /See:/ 'newAttributeBooleanValue' smart constructor.
data AttributeBooleanValue = AttributeBooleanValue'
  { -- | The attribute value. The valid values are @true@ or @false@.
    value :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AttributeBooleanValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'value', 'attributeBooleanValue_value' - The attribute value. The valid values are @true@ or @false@.
newAttributeBooleanValue ::
  AttributeBooleanValue
newAttributeBooleanValue =
  AttributeBooleanValue' {value = Prelude.Nothing}

-- | The attribute value. The valid values are @true@ or @false@.
attributeBooleanValue_value :: Lens.Lens' AttributeBooleanValue (Prelude.Maybe Prelude.Bool)
attributeBooleanValue_value = Lens.lens (\AttributeBooleanValue' {value} -> value) (\s@AttributeBooleanValue' {} a -> s {value = a} :: AttributeBooleanValue)

instance Prelude.FromXML AttributeBooleanValue where
  parseXML x =
    AttributeBooleanValue'
      Prelude.<$> (x Prelude..@? "value")

instance Prelude.Hashable AttributeBooleanValue

instance Prelude.NFData AttributeBooleanValue

instance Prelude.ToQuery AttributeBooleanValue where
  toQuery AttributeBooleanValue' {..} =
    Prelude.mconcat ["Value" Prelude.=: value]
