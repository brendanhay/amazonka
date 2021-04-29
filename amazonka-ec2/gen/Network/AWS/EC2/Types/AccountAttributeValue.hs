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
-- Module      : Network.AWS.EC2.Types.AccountAttributeValue
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.AccountAttributeValue where

import Network.AWS.EC2.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a value of an account attribute.
--
-- /See:/ 'newAccountAttributeValue' smart constructor.
data AccountAttributeValue = AccountAttributeValue'
  { -- | The value of the attribute.
    attributeValue :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'AccountAttributeValue' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeValue', 'accountAttributeValue_attributeValue' - The value of the attribute.
newAccountAttributeValue ::
  AccountAttributeValue
newAccountAttributeValue =
  AccountAttributeValue'
    { attributeValue =
        Prelude.Nothing
    }

-- | The value of the attribute.
accountAttributeValue_attributeValue :: Lens.Lens' AccountAttributeValue (Prelude.Maybe Prelude.Text)
accountAttributeValue_attributeValue = Lens.lens (\AccountAttributeValue' {attributeValue} -> attributeValue) (\s@AccountAttributeValue' {} a -> s {attributeValue = a} :: AccountAttributeValue)

instance Prelude.FromXML AccountAttributeValue where
  parseXML x =
    AccountAttributeValue'
      Prelude.<$> (x Prelude..@? "attributeValue")

instance Prelude.Hashable AccountAttributeValue

instance Prelude.NFData AccountAttributeValue
