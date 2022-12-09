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
-- Module      : Amazonka.EC2.Types.AccountAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.AccountAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.AccountAttributeValue
import qualified Amazonka.Prelude as Prelude

-- | Describes an account attribute.
--
-- /See:/ 'newAccountAttribute' smart constructor.
data AccountAttribute = AccountAttribute'
  { -- | The name of the account attribute.
    attributeName :: Prelude.Maybe Prelude.Text,
    -- | The values for the account attribute.
    attributeValues :: Prelude.Maybe [AccountAttributeValue]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AccountAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'accountAttribute_attributeName' - The name of the account attribute.
--
-- 'attributeValues', 'accountAttribute_attributeValues' - The values for the account attribute.
newAccountAttribute ::
  AccountAttribute
newAccountAttribute =
  AccountAttribute'
    { attributeName = Prelude.Nothing,
      attributeValues = Prelude.Nothing
    }

-- | The name of the account attribute.
accountAttribute_attributeName :: Lens.Lens' AccountAttribute (Prelude.Maybe Prelude.Text)
accountAttribute_attributeName = Lens.lens (\AccountAttribute' {attributeName} -> attributeName) (\s@AccountAttribute' {} a -> s {attributeName = a} :: AccountAttribute)

-- | The values for the account attribute.
accountAttribute_attributeValues :: Lens.Lens' AccountAttribute (Prelude.Maybe [AccountAttributeValue])
accountAttribute_attributeValues = Lens.lens (\AccountAttribute' {attributeValues} -> attributeValues) (\s@AccountAttribute' {} a -> s {attributeValues = a} :: AccountAttribute) Prelude.. Lens.mapping Lens.coerced

instance Data.FromXML AccountAttribute where
  parseXML x =
    AccountAttribute'
      Prelude.<$> (x Data..@? "attributeName")
      Prelude.<*> ( x Data..@? "attributeValueSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )

instance Prelude.Hashable AccountAttribute where
  hashWithSalt _salt AccountAttribute' {..} =
    _salt `Prelude.hashWithSalt` attributeName
      `Prelude.hashWithSalt` attributeValues

instance Prelude.NFData AccountAttribute where
  rnf AccountAttribute' {..} =
    Prelude.rnf attributeName
      `Prelude.seq` Prelude.rnf attributeValues
