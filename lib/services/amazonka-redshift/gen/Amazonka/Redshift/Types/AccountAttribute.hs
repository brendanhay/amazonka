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
-- Module      : Amazonka.Redshift.Types.AccountAttribute
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.AccountAttribute where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.AttributeValueTarget

-- | A name value pair that describes an aspect of an account.
--
-- /See:/ 'newAccountAttribute' smart constructor.
data AccountAttribute = AccountAttribute'
  { -- | A list of attribute values.
    attributeValues :: Prelude.Maybe [AttributeValueTarget],
    -- | The name of the attribute.
    attributeName :: Prelude.Maybe Prelude.Text
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
-- 'attributeValues', 'accountAttribute_attributeValues' - A list of attribute values.
--
-- 'attributeName', 'accountAttribute_attributeName' - The name of the attribute.
newAccountAttribute ::
  AccountAttribute
newAccountAttribute =
  AccountAttribute'
    { attributeValues =
        Prelude.Nothing,
      attributeName = Prelude.Nothing
    }

-- | A list of attribute values.
accountAttribute_attributeValues :: Lens.Lens' AccountAttribute (Prelude.Maybe [AttributeValueTarget])
accountAttribute_attributeValues = Lens.lens (\AccountAttribute' {attributeValues} -> attributeValues) (\s@AccountAttribute' {} a -> s {attributeValues = a} :: AccountAttribute) Prelude.. Lens.mapping Lens.coerced

-- | The name of the attribute.
accountAttribute_attributeName :: Lens.Lens' AccountAttribute (Prelude.Maybe Prelude.Text)
accountAttribute_attributeName = Lens.lens (\AccountAttribute' {attributeName} -> attributeName) (\s@AccountAttribute' {} a -> s {attributeName = a} :: AccountAttribute)

instance Core.FromXML AccountAttribute where
  parseXML x =
    AccountAttribute'
      Prelude.<$> ( x Core..@? "AttributeValues" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "AttributeValueTarget")
                  )
      Prelude.<*> (x Core..@? "AttributeName")

instance Prelude.Hashable AccountAttribute where
  hashWithSalt _salt AccountAttribute' {..} =
    _salt `Prelude.hashWithSalt` attributeValues
      `Prelude.hashWithSalt` attributeName

instance Prelude.NFData AccountAttribute where
  rnf AccountAttribute' {..} =
    Prelude.rnf attributeValues
      `Prelude.seq` Prelude.rnf attributeName
