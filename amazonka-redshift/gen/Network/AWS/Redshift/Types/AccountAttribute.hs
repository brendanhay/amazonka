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
-- Module      : Network.AWS.Redshift.Types.AccountAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.AccountAttribute where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.AttributeValueTarget

-- | A name value pair that describes an aspect of an account.
--
-- /See:/ 'newAccountAttribute' smart constructor.
data AccountAttribute = AccountAttribute'
  { -- | The name of the attribute.
    attributeName :: Core.Maybe Core.Text,
    -- | A list of attribute values.
    attributeValues :: Core.Maybe [AttributeValueTarget]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AccountAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeName', 'accountAttribute_attributeName' - The name of the attribute.
--
-- 'attributeValues', 'accountAttribute_attributeValues' - A list of attribute values.
newAccountAttribute ::
  AccountAttribute
newAccountAttribute =
  AccountAttribute'
    { attributeName = Core.Nothing,
      attributeValues = Core.Nothing
    }

-- | The name of the attribute.
accountAttribute_attributeName :: Lens.Lens' AccountAttribute (Core.Maybe Core.Text)
accountAttribute_attributeName = Lens.lens (\AccountAttribute' {attributeName} -> attributeName) (\s@AccountAttribute' {} a -> s {attributeName = a} :: AccountAttribute)

-- | A list of attribute values.
accountAttribute_attributeValues :: Lens.Lens' AccountAttribute (Core.Maybe [AttributeValueTarget])
accountAttribute_attributeValues = Lens.lens (\AccountAttribute' {attributeValues} -> attributeValues) (\s@AccountAttribute' {} a -> s {attributeValues = a} :: AccountAttribute) Core.. Lens.mapping Lens._Coerce

instance Core.FromXML AccountAttribute where
  parseXML x =
    AccountAttribute'
      Core.<$> (x Core..@? "AttributeName")
      Core.<*> ( x Core..@? "AttributeValues" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "AttributeValueTarget")
               )

instance Core.Hashable AccountAttribute

instance Core.NFData AccountAttribute
