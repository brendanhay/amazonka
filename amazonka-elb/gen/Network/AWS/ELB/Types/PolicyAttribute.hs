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
-- Module      : Network.AWS.ELB.Types.PolicyAttribute
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.PolicyAttribute where

import Network.AWS.ELB.Internal
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a policy attribute.
--
-- /See:/ 'newPolicyAttribute' smart constructor.
data PolicyAttribute = PolicyAttribute'
  { -- | The value of the attribute.
    attributeValue :: Prelude.Maybe Prelude.Text,
    -- | The name of the attribute.
    attributeName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PolicyAttribute' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'attributeValue', 'policyAttribute_attributeValue' - The value of the attribute.
--
-- 'attributeName', 'policyAttribute_attributeName' - The name of the attribute.
newPolicyAttribute ::
  PolicyAttribute
newPolicyAttribute =
  PolicyAttribute'
    { attributeValue = Prelude.Nothing,
      attributeName = Prelude.Nothing
    }

-- | The value of the attribute.
policyAttribute_attributeValue :: Lens.Lens' PolicyAttribute (Prelude.Maybe Prelude.Text)
policyAttribute_attributeValue = Lens.lens (\PolicyAttribute' {attributeValue} -> attributeValue) (\s@PolicyAttribute' {} a -> s {attributeValue = a} :: PolicyAttribute)

-- | The name of the attribute.
policyAttribute_attributeName :: Lens.Lens' PolicyAttribute (Prelude.Maybe Prelude.Text)
policyAttribute_attributeName = Lens.lens (\PolicyAttribute' {attributeName} -> attributeName) (\s@PolicyAttribute' {} a -> s {attributeName = a} :: PolicyAttribute)

instance Prelude.Hashable PolicyAttribute

instance Prelude.NFData PolicyAttribute

instance Prelude.ToQuery PolicyAttribute where
  toQuery PolicyAttribute' {..} =
    Prelude.mconcat
      [ "AttributeValue" Prelude.=: attributeValue,
        "AttributeName" Prelude.=: attributeName
      ]
