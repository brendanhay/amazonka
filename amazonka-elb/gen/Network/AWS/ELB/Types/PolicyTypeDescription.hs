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
-- Module      : Network.AWS.ELB.Types.PolicyTypeDescription
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELB.Types.PolicyTypeDescription where

import Network.AWS.ELB.Internal
import Network.AWS.ELB.Types.PolicyAttributeTypeDescription
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Information about a policy type.
--
-- /See:/ 'newPolicyTypeDescription' smart constructor.
data PolicyTypeDescription = PolicyTypeDescription'
  { -- | The description of the policy attributes associated with the policies
    -- defined by Elastic Load Balancing.
    policyAttributeTypeDescriptions :: Prelude.Maybe [PolicyAttributeTypeDescription],
    -- | The name of the policy type.
    policyTypeName :: Prelude.Maybe Prelude.Text,
    -- | A description of the policy type.
    description :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PolicyTypeDescription' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyAttributeTypeDescriptions', 'policyTypeDescription_policyAttributeTypeDescriptions' - The description of the policy attributes associated with the policies
-- defined by Elastic Load Balancing.
--
-- 'policyTypeName', 'policyTypeDescription_policyTypeName' - The name of the policy type.
--
-- 'description', 'policyTypeDescription_description' - A description of the policy type.
newPolicyTypeDescription ::
  PolicyTypeDescription
newPolicyTypeDescription =
  PolicyTypeDescription'
    { policyAttributeTypeDescriptions =
        Prelude.Nothing,
      policyTypeName = Prelude.Nothing,
      description = Prelude.Nothing
    }

-- | The description of the policy attributes associated with the policies
-- defined by Elastic Load Balancing.
policyTypeDescription_policyAttributeTypeDescriptions :: Lens.Lens' PolicyTypeDescription (Prelude.Maybe [PolicyAttributeTypeDescription])
policyTypeDescription_policyAttributeTypeDescriptions = Lens.lens (\PolicyTypeDescription' {policyAttributeTypeDescriptions} -> policyAttributeTypeDescriptions) (\s@PolicyTypeDescription' {} a -> s {policyAttributeTypeDescriptions = a} :: PolicyTypeDescription) Prelude.. Lens.mapping Prelude._Coerce

-- | The name of the policy type.
policyTypeDescription_policyTypeName :: Lens.Lens' PolicyTypeDescription (Prelude.Maybe Prelude.Text)
policyTypeDescription_policyTypeName = Lens.lens (\PolicyTypeDescription' {policyTypeName} -> policyTypeName) (\s@PolicyTypeDescription' {} a -> s {policyTypeName = a} :: PolicyTypeDescription)

-- | A description of the policy type.
policyTypeDescription_description :: Lens.Lens' PolicyTypeDescription (Prelude.Maybe Prelude.Text)
policyTypeDescription_description = Lens.lens (\PolicyTypeDescription' {description} -> description) (\s@PolicyTypeDescription' {} a -> s {description = a} :: PolicyTypeDescription)

instance Prelude.FromXML PolicyTypeDescription where
  parseXML x =
    PolicyTypeDescription'
      Prelude.<$> ( x Prelude..@? "PolicyAttributeTypeDescriptions"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "member")
                  )
      Prelude.<*> (x Prelude..@? "PolicyTypeName")
      Prelude.<*> (x Prelude..@? "Description")

instance Prelude.Hashable PolicyTypeDescription

instance Prelude.NFData PolicyTypeDescription
