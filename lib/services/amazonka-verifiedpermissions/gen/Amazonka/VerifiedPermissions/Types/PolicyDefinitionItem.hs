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
-- Module      : Amazonka.VerifiedPermissions.Types.PolicyDefinitionItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.PolicyDefinitionItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VerifiedPermissions.Types.StaticPolicyDefinitionItem
import Amazonka.VerifiedPermissions.Types.TemplateLinkedPolicyDefinitionItem

-- | A structure that describes a
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_PolicyDefinintion.html PolicyDefinintion>.
-- It will always have either an @StaticPolicy@ or a @TemplateLinkedPolicy@
-- element.
--
-- This data type is used as a response parameter for the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_CreatePolicy.html CreatePolicy>
-- and
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_ListPolicies.html ListPolicies>
-- operations.
--
-- /See:/ 'newPolicyDefinitionItem' smart constructor.
data PolicyDefinitionItem = PolicyDefinitionItem'
  { -- | Information about a static policy that wasn\'t created with a policy
    -- template.
    static :: Prelude.Maybe StaticPolicyDefinitionItem,
    -- | Information about a template-linked policy that was created by
    -- instantiating a policy template.
    templateLinked :: Prelude.Maybe TemplateLinkedPolicyDefinitionItem
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyDefinitionItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'static', 'policyDefinitionItem_static' - Information about a static policy that wasn\'t created with a policy
-- template.
--
-- 'templateLinked', 'policyDefinitionItem_templateLinked' - Information about a template-linked policy that was created by
-- instantiating a policy template.
newPolicyDefinitionItem ::
  PolicyDefinitionItem
newPolicyDefinitionItem =
  PolicyDefinitionItem'
    { static = Prelude.Nothing,
      templateLinked = Prelude.Nothing
    }

-- | Information about a static policy that wasn\'t created with a policy
-- template.
policyDefinitionItem_static :: Lens.Lens' PolicyDefinitionItem (Prelude.Maybe StaticPolicyDefinitionItem)
policyDefinitionItem_static = Lens.lens (\PolicyDefinitionItem' {static} -> static) (\s@PolicyDefinitionItem' {} a -> s {static = a} :: PolicyDefinitionItem)

-- | Information about a template-linked policy that was created by
-- instantiating a policy template.
policyDefinitionItem_templateLinked :: Lens.Lens' PolicyDefinitionItem (Prelude.Maybe TemplateLinkedPolicyDefinitionItem)
policyDefinitionItem_templateLinked = Lens.lens (\PolicyDefinitionItem' {templateLinked} -> templateLinked) (\s@PolicyDefinitionItem' {} a -> s {templateLinked = a} :: PolicyDefinitionItem)

instance Data.FromJSON PolicyDefinitionItem where
  parseJSON =
    Data.withObject
      "PolicyDefinitionItem"
      ( \x ->
          PolicyDefinitionItem'
            Prelude.<$> (x Data..:? "static")
            Prelude.<*> (x Data..:? "templateLinked")
      )

instance Prelude.Hashable PolicyDefinitionItem where
  hashWithSalt _salt PolicyDefinitionItem' {..} =
    _salt
      `Prelude.hashWithSalt` static
      `Prelude.hashWithSalt` templateLinked

instance Prelude.NFData PolicyDefinitionItem where
  rnf PolicyDefinitionItem' {..} =
    Prelude.rnf static
      `Prelude.seq` Prelude.rnf templateLinked
