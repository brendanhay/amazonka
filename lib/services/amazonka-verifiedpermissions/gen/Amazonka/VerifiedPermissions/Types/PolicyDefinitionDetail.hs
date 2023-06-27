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
-- Module      : Amazonka.VerifiedPermissions.Types.PolicyDefinitionDetail
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.PolicyDefinitionDetail where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VerifiedPermissions.Types.StaticPolicyDefinitionDetail
import Amazonka.VerifiedPermissions.Types.TemplateLinkedPolicyDefinitionDetail

-- | A structure that describes a policy definition. It must always have
-- either an @static@ or a @templateLinked@ element.
--
-- This data type is used as a response parameter for the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_GetPolicy.html GetPolicy>
-- operation.
--
-- /See:/ 'newPolicyDefinitionDetail' smart constructor.
data PolicyDefinitionDetail = PolicyDefinitionDetail'
  { -- | Information about a static policy that wasn\'t created with a policy
    -- template.
    static :: Prelude.Maybe StaticPolicyDefinitionDetail,
    -- | Information about a template-linked policy that was created by
    -- instantiating a policy template.
    templateLinked :: Prelude.Maybe TemplateLinkedPolicyDefinitionDetail
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyDefinitionDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'static', 'policyDefinitionDetail_static' - Information about a static policy that wasn\'t created with a policy
-- template.
--
-- 'templateLinked', 'policyDefinitionDetail_templateLinked' - Information about a template-linked policy that was created by
-- instantiating a policy template.
newPolicyDefinitionDetail ::
  PolicyDefinitionDetail
newPolicyDefinitionDetail =
  PolicyDefinitionDetail'
    { static = Prelude.Nothing,
      templateLinked = Prelude.Nothing
    }

-- | Information about a static policy that wasn\'t created with a policy
-- template.
policyDefinitionDetail_static :: Lens.Lens' PolicyDefinitionDetail (Prelude.Maybe StaticPolicyDefinitionDetail)
policyDefinitionDetail_static = Lens.lens (\PolicyDefinitionDetail' {static} -> static) (\s@PolicyDefinitionDetail' {} a -> s {static = a} :: PolicyDefinitionDetail)

-- | Information about a template-linked policy that was created by
-- instantiating a policy template.
policyDefinitionDetail_templateLinked :: Lens.Lens' PolicyDefinitionDetail (Prelude.Maybe TemplateLinkedPolicyDefinitionDetail)
policyDefinitionDetail_templateLinked = Lens.lens (\PolicyDefinitionDetail' {templateLinked} -> templateLinked) (\s@PolicyDefinitionDetail' {} a -> s {templateLinked = a} :: PolicyDefinitionDetail)

instance Data.FromJSON PolicyDefinitionDetail where
  parseJSON =
    Data.withObject
      "PolicyDefinitionDetail"
      ( \x ->
          PolicyDefinitionDetail'
            Prelude.<$> (x Data..:? "static")
            Prelude.<*> (x Data..:? "templateLinked")
      )

instance Prelude.Hashable PolicyDefinitionDetail where
  hashWithSalt _salt PolicyDefinitionDetail' {..} =
    _salt
      `Prelude.hashWithSalt` static
      `Prelude.hashWithSalt` templateLinked

instance Prelude.NFData PolicyDefinitionDetail where
  rnf PolicyDefinitionDetail' {..} =
    Prelude.rnf static
      `Prelude.seq` Prelude.rnf templateLinked
