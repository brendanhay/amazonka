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
-- Module      : Amazonka.VerifiedPermissions.Types.PolicyDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.PolicyDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VerifiedPermissions.Types.StaticPolicyDefinition
import Amazonka.VerifiedPermissions.Types.TemplateLinkedPolicyDefinition

-- | A structure that contains the details for a Cedar policy definition. It
-- includes the policy type, a description, and a policy body. This is a
-- top level data type used to create a policy.
--
-- This data type is used as a request parameter for the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_CreatePolicy.html CreatePolicy>
-- operation. This structure must always have either an @static@ or a
-- @templateLinked@ element.
--
-- /See:/ 'newPolicyDefinition' smart constructor.
data PolicyDefinition = PolicyDefinition'
  { -- | A structure that describes a static policy. An static policy doesn\'t
    -- use a template or allow placeholders for entities.
    static :: Prelude.Maybe StaticPolicyDefinition,
    -- | A structure that describes a policy that was instantiated from a
    -- template. The template can specify placeholders for @principal@ and
    -- @resource@. When you use
    -- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_CreatePolicy.html CreatePolicy>
    -- to create a policy from a template, you specify the exact principal and
    -- resource to use for the instantiated policy.
    templateLinked :: Prelude.Maybe TemplateLinkedPolicyDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PolicyDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'static', 'policyDefinition_static' - A structure that describes a static policy. An static policy doesn\'t
-- use a template or allow placeholders for entities.
--
-- 'templateLinked', 'policyDefinition_templateLinked' - A structure that describes a policy that was instantiated from a
-- template. The template can specify placeholders for @principal@ and
-- @resource@. When you use
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_CreatePolicy.html CreatePolicy>
-- to create a policy from a template, you specify the exact principal and
-- resource to use for the instantiated policy.
newPolicyDefinition ::
  PolicyDefinition
newPolicyDefinition =
  PolicyDefinition'
    { static = Prelude.Nothing,
      templateLinked = Prelude.Nothing
    }

-- | A structure that describes a static policy. An static policy doesn\'t
-- use a template or allow placeholders for entities.
policyDefinition_static :: Lens.Lens' PolicyDefinition (Prelude.Maybe StaticPolicyDefinition)
policyDefinition_static = Lens.lens (\PolicyDefinition' {static} -> static) (\s@PolicyDefinition' {} a -> s {static = a} :: PolicyDefinition)

-- | A structure that describes a policy that was instantiated from a
-- template. The template can specify placeholders for @principal@ and
-- @resource@. When you use
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_CreatePolicy.html CreatePolicy>
-- to create a policy from a template, you specify the exact principal and
-- resource to use for the instantiated policy.
policyDefinition_templateLinked :: Lens.Lens' PolicyDefinition (Prelude.Maybe TemplateLinkedPolicyDefinition)
policyDefinition_templateLinked = Lens.lens (\PolicyDefinition' {templateLinked} -> templateLinked) (\s@PolicyDefinition' {} a -> s {templateLinked = a} :: PolicyDefinition)

instance Prelude.Hashable PolicyDefinition where
  hashWithSalt _salt PolicyDefinition' {..} =
    _salt
      `Prelude.hashWithSalt` static
      `Prelude.hashWithSalt` templateLinked

instance Prelude.NFData PolicyDefinition where
  rnf PolicyDefinition' {..} =
    Prelude.rnf static
      `Prelude.seq` Prelude.rnf templateLinked

instance Data.ToJSON PolicyDefinition where
  toJSON PolicyDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("static" Data..=) Prelude.<$> static,
            ("templateLinked" Data..=)
              Prelude.<$> templateLinked
          ]
      )
