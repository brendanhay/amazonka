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
-- Module      : Amazonka.VerifiedPermissions.Types.UpdatePolicyDefinition
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.UpdatePolicyDefinition where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.VerifiedPermissions.Types.UpdateStaticPolicyDefinition

-- | Contains information about updates to be applied to a policy.
--
-- This data type is used as a request parameter in the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_UpdatePolicy.html UpdatePolicy>
-- operation.
--
-- /See:/ 'newUpdatePolicyDefinition' smart constructor.
data UpdatePolicyDefinition = UpdatePolicyDefinition'
  { -- | Contains details about the updates to be applied to a static policy.
    static :: Prelude.Maybe UpdateStaticPolicyDefinition
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdatePolicyDefinition' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'static', 'updatePolicyDefinition_static' - Contains details about the updates to be applied to a static policy.
newUpdatePolicyDefinition ::
  UpdatePolicyDefinition
newUpdatePolicyDefinition =
  UpdatePolicyDefinition' {static = Prelude.Nothing}

-- | Contains details about the updates to be applied to a static policy.
updatePolicyDefinition_static :: Lens.Lens' UpdatePolicyDefinition (Prelude.Maybe UpdateStaticPolicyDefinition)
updatePolicyDefinition_static = Lens.lens (\UpdatePolicyDefinition' {static} -> static) (\s@UpdatePolicyDefinition' {} a -> s {static = a} :: UpdatePolicyDefinition)

instance Prelude.Hashable UpdatePolicyDefinition where
  hashWithSalt _salt UpdatePolicyDefinition' {..} =
    _salt `Prelude.hashWithSalt` static

instance Prelude.NFData UpdatePolicyDefinition where
  rnf UpdatePolicyDefinition' {..} = Prelude.rnf static

instance Data.ToJSON UpdatePolicyDefinition where
  toJSON UpdatePolicyDefinition' {..} =
    Data.object
      ( Prelude.catMaybes
          [("static" Data..=) Prelude.<$> static]
      )
