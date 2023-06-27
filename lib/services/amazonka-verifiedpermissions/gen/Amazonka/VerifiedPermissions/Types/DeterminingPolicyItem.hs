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
-- Module      : Amazonka.VerifiedPermissions.Types.DeterminingPolicyItem
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.VerifiedPermissions.Types.DeterminingPolicyItem where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about one of the policies that determined an
-- authorization decision.
--
-- This data type is used as an element in a response parameter for the
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_IsAuthorized.html IsAuthorized>
-- and
-- <https://docs.aws.amazon.com/verifiedpermissions/latest/apireference/API_IsAuthorizedWithToken.html IsAuthorizedWithToken>
-- operations.
--
-- Example:
-- @\"determiningPolicies\":[{\"policyId\":\"SPEXAMPLEabcdefg111111\"}]@
--
-- /See:/ 'newDeterminingPolicyItem' smart constructor.
data DeterminingPolicyItem = DeterminingPolicyItem'
  { -- | The Id of a policy that determined to an authorization decision.
    --
    -- Example: @\"policyId\":\"SPEXAMPLEabcdefg111111\"@
    policyId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DeterminingPolicyItem' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'policyId', 'determiningPolicyItem_policyId' - The Id of a policy that determined to an authorization decision.
--
-- Example: @\"policyId\":\"SPEXAMPLEabcdefg111111\"@
newDeterminingPolicyItem ::
  -- | 'policyId'
  Prelude.Text ->
  DeterminingPolicyItem
newDeterminingPolicyItem pPolicyId_ =
  DeterminingPolicyItem' {policyId = pPolicyId_}

-- | The Id of a policy that determined to an authorization decision.
--
-- Example: @\"policyId\":\"SPEXAMPLEabcdefg111111\"@
determiningPolicyItem_policyId :: Lens.Lens' DeterminingPolicyItem Prelude.Text
determiningPolicyItem_policyId = Lens.lens (\DeterminingPolicyItem' {policyId} -> policyId) (\s@DeterminingPolicyItem' {} a -> s {policyId = a} :: DeterminingPolicyItem)

instance Data.FromJSON DeterminingPolicyItem where
  parseJSON =
    Data.withObject
      "DeterminingPolicyItem"
      ( \x ->
          DeterminingPolicyItem'
            Prelude.<$> (x Data..: "policyId")
      )

instance Prelude.Hashable DeterminingPolicyItem where
  hashWithSalt _salt DeterminingPolicyItem' {..} =
    _salt `Prelude.hashWithSalt` policyId

instance Prelude.NFData DeterminingPolicyItem where
  rnf DeterminingPolicyItem' {..} = Prelude.rnf policyId
