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
-- Module      : Network.AWS.EKS.Types.Provider
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EKS.Types.Provider where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Identifies the AWS Key Management Service (AWS KMS) customer master key
-- (CMK) used to encrypt the secrets.
--
-- /See:/ 'newProvider' smart constructor.
data Provider = Provider'
  { -- | Amazon Resource Name (ARN) or alias of the customer master key (CMK).
    -- The CMK must be symmetric, created in the same region as the cluster,
    -- and if the CMK was created in a different account, the user must have
    -- access to the CMK. For more information, see
    -- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policy-modifying-external-accounts.html Allowing Users in Other Accounts to Use a CMK>
    -- in the /AWS Key Management Service Developer Guide/.
    keyArn :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Provider' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'keyArn', 'provider_keyArn' - Amazon Resource Name (ARN) or alias of the customer master key (CMK).
-- The CMK must be symmetric, created in the same region as the cluster,
-- and if the CMK was created in a different account, the user must have
-- access to the CMK. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policy-modifying-external-accounts.html Allowing Users in Other Accounts to Use a CMK>
-- in the /AWS Key Management Service Developer Guide/.
newProvider ::
  Provider
newProvider = Provider' {keyArn = Prelude.Nothing}

-- | Amazon Resource Name (ARN) or alias of the customer master key (CMK).
-- The CMK must be symmetric, created in the same region as the cluster,
-- and if the CMK was created in a different account, the user must have
-- access to the CMK. For more information, see
-- <https://docs.aws.amazon.com/kms/latest/developerguide/key-policy-modifying-external-accounts.html Allowing Users in Other Accounts to Use a CMK>
-- in the /AWS Key Management Service Developer Guide/.
provider_keyArn :: Lens.Lens' Provider (Prelude.Maybe Prelude.Text)
provider_keyArn = Lens.lens (\Provider' {keyArn} -> keyArn) (\s@Provider' {} a -> s {keyArn = a} :: Provider)

instance Core.FromJSON Provider where
  parseJSON =
    Core.withObject
      "Provider"
      (\x -> Provider' Prelude.<$> (x Core..:? "keyArn"))

instance Prelude.Hashable Provider

instance Prelude.NFData Provider

instance Core.ToJSON Provider where
  toJSON Provider' {..} =
    Core.object
      ( Prelude.catMaybes
          [("keyArn" Core..=) Prelude.<$> keyArn]
      )
