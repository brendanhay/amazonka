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
-- Module      : Network.AWS.STS.Types.FederatedUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.STS.Types.FederatedUser where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Identifiers for the federated user that is associated with the
-- credentials.
--
-- /See:/ 'newFederatedUser' smart constructor.
data FederatedUser = FederatedUser'
  { -- | The string that identifies the federated user associated with the
    -- credentials, similar to the unique ID of an IAM user.
    federatedUserId :: Core.Text,
    -- | The ARN that specifies the federated user that is associated with the
    -- credentials. For more information about ARNs and how to use them in
    -- policies, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers>
    -- in the /IAM User Guide/.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'FederatedUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'federatedUserId', 'federatedUser_federatedUserId' - The string that identifies the federated user associated with the
-- credentials, similar to the unique ID of an IAM user.
--
-- 'arn', 'federatedUser_arn' - The ARN that specifies the federated user that is associated with the
-- credentials. For more information about ARNs and how to use them in
-- policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers>
-- in the /IAM User Guide/.
newFederatedUser ::
  -- | 'federatedUserId'
  Core.Text ->
  -- | 'arn'
  Core.Text ->
  FederatedUser
newFederatedUser pFederatedUserId_ pArn_ =
  FederatedUser'
    { federatedUserId = pFederatedUserId_,
      arn = pArn_
    }

-- | The string that identifies the federated user associated with the
-- credentials, similar to the unique ID of an IAM user.
federatedUser_federatedUserId :: Lens.Lens' FederatedUser Core.Text
federatedUser_federatedUserId = Lens.lens (\FederatedUser' {federatedUserId} -> federatedUserId) (\s@FederatedUser' {} a -> s {federatedUserId = a} :: FederatedUser)

-- | The ARN that specifies the federated user that is associated with the
-- credentials. For more information about ARNs and how to use them in
-- policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers>
-- in the /IAM User Guide/.
federatedUser_arn :: Lens.Lens' FederatedUser Core.Text
federatedUser_arn = Lens.lens (\FederatedUser' {arn} -> arn) (\s@FederatedUser' {} a -> s {arn = a} :: FederatedUser)

instance Core.FromXML FederatedUser where
  parseXML x =
    FederatedUser'
      Core.<$> (x Core..@ "FederatedUserId")
      Core.<*> (x Core..@ "Arn")

instance Core.Hashable FederatedUser

instance Core.NFData FederatedUser
