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
-- Module      : Amazonka.STS.Types.FederatedUser
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.STS.Types.FederatedUser where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Identifiers for the federated user that is associated with the
-- credentials.
--
-- /See:/ 'newFederatedUser' smart constructor.
data FederatedUser = FederatedUser'
  { -- | The string that identifies the federated user associated with the
    -- credentials, similar to the unique ID of an IAM user.
    federatedUserId :: Prelude.Text,
    -- | The ARN that specifies the federated user that is associated with the
    -- credentials. For more information about ARNs and how to use them in
    -- policies, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers>
    -- in the /IAM User Guide/.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'arn'
  Prelude.Text ->
  FederatedUser
newFederatedUser pFederatedUserId_ pArn_ =
  FederatedUser'
    { federatedUserId = pFederatedUserId_,
      arn = pArn_
    }

-- | The string that identifies the federated user associated with the
-- credentials, similar to the unique ID of an IAM user.
federatedUser_federatedUserId :: Lens.Lens' FederatedUser Prelude.Text
federatedUser_federatedUserId = Lens.lens (\FederatedUser' {federatedUserId} -> federatedUserId) (\s@FederatedUser' {} a -> s {federatedUserId = a} :: FederatedUser)

-- | The ARN that specifies the federated user that is associated with the
-- credentials. For more information about ARNs and how to use them in
-- policies, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/reference_identifiers.html IAM Identifiers>
-- in the /IAM User Guide/.
federatedUser_arn :: Lens.Lens' FederatedUser Prelude.Text
federatedUser_arn = Lens.lens (\FederatedUser' {arn} -> arn) (\s@FederatedUser' {} a -> s {arn = a} :: FederatedUser)

instance Data.FromXML FederatedUser where
  parseXML x =
    FederatedUser'
      Prelude.<$> (x Data..@ "FederatedUserId")
      Prelude.<*> (x Data..@ "Arn")

instance Prelude.Hashable FederatedUser where
  hashWithSalt _salt FederatedUser' {..} =
    _salt
      `Prelude.hashWithSalt` federatedUserId
      `Prelude.hashWithSalt` arn

instance Prelude.NFData FederatedUser where
  rnf FederatedUser' {..} =
    Prelude.rnf federatedUserId
      `Prelude.seq` Prelude.rnf arn
