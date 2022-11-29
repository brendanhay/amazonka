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
-- Module      : Amazonka.LicenseManagerUserSubscriptions.Types.InstanceUserSummary
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.LicenseManagerUserSubscriptions.Types.InstanceUserSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.LicenseManagerUserSubscriptions.Types.IdentityProvider
import qualified Amazonka.Prelude as Prelude

-- | Describes users of an EC2 instance providing user-based subscriptions.
--
-- /See:/ 'newInstanceUserSummary' smart constructor.
data InstanceUserSummary = InstanceUserSummary'
  { -- | The date a user was associated with an EC2 instance.
    associationDate :: Prelude.Maybe Prelude.Text,
    -- | The domain name of the user.
    domain :: Prelude.Maybe Prelude.Text,
    -- | The date a user was disassociated from an EC2 instance.
    disassociationDate :: Prelude.Maybe Prelude.Text,
    -- | The status message for users of an EC2 instance.
    statusMessage :: Prelude.Maybe Prelude.Text,
    -- | An object that specifies details for the identity provider.
    identityProvider :: IdentityProvider,
    -- | The ID of the EC2 instance, which provides user-based subscriptions.
    instanceId :: Prelude.Text,
    -- | The status of a user associated with an EC2 instance.
    status :: Prelude.Text,
    -- | The user name from the identity provider for the user.
    username :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceUserSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associationDate', 'instanceUserSummary_associationDate' - The date a user was associated with an EC2 instance.
--
-- 'domain', 'instanceUserSummary_domain' - The domain name of the user.
--
-- 'disassociationDate', 'instanceUserSummary_disassociationDate' - The date a user was disassociated from an EC2 instance.
--
-- 'statusMessage', 'instanceUserSummary_statusMessage' - The status message for users of an EC2 instance.
--
-- 'identityProvider', 'instanceUserSummary_identityProvider' - An object that specifies details for the identity provider.
--
-- 'instanceId', 'instanceUserSummary_instanceId' - The ID of the EC2 instance, which provides user-based subscriptions.
--
-- 'status', 'instanceUserSummary_status' - The status of a user associated with an EC2 instance.
--
-- 'username', 'instanceUserSummary_username' - The user name from the identity provider for the user.
newInstanceUserSummary ::
  -- | 'identityProvider'
  IdentityProvider ->
  -- | 'instanceId'
  Prelude.Text ->
  -- | 'status'
  Prelude.Text ->
  -- | 'username'
  Prelude.Text ->
  InstanceUserSummary
newInstanceUserSummary
  pIdentityProvider_
  pInstanceId_
  pStatus_
  pUsername_ =
    InstanceUserSummary'
      { associationDate =
          Prelude.Nothing,
        domain = Prelude.Nothing,
        disassociationDate = Prelude.Nothing,
        statusMessage = Prelude.Nothing,
        identityProvider = pIdentityProvider_,
        instanceId = pInstanceId_,
        status = pStatus_,
        username = pUsername_
      }

-- | The date a user was associated with an EC2 instance.
instanceUserSummary_associationDate :: Lens.Lens' InstanceUserSummary (Prelude.Maybe Prelude.Text)
instanceUserSummary_associationDate = Lens.lens (\InstanceUserSummary' {associationDate} -> associationDate) (\s@InstanceUserSummary' {} a -> s {associationDate = a} :: InstanceUserSummary)

-- | The domain name of the user.
instanceUserSummary_domain :: Lens.Lens' InstanceUserSummary (Prelude.Maybe Prelude.Text)
instanceUserSummary_domain = Lens.lens (\InstanceUserSummary' {domain} -> domain) (\s@InstanceUserSummary' {} a -> s {domain = a} :: InstanceUserSummary)

-- | The date a user was disassociated from an EC2 instance.
instanceUserSummary_disassociationDate :: Lens.Lens' InstanceUserSummary (Prelude.Maybe Prelude.Text)
instanceUserSummary_disassociationDate = Lens.lens (\InstanceUserSummary' {disassociationDate} -> disassociationDate) (\s@InstanceUserSummary' {} a -> s {disassociationDate = a} :: InstanceUserSummary)

-- | The status message for users of an EC2 instance.
instanceUserSummary_statusMessage :: Lens.Lens' InstanceUserSummary (Prelude.Maybe Prelude.Text)
instanceUserSummary_statusMessage = Lens.lens (\InstanceUserSummary' {statusMessage} -> statusMessage) (\s@InstanceUserSummary' {} a -> s {statusMessage = a} :: InstanceUserSummary)

-- | An object that specifies details for the identity provider.
instanceUserSummary_identityProvider :: Lens.Lens' InstanceUserSummary IdentityProvider
instanceUserSummary_identityProvider = Lens.lens (\InstanceUserSummary' {identityProvider} -> identityProvider) (\s@InstanceUserSummary' {} a -> s {identityProvider = a} :: InstanceUserSummary)

-- | The ID of the EC2 instance, which provides user-based subscriptions.
instanceUserSummary_instanceId :: Lens.Lens' InstanceUserSummary Prelude.Text
instanceUserSummary_instanceId = Lens.lens (\InstanceUserSummary' {instanceId} -> instanceId) (\s@InstanceUserSummary' {} a -> s {instanceId = a} :: InstanceUserSummary)

-- | The status of a user associated with an EC2 instance.
instanceUserSummary_status :: Lens.Lens' InstanceUserSummary Prelude.Text
instanceUserSummary_status = Lens.lens (\InstanceUserSummary' {status} -> status) (\s@InstanceUserSummary' {} a -> s {status = a} :: InstanceUserSummary)

-- | The user name from the identity provider for the user.
instanceUserSummary_username :: Lens.Lens' InstanceUserSummary Prelude.Text
instanceUserSummary_username = Lens.lens (\InstanceUserSummary' {username} -> username) (\s@InstanceUserSummary' {} a -> s {username = a} :: InstanceUserSummary)

instance Core.FromJSON InstanceUserSummary where
  parseJSON =
    Core.withObject
      "InstanceUserSummary"
      ( \x ->
          InstanceUserSummary'
            Prelude.<$> (x Core..:? "AssociationDate")
            Prelude.<*> (x Core..:? "Domain")
            Prelude.<*> (x Core..:? "DisassociationDate")
            Prelude.<*> (x Core..:? "StatusMessage")
            Prelude.<*> (x Core..: "IdentityProvider")
            Prelude.<*> (x Core..: "InstanceId")
            Prelude.<*> (x Core..: "Status")
            Prelude.<*> (x Core..: "Username")
      )

instance Prelude.Hashable InstanceUserSummary where
  hashWithSalt _salt InstanceUserSummary' {..} =
    _salt `Prelude.hashWithSalt` associationDate
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` disassociationDate
      `Prelude.hashWithSalt` statusMessage
      `Prelude.hashWithSalt` identityProvider
      `Prelude.hashWithSalt` instanceId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` username

instance Prelude.NFData InstanceUserSummary where
  rnf InstanceUserSummary' {..} =
    Prelude.rnf associationDate
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf disassociationDate
      `Prelude.seq` Prelude.rnf statusMessage
      `Prelude.seq` Prelude.rnf identityProvider
      `Prelude.seq` Prelude.rnf instanceId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf username
