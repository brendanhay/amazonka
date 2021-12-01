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
-- Module      : Amazonka.SSM.Types.Activation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.Activation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.Tag

-- | An activation registers one or more on-premises servers or virtual
-- machines (VMs) with Amazon Web Services so that you can configure those
-- servers or VMs using Run Command. A server or VM that has been
-- registered with Amazon Web Services Systems Manager is called a managed
-- instance.
--
-- /See:/ 'newActivation' smart constructor.
data Activation = Activation'
  { -- | Whether or not the activation is expired.
    expired :: Prelude.Maybe Prelude.Bool,
    -- | A name for the managed instance when it is created.
    defaultInstanceName :: Prelude.Maybe Prelude.Text,
    -- | The ID created by Systems Manager when you submitted the activation.
    activationId :: Prelude.Maybe Prelude.Text,
    -- | The date the activation was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of managed instances that can be registered using
    -- this activation.
    registrationLimit :: Prelude.Maybe Prelude.Natural,
    -- | The date when this activation can no longer be used to register managed
    -- instances.
    expirationDate :: Prelude.Maybe Core.POSIX,
    -- | A user defined description of the activation.
    description :: Prelude.Maybe Prelude.Text,
    -- | Tags assigned to the activation.
    tags :: Prelude.Maybe [Tag],
    -- | The number of managed instances already registered with this activation.
    registrationsCount :: Prelude.Maybe Prelude.Natural,
    -- | The Identity and Access Management (IAM) role to assign to the managed
    -- instance.
    iamRole :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Activation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expired', 'activation_expired' - Whether or not the activation is expired.
--
-- 'defaultInstanceName', 'activation_defaultInstanceName' - A name for the managed instance when it is created.
--
-- 'activationId', 'activation_activationId' - The ID created by Systems Manager when you submitted the activation.
--
-- 'createdDate', 'activation_createdDate' - The date the activation was created.
--
-- 'registrationLimit', 'activation_registrationLimit' - The maximum number of managed instances that can be registered using
-- this activation.
--
-- 'expirationDate', 'activation_expirationDate' - The date when this activation can no longer be used to register managed
-- instances.
--
-- 'description', 'activation_description' - A user defined description of the activation.
--
-- 'tags', 'activation_tags' - Tags assigned to the activation.
--
-- 'registrationsCount', 'activation_registrationsCount' - The number of managed instances already registered with this activation.
--
-- 'iamRole', 'activation_iamRole' - The Identity and Access Management (IAM) role to assign to the managed
-- instance.
newActivation ::
  Activation
newActivation =
  Activation'
    { expired = Prelude.Nothing,
      defaultInstanceName = Prelude.Nothing,
      activationId = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      registrationLimit = Prelude.Nothing,
      expirationDate = Prelude.Nothing,
      description = Prelude.Nothing,
      tags = Prelude.Nothing,
      registrationsCount = Prelude.Nothing,
      iamRole = Prelude.Nothing
    }

-- | Whether or not the activation is expired.
activation_expired :: Lens.Lens' Activation (Prelude.Maybe Prelude.Bool)
activation_expired = Lens.lens (\Activation' {expired} -> expired) (\s@Activation' {} a -> s {expired = a} :: Activation)

-- | A name for the managed instance when it is created.
activation_defaultInstanceName :: Lens.Lens' Activation (Prelude.Maybe Prelude.Text)
activation_defaultInstanceName = Lens.lens (\Activation' {defaultInstanceName} -> defaultInstanceName) (\s@Activation' {} a -> s {defaultInstanceName = a} :: Activation)

-- | The ID created by Systems Manager when you submitted the activation.
activation_activationId :: Lens.Lens' Activation (Prelude.Maybe Prelude.Text)
activation_activationId = Lens.lens (\Activation' {activationId} -> activationId) (\s@Activation' {} a -> s {activationId = a} :: Activation)

-- | The date the activation was created.
activation_createdDate :: Lens.Lens' Activation (Prelude.Maybe Prelude.UTCTime)
activation_createdDate = Lens.lens (\Activation' {createdDate} -> createdDate) (\s@Activation' {} a -> s {createdDate = a} :: Activation) Prelude.. Lens.mapping Core._Time

-- | The maximum number of managed instances that can be registered using
-- this activation.
activation_registrationLimit :: Lens.Lens' Activation (Prelude.Maybe Prelude.Natural)
activation_registrationLimit = Lens.lens (\Activation' {registrationLimit} -> registrationLimit) (\s@Activation' {} a -> s {registrationLimit = a} :: Activation)

-- | The date when this activation can no longer be used to register managed
-- instances.
activation_expirationDate :: Lens.Lens' Activation (Prelude.Maybe Prelude.UTCTime)
activation_expirationDate = Lens.lens (\Activation' {expirationDate} -> expirationDate) (\s@Activation' {} a -> s {expirationDate = a} :: Activation) Prelude.. Lens.mapping Core._Time

-- | A user defined description of the activation.
activation_description :: Lens.Lens' Activation (Prelude.Maybe Prelude.Text)
activation_description = Lens.lens (\Activation' {description} -> description) (\s@Activation' {} a -> s {description = a} :: Activation)

-- | Tags assigned to the activation.
activation_tags :: Lens.Lens' Activation (Prelude.Maybe [Tag])
activation_tags = Lens.lens (\Activation' {tags} -> tags) (\s@Activation' {} a -> s {tags = a} :: Activation) Prelude.. Lens.mapping Lens.coerced

-- | The number of managed instances already registered with this activation.
activation_registrationsCount :: Lens.Lens' Activation (Prelude.Maybe Prelude.Natural)
activation_registrationsCount = Lens.lens (\Activation' {registrationsCount} -> registrationsCount) (\s@Activation' {} a -> s {registrationsCount = a} :: Activation)

-- | The Identity and Access Management (IAM) role to assign to the managed
-- instance.
activation_iamRole :: Lens.Lens' Activation (Prelude.Maybe Prelude.Text)
activation_iamRole = Lens.lens (\Activation' {iamRole} -> iamRole) (\s@Activation' {} a -> s {iamRole = a} :: Activation)

instance Core.FromJSON Activation where
  parseJSON =
    Core.withObject
      "Activation"
      ( \x ->
          Activation'
            Prelude.<$> (x Core..:? "Expired")
            Prelude.<*> (x Core..:? "DefaultInstanceName")
            Prelude.<*> (x Core..:? "ActivationId")
            Prelude.<*> (x Core..:? "CreatedDate")
            Prelude.<*> (x Core..:? "RegistrationLimit")
            Prelude.<*> (x Core..:? "ExpirationDate")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "RegistrationsCount")
            Prelude.<*> (x Core..:? "IamRole")
      )

instance Prelude.Hashable Activation where
  hashWithSalt salt' Activation' {..} =
    salt' `Prelude.hashWithSalt` iamRole
      `Prelude.hashWithSalt` registrationsCount
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` expirationDate
      `Prelude.hashWithSalt` registrationLimit
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` activationId
      `Prelude.hashWithSalt` defaultInstanceName
      `Prelude.hashWithSalt` expired

instance Prelude.NFData Activation where
  rnf Activation' {..} =
    Prelude.rnf expired
      `Prelude.seq` Prelude.rnf iamRole
      `Prelude.seq` Prelude.rnf registrationsCount
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf expirationDate
      `Prelude.seq` Prelude.rnf registrationLimit
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf activationId
      `Prelude.seq` Prelude.rnf defaultInstanceName
