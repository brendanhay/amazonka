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
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SSM.Types.Activation where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SSM.Types.Tag

-- | An activation registers one or more on-premises servers or virtual
-- machines (VMs) with Amazon Web Services so that you can configure those
-- servers or VMs using Run Command. A server or VM that has been
-- registered with Amazon Web Services Systems Manager is called a managed
-- node.
--
-- /See:/ 'newActivation' smart constructor.
data Activation = Activation'
  { -- | Tags assigned to the activation.
    tags :: Prelude.Maybe [Tag],
    -- | The Identity and Access Management (IAM) role to assign to the managed
    -- node.
    iamRole :: Prelude.Maybe Prelude.Text,
    -- | A name for the managed node when it is created.
    defaultInstanceName :: Prelude.Maybe Prelude.Text,
    -- | Whether or not the activation is expired.
    expired :: Prelude.Maybe Prelude.Bool,
    -- | A user defined description of the activation.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID created by Systems Manager when you submitted the activation.
    activationId :: Prelude.Maybe Prelude.Text,
    -- | The number of managed nodes already registered with this activation.
    registrationsCount :: Prelude.Maybe Prelude.Natural,
    -- | The date the activation was created.
    createdDate :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of managed nodes that can be registered using this
    -- activation.
    registrationLimit :: Prelude.Maybe Prelude.Natural,
    -- | The date when this activation can no longer be used to register managed
    -- nodes.
    expirationDate :: Prelude.Maybe Core.POSIX
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
-- 'tags', 'activation_tags' - Tags assigned to the activation.
--
-- 'iamRole', 'activation_iamRole' - The Identity and Access Management (IAM) role to assign to the managed
-- node.
--
-- 'defaultInstanceName', 'activation_defaultInstanceName' - A name for the managed node when it is created.
--
-- 'expired', 'activation_expired' - Whether or not the activation is expired.
--
-- 'description', 'activation_description' - A user defined description of the activation.
--
-- 'activationId', 'activation_activationId' - The ID created by Systems Manager when you submitted the activation.
--
-- 'registrationsCount', 'activation_registrationsCount' - The number of managed nodes already registered with this activation.
--
-- 'createdDate', 'activation_createdDate' - The date the activation was created.
--
-- 'registrationLimit', 'activation_registrationLimit' - The maximum number of managed nodes that can be registered using this
-- activation.
--
-- 'expirationDate', 'activation_expirationDate' - The date when this activation can no longer be used to register managed
-- nodes.
newActivation ::
  Activation
newActivation =
  Activation'
    { tags = Prelude.Nothing,
      iamRole = Prelude.Nothing,
      defaultInstanceName = Prelude.Nothing,
      expired = Prelude.Nothing,
      description = Prelude.Nothing,
      activationId = Prelude.Nothing,
      registrationsCount = Prelude.Nothing,
      createdDate = Prelude.Nothing,
      registrationLimit = Prelude.Nothing,
      expirationDate = Prelude.Nothing
    }

-- | Tags assigned to the activation.
activation_tags :: Lens.Lens' Activation (Prelude.Maybe [Tag])
activation_tags = Lens.lens (\Activation' {tags} -> tags) (\s@Activation' {} a -> s {tags = a} :: Activation) Prelude.. Lens.mapping Lens.coerced

-- | The Identity and Access Management (IAM) role to assign to the managed
-- node.
activation_iamRole :: Lens.Lens' Activation (Prelude.Maybe Prelude.Text)
activation_iamRole = Lens.lens (\Activation' {iamRole} -> iamRole) (\s@Activation' {} a -> s {iamRole = a} :: Activation)

-- | A name for the managed node when it is created.
activation_defaultInstanceName :: Lens.Lens' Activation (Prelude.Maybe Prelude.Text)
activation_defaultInstanceName = Lens.lens (\Activation' {defaultInstanceName} -> defaultInstanceName) (\s@Activation' {} a -> s {defaultInstanceName = a} :: Activation)

-- | Whether or not the activation is expired.
activation_expired :: Lens.Lens' Activation (Prelude.Maybe Prelude.Bool)
activation_expired = Lens.lens (\Activation' {expired} -> expired) (\s@Activation' {} a -> s {expired = a} :: Activation)

-- | A user defined description of the activation.
activation_description :: Lens.Lens' Activation (Prelude.Maybe Prelude.Text)
activation_description = Lens.lens (\Activation' {description} -> description) (\s@Activation' {} a -> s {description = a} :: Activation)

-- | The ID created by Systems Manager when you submitted the activation.
activation_activationId :: Lens.Lens' Activation (Prelude.Maybe Prelude.Text)
activation_activationId = Lens.lens (\Activation' {activationId} -> activationId) (\s@Activation' {} a -> s {activationId = a} :: Activation)

-- | The number of managed nodes already registered with this activation.
activation_registrationsCount :: Lens.Lens' Activation (Prelude.Maybe Prelude.Natural)
activation_registrationsCount = Lens.lens (\Activation' {registrationsCount} -> registrationsCount) (\s@Activation' {} a -> s {registrationsCount = a} :: Activation)

-- | The date the activation was created.
activation_createdDate :: Lens.Lens' Activation (Prelude.Maybe Prelude.UTCTime)
activation_createdDate = Lens.lens (\Activation' {createdDate} -> createdDate) (\s@Activation' {} a -> s {createdDate = a} :: Activation) Prelude.. Lens.mapping Core._Time

-- | The maximum number of managed nodes that can be registered using this
-- activation.
activation_registrationLimit :: Lens.Lens' Activation (Prelude.Maybe Prelude.Natural)
activation_registrationLimit = Lens.lens (\Activation' {registrationLimit} -> registrationLimit) (\s@Activation' {} a -> s {registrationLimit = a} :: Activation)

-- | The date when this activation can no longer be used to register managed
-- nodes.
activation_expirationDate :: Lens.Lens' Activation (Prelude.Maybe Prelude.UTCTime)
activation_expirationDate = Lens.lens (\Activation' {expirationDate} -> expirationDate) (\s@Activation' {} a -> s {expirationDate = a} :: Activation) Prelude.. Lens.mapping Core._Time

instance Core.FromJSON Activation where
  parseJSON =
    Core.withObject
      "Activation"
      ( \x ->
          Activation'
            Prelude.<$> (x Core..:? "Tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "IamRole")
            Prelude.<*> (x Core..:? "DefaultInstanceName")
            Prelude.<*> (x Core..:? "Expired")
            Prelude.<*> (x Core..:? "Description")
            Prelude.<*> (x Core..:? "ActivationId")
            Prelude.<*> (x Core..:? "RegistrationsCount")
            Prelude.<*> (x Core..:? "CreatedDate")
            Prelude.<*> (x Core..:? "RegistrationLimit")
            Prelude.<*> (x Core..:? "ExpirationDate")
      )

instance Prelude.Hashable Activation where
  hashWithSalt _salt Activation' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` iamRole
      `Prelude.hashWithSalt` defaultInstanceName
      `Prelude.hashWithSalt` expired
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` activationId
      `Prelude.hashWithSalt` registrationsCount
      `Prelude.hashWithSalt` createdDate
      `Prelude.hashWithSalt` registrationLimit
      `Prelude.hashWithSalt` expirationDate

instance Prelude.NFData Activation where
  rnf Activation' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf iamRole
      `Prelude.seq` Prelude.rnf defaultInstanceName
      `Prelude.seq` Prelude.rnf expired
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf activationId
      `Prelude.seq` Prelude.rnf registrationsCount
      `Prelude.seq` Prelude.rnf createdDate
      `Prelude.seq` Prelude.rnf registrationLimit
      `Prelude.seq` Prelude.rnf expirationDate
